{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Scion.Inspect.ModuleLoader
-- Copyright   : (c) B. Scott Michel, 2011
-- License     : BSD-style
--
-- Maintainer  : bscottm@ieee.org
-- Stability   : experimental
-- Portability : portable
--
-- Collect symbol names from modules to support IDE completion.
--
-- Note: The inspiration for this source code comes from the Leksah IDE's server

module Scion.Inspect.IFaceLoader
  ( updateModulesCache
  , getModulesForTypecheck
  ) where

import Scion.Types
import Scion.Inspect
import Scion.Utils

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.List as List
import qualified Data.Foldable as Fold

-- GHC's imports
import GHC
import GHC.Exception()
import HscTypes
import Module
import Outputable
import Finder
import qualified Maybes
import LoadIface
import TcRnTypes
import TcRnMonad
import OccName
import IfaceSyn
import Fingerprint

-- System imports
import System.Directory
import System.Time

import Debug.Trace

-- | Synonym for a set of OccNames
type OccNameSet = Set.Set OccName

-- | Update the cached modules
updateModulesCache :: Maybe BgTcCache
                   -> ScionM ()
updateModulesCache tychk =
  getModulesForTypecheck tychk
  >>= (\(_, depMods) -> gets moduleCache
                        >>= (\mc -> updateModules depMods mc
                                    >>= (\newModCache -> modifySessionState $ updateSessionMCache newModCache)))
  >> return ()

updateModules :: [Module]
              -> ModuleCache
              -> ScionM ModuleCache

updateModules [] mCache = return mCache
updateModules (m:mods) mCache
  | ignorableMod m 
  = modDebugMsg m "Ignoring "
    >> updateModules mods mCache
  | Nothing <- Map.lookup m mCache
  = modDebugMsg m "Adding   "
    >> cacheModule m mCache
    >>= (\updCache -> updateModules mods updCache)
  | otherwise
  = case Map.lookup m mCache of
      (Just mData) ->
        ifM (moduleChanged m (lastModTime mData))
          (modDebugMsg m "Updating "
            >> cacheModule m mCache
            >>= (\updCache -> updateModules mods updCache))
          (modDebugMsg m "NoMod    " >> updateModules mods mCache)
      Nothing      ->
          modDebugMsg m "NoMod??! " >> updateModules mods mCache
      
-- A module is ignorable if it's part of the main package or it's unknown
ignorableMod :: Module
             -> Bool
ignorableMod m = 
  let mpkg = modulePackageId m
  in  mpkg == mainPackageId || mpkg == unknownPackageId
  
-- Predicate for detecting if the module's time/date stamp has changed
moduleChanged :: Module
              -> IO ClockTime
              -> ScionM Bool

moduleChanged m modTime = getSession >>= (\hsc -> liftIO (findExactModule hsc m >>= checkMTimes))
  where
    -- May return True or False
    checkMTimes (Found loc _) =
      modTime
      >>= (\mcMTime -> getModificationTime (ml_hi_file loc)
                       >>= (\hiMTime -> return (diffClockTimes mcMTime hiMTime /= noTimeDiff)))
    -- Ensure that we leave the interface file alone if it cannot be found.
    checkMTimes _ = return False

-- Install the new module cache in the SessionState record:
updateSessionMCache :: ModuleCache
                    -> SessionState
                    -> SessionState
updateSessionMCache newModCache session = session { moduleCache = newModCache } 

-- | Trace actions related to whether we load/ignore/update a Haskell interface
modDebugMsg :: Module
            -> String
            -> ScionM ()
modDebugMsg m msg = liftIO $ logInfo (msg ++ ((moduleNameString . moduleName) m))

-- | General debug messages
debugMsg :: String -> ScionM ()
debugMsg msg = liftIO $ logInfo msg

-- | Find and load the Haskell interface file, extracting its exports and correlating them
-- with the declarations. Note that the interface's export list only tells us the names of
-- things that are exported; we subsequently have to look at the mi_decls list to extract
-- specifics (Is something a type name or class? Does a constructor have arguments?)
cacheModule :: Module
            -> ModuleCache
            -> ScionM ModuleCache
cacheModule m cache =
  getInterfaceFile m
  >>= (\maybeIface ->
        case maybeIface of
          Just (iface, fpath) ->
            {- Show the exported symbol set 
            debugExportSet (exportSet iface)
            >> -}
            (debugModSymData (exportSet iface) (collectedDecls iface))
            >> (return $ updatedCache iface fpath)
          Nothing             ->
            modDebugMsg m "Could not load " >> return cache
      )
      
  where
    exportSet :: ModIface -> OccNameSet
    exportSet iface = extractIfaceExports iface
    collectedDecls :: ModIface -> ModSymData
    collectedDecls iface = collectExportDecls (exportSet iface) (mi_decls iface)
    updatedCache iface fpath = Map.insert m (mkModCacheData fpath (collectedDecls iface)) cache

-- Debugging stuff
debugExportSet :: OccNameSet -> ScionM ()
debugExportSet eSet = liftIO $ putStrLn ((List.foldl' showExport "exports [ " (Set.toAscList eSet)) ++ "]")
  where
    showExport str eName = str ++ (occNameString eName) ++ " "

debugModSymData :: OccNameSet -> ModSymData -> ScionM ()
debugModSymData eSet msyms = liftIO $ putStrLn $ matchLengths ++ "\n" ++ modSymDump
  where
    matchLengths
      | Set.size eSet == Map.size msyms
      = "-- Everything extracted --"
      | otherwise
      = let missing = Set.difference (Set.fromList (map occNameString (Set.toList eSet)))
                                     (Set.fromList $ Map.keys msyms)
        in  (show $ Set.size eSet) ++ " <> " ++ (show $ Map.size msyms) ++ "\ndifference is " ++ (show missing) ++ "\n"
    modSymDump = (Fold.foldl' (showModSymData) "" (Map.toList msyms))
    showModSymData s (name, decls) = s ++ name ++ " -> [ " ++ (Fold.foldl showModDecls "" decls) ++ "]\n"
    showModDecls s d = s ++ (show d) ++ " "   

extractIfaceExports :: ModIface -> OccNameSet
extractIfaceExports iface = Fold.foldl' insertExp Set.empty [i | (_, i) <- mi_exports iface]
  where
    insertExp eSet names = Fold.foldl' insertExp' eSet names
    insertExp' eSet (Avail name) = Set.insert name eSet
    insertExp' eSet (AvailTC name mbrs) = Set.union (Set.insert name eSet) (Set.fromList mbrs)

-- | Collect export declarations, filtered by the exported name set.
collectExportDecls :: OccNameSet                  -- ^ The exported name set
                   -> [(Fingerprint, IfaceDecl)]  -- ^ The interface file's declarations
                   -> ModSymData                  -- ^ The collected association between name strings and declaration data

collectExportDecls eSet decls = Fold.foldl' processDecl Map.empty [ d | (_, d) <- decls ]
  where
    -- Regular old function or top level identifier
    processDecl msymMap (IfaceId { ifName = name }) =
      filterDecl eSet msymMap name MIdDecl
    -- A 'data' declaration: insert it first, followed by its data type constructors
    processDecl msymMap sym@(IfaceData { ifName = name }) =
      let dDecl = filterDecl eSet msymMap name (MTypeDecl sym)
      in  addDataCons eSet dDecl (ifCons sym)
    -- A 'newtype' (synonym) declaration
    processDecl msymMap sym@(IfaceSyn { ifName = name }) =
      filterDecl eSet msymMap name (MTypeDecl sym)
    -- A 'class' declaration: insert the class name first, followed by its functions
    processDecl msymMap sym@(IfaceClass { ifName = name }) =
      let cDecl = filterDecl eSet msymMap name (MClassDecl sym)
      in  Fold.foldl' (filterSig eSet) cDecl (ifSigs sym) 
    processDecl msymMap (IfaceForeign _ _) = msymMap

-- | Process
filterDecl :: OccNameSet -> ModSymData -> OccName -> ModDecl -> ModSymData
filterDecl eSet msymMap name sym =
  let nameStr  = occNameString name
      symSeq = case Map.lookup nameStr msymMap of
                (Just msyms) -> msyms Seq.|> sym
                Nothing      -> Seq.singleton sym
  in  if Set.member name eSet
        then Map.insert nameStr symSeq msymMap
        else msymMap

addDataCons :: OccNameSet -> ModSymData -> IfaceConDecls -> ModSymData
addDataCons _ msymMap IfAbstractTyCon = trace "IfAbstractTyCon" msymMap
addDataCons _ msymMap IfOpenDataTyCon = trace "IfOpenDataTyCon" msymMap
addDataCons eSet msymMap (IfDataTyCon conDecls) = Fold.foldl' (filterCon eSet) msymMap conDecls
addDataCons eSet msymMap (IfNewTyCon newTyDecl) = filterCon eSet msymMap newTyDecl

filterCon :: OccNameSet -> ModSymData -> IfaceConDecl -> ModSymData
filterCon eSet msymMap c@(IfCon { ifConOcc = name }) =
  let nameStr = occNameString name
      conSym  = MConDecl c
      symSeq  = case Map.lookup nameStr msymMap of
                  (Just msyms) -> msyms Seq.|> conSym
                  Nothing      -> Seq.singleton conSym
  in  if Set.member name eSet
        then Map.insert nameStr symSeq msymMap
        else msymMap

filterSig :: OccNameSet -> ModSymData -> IfaceClassOp -> ModSymData
filterSig eSet msymMap op@(IfaceClassOp name _ _) =
  let nameStr = occNameString name
      sigSym  = MClassOp op 
      symSeq  = case Map.lookup nameStr msymMap of
                  (Just msyms) -> msyms Seq.|> sigSym
                  Nothing      -> Seq.singleton sigSym
  in  if Set.member name eSet
        then Map.insert nameStr symSeq msymMap
        else msymMap

-- | General debug messages
debugMsg :: String -> ScionM ()
debugMsg msg = liftIO $ logInfo msg

-- | Find and load the Haskell interface file, extracting its exports and correlating them
-- with the declarations. Note that the interface's export list only tells us the names of
-- things that are exported; we subsequently have to look at the mi_decls list to extract
-- specifics (Is something a type name or class? Does a constructor have arguments?)
cacheModule :: Module
            -> ModuleCache
            -> ScionM ModuleCache
cacheModule m cache =
  getInterfaceFile m
  >>= (\maybeIface ->
        case maybeIface of
          Just (iface, fpath) ->
            debugExportSet iface
            >> (return $ Map.insert m (mkModCacheData fpath (collectedDecls iface)) cache)
          Nothing             ->
            modDebugMsg m "Could not load " >> return cache
      )
      
  where
    exportSet :: ModIface -> OccNameSet
    exportSet iface = extractIfaceExports iface
    collectedDecls :: ModIface -> ModSymData
    collectedDecls iface = collectExportDecls (exportSet iface) (mi_decls iface)
    
    -- Debugging stuff
    debugExportSet iface = mapM_ showExport (Set.toAscList $ exportSet iface)
    showExport eName = liftIO $ putStrLn (occNameString eName)

extractIfaceExports :: ModIface -> OccNameSet
extractIfaceExports iface = collect Set.empty (mi_exports iface)
  where
    -- Collect the exported names as a set, should make filtering the
    -- decls easier. Could use Set.unions across all collected sets,
    -- but makes better sense to pass the accumulated set in a tail-recursive
    -- function.
    collect eSet [] = eSet
    collect eSet ((_, info):exps) = collect (insertExp eSet info) exps
    -- Same note about accumulation on a tail-recursive function
    insertExp eSet [] = eSet
    insertExp eSet (n:names) = insertExp (insertExp' eSet n) names
    -- insertExp eSet (_, names) = Set.unions $ map (insertExp' eSet) names
    -- insertExp' :: OccNameSet -> (GenAvailInfo OccName) -> OccNameSet
    insertExp' eSet (Avail name) = Set.insert name eSet
    insertExp' eSet (AvailTC name mbrs) = Set.union (Set.insert name eSet) (Set.fromList mbrs)
    
collectExportDecls :: OccNameSet
                   -> [(Fingerprint, IfaceDecl)]
                   -> ModSymData

collectExportDecls eSet decls = Map.empty
    
-- | Get the list of modules associated with the type-checked source
getModulesForTypecheck :: Maybe BgTcCache           -- ^ The type-checked source
                       -> ScionM (Module, [Module]) -- ^ The list of modules

getModulesForTypecheck (Just (Typechecked tcm)) = generateModules (tm_parsed_module tcm)
getModulesForTypecheck (Just (Parsed pm))       = generateModules pm
getModulesForTypecheck Nothing = undefined

-- | Fabricate a module name that can be easily detected as bogus. The main source
-- of these "unknown" modules is the exception raised by 'modLookup' (below) when
-- GHC can't figure out to whom the module belongs. Consequently, these modules are
-- not candidates from which names are extracted.
unknownModule :: ModuleName
              -> Module
unknownModule = mkModule unknownPackageId

-- | Package identifier for unknown/unloaded modules
unknownPackageId :: PackageId
unknownPackageId = stringToPackageId "*unknown*"

-- | Extract the modules referenced by the current parsed module, returning
-- the primary module's data and a list of the dependent modules
generateModules :: ParsedModule              -- ^ The current module
                -> ScionM (Module, [Module]) -- ^ The primary module, dependent modules list
generateModules modSummary
  = getInnerModules >>= (\innerMods -> return (thisMod, innerMods))
  where
    thisModSum      = pm_mod_summary modSummary
    thisMod         = ms_mod thisModSum
    innerImports    = map unLoc $ ms_imps thisModSum
    innerModNames   = map (unLoc . ideclName) innerImports
    getInnerModules = mapM (\m -> modLookup m) innerModNames
    -- Catch the GHC source error exception when a module doesn't appear to be loaded
    modLookup m     = gcatch (lookupModule m Nothing)
                             (\(_ :: SourceError) -> return $ unknownModule m)

-- | Get the type names for the current source in the background typecheck cache,
-- both local and imported from modules.
extractMainPackageModuleSyms :: Maybe BgTcCache -> [(String,String)]
extractMainPackageModuleSyms Nothing = []
extractMainPackageModuleSyms tychk = localTypes tychk
  where
    -- Types local to the current source
    localTypes (Just (Typechecked tcm)) = map ((formatInfo (getTcmModuleName tcm)) . unLoc) $ typeDecls tcm
    localTypes (Just (Parsed pm))       = map (formatInfo (getModuleName pm)) $ typeDeclsParsed pm
    localTypes Nothing                  = error "Bad pattern match in cmdTypeNames/localTypes"
    -- Output format is a tuple ("type","module")
    formatInfo modname ty = (formatTyDecl ty, modname)
    -- The stuff you have to go through just to get the module's name... :-)
    getTcmModuleName tcm = (getModuleName . tm_parsed_module) tcm
    getModuleName pm     = (moduleNameString . moduleName . ms_mod . pm_mod_summary) pm
    -- Format a type declaration
    formatTyDecl :: (Outputable t) => TyClDecl t -> String
    formatTyDecl (TyFamily { tcdLName = name })  = formatTyName name
    formatTyDecl (TyData { tcdLName = name })    = formatTyName name
    formatTyDecl (TySynonym { tcdLName = name }) = formatTyName name
    -- Theoretically, this is never matched
    formatTyDecl _ = error "Bad filtering in cmdTypeNames"
    -- Type name formattter
    formatTyName :: (Outputable e) => Located e -> String
    formatTyName = (showSDocUnqual . ppr . unLoc)

-- | Load an interface file
getInterfaceFile :: Module
                 -> ScionM (Maybe (ModIface, FilePath))
getInterfaceFile m =
    let iface              =   findAndReadIface empty m False
        gblEnv             =   IfGblEnv { if_rec_types = Nothing }
        ifaceLoader hscEnv = liftIO $ initTcRnIf  'a' hscEnv gblEnv () iface
    in getSession >>= ifaceLoader >>= (\result ->
                                        case result of
                                          Maybes.Succeeded mIface ->    return (Just mIface)
                                          _                       ->    return Nothing)
