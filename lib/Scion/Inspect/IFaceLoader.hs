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
  ( updateMCacheFromTypecheck
  , unknownPackageId
  , unknownModule
  ) where

import Scion.Types
import Scion.Utils

import qualified Data.Map as Map
import qualified Data.Set as Set
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
import RdrName
import LazyUniqFM

-- System imports
import System.Directory
import System.Time

-- | Synonym for a set of OccNames
type OccNameSet = Set.Set OccName
-- | A Module set that tracks modules already read and avoid infinite interface read cycles
type ModulesRead = Set.Set Module
-- | Modules that are hidden or had IO errors.
type ModErrorSet = Set.Set ModuleName
-- | State we drag along while we're reading interfaces
data ModStateT =
  ModStateT {
      modsRead   :: ModulesRead
    , exportSyms :: OccNameSet
    , modSyms    :: ModSymData
    , hiddenMods :: ModErrorSet
    , otherMods  :: ModErrorSet
    }

-- | Dependent module information type
type DepModuleInfo = (Module, [Module])

-- | Get the list of modules associated with the type-checked source, updating the module cache
-- as needed.
updateMCacheFromTypecheck :: ParsedModule          -- ^ The parsed module
                           -> ScionM ()
updateMCacheFromTypecheck pm = generateDepModuleInfo pm >>= updateModuleCache >> return ()

-- | Update the module cache
updateModuleCache :: ([ImportDecl RdrName], DepModuleInfo)
                  -> ScionM ()
updateModuleCache (impDecls, (topMod, depMods)) =
      getSessionSelector moduleCache
      >>= updateModules depMods
      >>= updateImpDecls topMod impDecls
      >>= updateSessionCache
      >> return ()

-- | Update a module's import declarations list. Note that this in only valid for
-- home modules and we rely on scion-server to have loaded that particular file
-- when we query the module cache.
updateImpDecls :: Module
               -> [ImportDecl RdrName]
               -> ModuleCache
               -> ScionM ModuleCache
updateImpDecls m impDecls mCache =
  let struct = case Map.lookup m mCache of
                (Just mdata) -> mdata
                Nothing      -> emptyModCacheData
  in  return $ Map.insert m (struct { importDecls = impDecls }) mCache
    
-- | Update the scion-server session's module cache.
updateSessionCache :: ModuleCache
                   -> ScionM ()
updateSessionCache newModCache =
  modifySessionState $ (\session -> session { moduleCache = newModCache })

-- | Extract the modules referenced by the current parsed module, returning
-- the primary module's data and a list of the dependent modules
generateDepModuleInfo :: ParsedModule              -- ^ The current module
                      -> ScionM ([ImportDecl RdrName], DepModuleInfo)
                         -- ^ Primary module, dependent modules list
generateDepModuleInfo pm = getInnerModules >>= (\innerMods -> return (impDecls, (thisMod, innerMods)))
  where
    thisModSum      = pm_mod_summary pm
    thisMod         = ms_mod thisModSum
    impDecls        = map unLoc $ ms_imps thisModSum
    innerModNames   = map (unLoc . ideclName) impDecls
    getInnerModules = mapM modLookup innerModNames
    -- Catch the GHC source error exception when a module doesn't appear to be loaded
    modLookup mName = gcatch (lookupModule mName Nothing >>= (\m -> return m ))
                             (\(_ :: SourceError) -> return (unknownModule mName))

-- | Examine the incoming module list, read interface files if needed, return the updated module cache
updateModules :: [Module]
              -> ModuleCache
              -> ScionM ModuleCache

updateModules [] mCache = return mCache
updateModules (m:mods) mCache
  | unknownPackageId == (modulePackageId m) 
  = modDebugMsg m "Ignoring "
    >> updateModules mods mCache
  | mainPackageId == (modulePackageId m)
  = modDebugMsg m "Adding (main) "
    >> cacheHomePackageModule m mCache
    >>= updateModules mods
  | Nothing <- Map.lookup m mCache
  = modDebugMsg m "Adding   "
    >> cacheIFaceModule m mCache
    >>= updateModules mods
  | otherwise
  = case Map.lookup m mCache of
      (Just mData) ->
        ifM (moduleChanged m (lastModTime mData))
          (modDebugMsg m "Updating "
            >> cacheIFaceModule m mCache
            >>= updateModules mods)
          (modDebugMsg m "NoMod    " >> updateModules mods mCache)
      Nothing      ->
          modDebugMsg m "NoMod??! " >> updateModules mods mCache

-- | Package identifier for unknown/unloaded modules
unknownPackageId :: PackageId
unknownPackageId = stringToPackageId "*unknown*"
  
-- Predicate for detecting if the module's time/date stamp has changed
moduleChanged :: Module
              -> IO ClockTime
              -> ScionM Bool

moduleChanged m modTime = getSession >>= compareMTimes
  where
    compareMTimes hsc = liftIO (findExactModule hsc m >>= checkMTimes)
    -- May return True or False
    checkMTimes (Found loc _) =
      modTime
      >>= (\mcMTime -> getModificationTime (ml_hi_file loc)
                       >>= (\hiMTime -> return (diffClockTimes mcMTime hiMTime /= noTimeDiff)))
    -- Ensure that we leave the interface file alone if it cannot be found.
    checkMTimes _ = return False

-- | Trace actions related to whether we load/ignore/update a Haskell interface
modDebugMsg :: Module
            -> String
            -> ScionM ()
modDebugMsg m msg = message Verbose (msg ++ ((moduleNameString . moduleName) m))

-- | Find and load the Haskell interface file, extracting its exports and correlating them
-- with the declarations. Note that the interface's export list only tells us the names of
-- things that are exported; we subsequently have to look at the mi_decls list to extract
-- specifics (Is something a type name or class? Does a constructor have arguments?)
cacheIFaceModule :: Module
                 -> ModuleCache
                 -> ScionM ModuleCache
cacheIFaceModule m cache = getInterfaceFile m >>= readIFace
  where
    readIFace :: Maybe (ModIface, FilePath) -> ScionM ModuleCache
    readIFace (Just (iface, fpath)) =
      let eSet = exportSet iface
          initialMState = ModStateT {
                              modsRead   =  Set.singleton m
                            , exportSyms =  eSet
                            , modSyms    =  case Map.lookup m cache of
                                              (Just msyms) -> (modSymData msyms)
                                              Nothing      -> Map.empty
                            , hiddenMods =  Set.empty
                            , otherMods  =  Set.empty
                            }
      in  collectInterface initialMState iface
          >>= (\mstate ->
                let updMSyms = modSyms mstate
                in  (debugModSymData (exportSyms mstate) updMSyms)
                    >> (reportProblems m mstate)
                    >> (return $ Map.insert m (mkModCacheData fpath updMSyms) cache))
    
    readIFace Nothing = modDebugMsg m "Could not load " >> return cache

-- | Extract the set of occurrance names exported through the module interface. This is a
-- straightforward list-to-set transformation
exportSet :: ModIface -> OccNameSet
exportSet iface = Fold.foldl' insertExp Set.empty [i | (_, i) <- mi_exports iface]
  where
    insertExp eSet names = Fold.foldl' insertExp' eSet names
    insertExp' eSet (Avail name) = Set.insert name eSet
    insertExp' eSet (AvailTC name mbrs) = Set.union (Set.insert name eSet) (Set.fromList mbrs)

reportProblems :: Module -> ModStateT -> ScionM ()
reportProblems m mstate =
  if haveProblems
      then (liftIO $ logWarn $ (moduleNameString (moduleName m)) ++ " module cache:")
           {- >> listProblems "-- Hidden modules: " (modNameList (hiddenMods mstate)) -}
           >> listProblems "-- Unreadable modules: " (modNameList (otherMods mstate))
           >> listProblems "-- Symbols not cached: " (occNameList (exportSyms mstate))
      else return ()
  where
    -- The haveProblems predicate is here to make commenting/uncommenting stuff easier.
    haveProblems = not ({-(Set.null (hiddenMods mstate)) &&-} (Set.null (otherMods mstate)) && (Set.null (exportSyms mstate)))
    listProblems title (mn:mns) = liftIO $ logWarn $ title ++ (Fold.foldl' (\acc s -> acc ++ ", " ++ s) mn mns)
    listProblems _     []       = return ()
    modNameList modnames = [ moduleNameString mn | mn <- Set.toList modnames ]
    occNameList occNames = [ occNameString o | o <- Set.toList occNames ]    

-- | Cache names from a home package module, i.e., something that's not an external package and
-- is likely to be part of the "main" package
cacheHomePackageModule :: Module
                       -> ModuleCache
                       -> ScionM ModuleCache
cacheHomePackageModule m cache = withSession readHomePackageModule
  where
    readHomePackageModule hsc =
      case lookupUFM (hsc_HPT hsc) (moduleName m) of
        (Just hmi) ->
          let iface = hm_iface hmi
              eSet = exportSet iface
              initialMState = ModStateT {
                                  modsRead   =  Set.singleton m
                                , exportSyms =  eSet
                                , modSyms    =  case Map.lookup m cache of
                                                  (Just msyms) -> modSymData msyms
                                                  Nothing      -> Map.empty
                                , hiddenMods =  Set.empty
                                , otherMods  =  Set.empty
                                }
          in  collectInterface initialMState iface
              >>= (\mstate ->
                    let updMSyms = modSyms mstate
                    in  (debugModSymData (exportSyms mstate) updMSyms)
                        >> (reportProblems m mstate)
                        >> (return $ Map.insert m (mkModCacheData "" updMSyms) cache))
        Nothing    -> return cache

-- | Collect declarations from a Haskell interface's mi_usages module usage list. 
collectUsageDecls :: ModStateT -> Usage -> ScionM ModStateT
collectUsageDecls mstate (UsagePackageModule usedMod _)  =
  let eSet = exportSyms mstate
      mods = modsRead mstate
  in  if (Set.notMember usedMod mods) && not (Set.null eSet)
        then getInterfaceFile usedMod
             >>= (\ifaceFile -> case ifaceFile of
                                  (Just (iface, _)) -> collectInterface mstate iface
                                  Nothing           -> return mstate)
        else return mstate
    
collectUsageDecls mstate (UsageHomeModule usedMod _ _ _) =
  let mods = modsRead mstate
      addHiddenMod theMod
        | not (Set.member theMod (hiddenMods mstate))
        = return mstate {
                  hiddenMods = Set.insert theMod (hiddenMods mstate)
                }
        | otherwise
        = return mstate
      processModule m = 
        if Set.notMember m mods 
          then getInterfaceFile m
               >>= readModule m
          else return mstate
      -- Read an actual interface
      readModule m (Just (iface, _)) = collectInterface (updMState m) iface
      -- Need to try harder: This could be a home module
      readModule m Nothing           = 
        withSession (\hsc ->
                      case lookupUFM (hsc_HPT hsc) (moduleName m) of
                        Just homeModInfo -> collectInterface (updMState m) (hm_iface homeModInfo)
                        Nothing          -> return (updMState m)
                    )
      updMState m = mstate { modsRead = Set.insert m mods }
  in  if not (Set.null (exportSyms mstate))
        then gcatch
              (lookupModule usedMod Nothing >>= processModule)
              -- We can get a SourceError if GHC can't find the module
              (\(_ :: SourceError) -> addHiddenMod usedMod)
        else return mstate

-- | The basic Haskell interface collector driver.
collectInterface :: ModStateT -> ModIface -> ScionM ModStateT
collectInterface mstate iface =
  let updMState = collectExportDecls mstate (mi_decls iface)
  in  Fold.foldlM collectUsageDecls updMState (mi_usages iface)

debugModSymData :: OccNameSet -> ModSymData -> ScionM ()
debugModSymData eSet msyms = message Verbose $ matchLengths ++ "\n" ++ modSymDump
  where
    missing = Set.difference (Set.fromList (map occNameString (Set.toList eSet)))
                             (Set.fromList (map (showSDoc . ppr) (Map.keys msyms)))
    exportedSize = Set.size eSet
    msymSize = Map.size msyms
    matchLengths
      | Set.null missing
      = "-- Everything extracted --"
      | otherwise
      = (show exportedSize) ++ " <> " ++ (show $ msymSize) ++ "\ndifference is " ++ (show missing) ++ "\n"
    modSymDump = (Fold.foldl' (showModSymData) "" (Map.toList msyms))
    showModSymData s (name, decls) = s ++ ((showSDoc . ppr) name) ++ " -> [ " ++ (Fold.foldl showModDecls "" decls) ++ "]\n"
    showModDecls s d = s ++ (show d) ++ " "   

-- | Collect export declarations, filtered by the exported name set.
collectExportDecls :: ModStateT    -- ^ The exported name set
                   -> [(Fingerprint, IfaceDecl)]  -- ^ The interface file's declarations
                   -> ModStateT    -- ^ The collected association between name strings and declaration data

collectExportDecls inOccMSyms decls = Fold.foldl' processDecl inOccMSyms [ d | (_, d) <- decls ]
  where
    processDecl :: ModStateT -> IfaceDecl -> ModStateT
    -- Regular old function or top level identifier
    processDecl occMSymTuple (IfaceId { ifName = name }) =
      filterDecl occMSymTuple name MIdDecl
    -- A 'data' declaration: insert it first, followed by its data type constructors
    processDecl occMSymTuple sym@(IfaceData { ifName = name }) =
      let updOccMSymTuple = filterDecl occMSymTuple name (MTypeDecl sym)
      in  addDataCons updOccMSymTuple (ifCons sym)
    -- A 'newtype' (synonym) declaration
    processDecl occMSymTuple sym@(IfaceSyn { ifName = name }) =
      filterDecl occMSymTuple name (MTypeDecl sym)
    -- A 'class' declaration: insert the class name first, followed by its functions
    processDecl occMSymTuple sym@(IfaceClass { ifName = name }) =
      let updOccMSymTuple = filterDecl occMSymTuple name (MClassDecl sym)
      in  Fold.foldl' filterSig updOccMSymTuple (ifSigs sym)
    -- Ingore anything else...
    processDecl occMSymTuple (IfaceForeign _ _) = occMSymTuple

-- | Process
filterDecl :: ModStateT -> OccName -> ModDecl -> ModStateT
filterDecl mstate name sym =
  let nameStr  = mkRdrUnqual name
      eSet     = exportSyms mstate
      msymMap  = modSyms mstate
      symSeq = case Map.lookup nameStr msymMap of
                (Just msyms) -> Set.insert sym msyms
                Nothing      -> Set.singleton sym
  in  if Set.member name eSet
        then mstate {
                exportSyms = Set.delete name eSet
              , modSyms    = Map.insert nameStr symSeq msymMap
              }
        else mstate

addDataCons :: ModStateT -> IfaceConDecls -> ModStateT
addDataCons occMSymTuple IfAbstractTyCon = occMSymTuple
addDataCons occMSymTuple IfOpenDataTyCon = occMSymTuple
addDataCons occMSymTuple (IfDataTyCon conDecls) = Fold.foldl' filterCon occMSymTuple conDecls
addDataCons occMSymTuple (IfNewTyCon newTyDecl) =             filterCon occMSymTuple newTyDecl

filterCon :: ModStateT -> IfaceConDecl -> ModStateT
filterCon mstate c@(IfCon { ifConOcc = name }) =
  let nameStr = mkRdrUnqual name
      eSet    = exportSyms mstate
      msymMap = modSyms mstate
      conSym  = MConDecl c
      symSeq  = case Map.lookup nameStr msymMap of
                  (Just msyms) -> Set.insert conSym msyms
                  Nothing      -> Set.singleton conSym
  in  if Set.member name eSet
        then mstate {
            exportSyms = Set.delete name eSet
          , modSyms    = Map.insert nameStr symSeq msymMap
          }
        else mstate

filterSig :: ModStateT -> IfaceClassOp -> ModStateT
filterSig mstate op@(IfaceClassOp name _ _) =
  let nameStr = mkRdrUnqual name
      msymMap = modSyms mstate
      eSet    = exportSyms mstate
      sigSym  = MClassOp op 
      symSeq  = case Map.lookup nameStr msymMap of
                  (Just msyms) -> Set.insert sigSym msyms
                  Nothing      -> Set.singleton sigSym
  in  if Set.member name eSet
        then mstate {
                exportSyms = Set.delete name eSet
              , modSyms = Map.insert nameStr symSeq msymMap
              }
        else mstate

-- | Load an interface file
getInterfaceFile :: Module
                 -> ScionM (Maybe (ModIface, FilePath))
getInterfaceFile m =
    let iface              = findAndReadIface empty m False
        gblEnv             = IfGblEnv { if_rec_types = Nothing }
        ifaceLoader hscEnv = liftIO $ initTcRnIf  'a' hscEnv gblEnv () iface
        returnIFace result = case result of
                              Maybes.Succeeded mIface -> return (Just mIface)
                              _otherwise              -> return Nothing
    in getSession >>= ifaceLoader >>= returnIFace

-- | Fabricate a module name that can be easily detected as bogus. The main source
-- of these "unknown" modules is the exception raised by 'modLookup' (below) when
-- GHC can't figure out to whom the module belongs. Consequently, these modules are
-- not candidates from which names are extracted.
unknownModule :: ModuleName
              -> Module
unknownModule = mkModule unknownPackageId
