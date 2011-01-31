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
  ( updateModules
  , unknownPackageId
  ) where

import Scion.Types
import Scion.Utils

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
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
                              modsRead   = Set.singleton m
                            , exportSyms = eSet
                            , modSyms    = Map.empty
                            , hiddenMods = Set.empty
                            , otherMods  = Set.empty
                            }
      in  collectInterface initialMState iface
          >>= (\mstate ->
                let updMSyms = modSyms mstate
                in  (debugModSymData (exportSyms mstate) updMSyms)
                    >> (reportProblems m mstate)
                    >> (return $ Map.insert m (mkModCacheData fpath updMSyms) cache))
    
    readIFace Nothing = modDebugMsg m "Could not load " >> return cache

-- | Extract the set of names exported through the module interface
exportSet :: ModIface -> OccNameSet
exportSet iface = extractIfaceExports iface

reportProblems :: Module -> ModStateT -> ScionM ()
reportProblems m mstate =
  if {- not (Set.null (hiddenMods mstate)) || -} 
    not (Set.null (otherMods mstate))
    || not (Set.null (exportSyms mstate))
      then (liftIO $ logWarn $ (moduleNameString (moduleName m)) ++ " module cache:")
           {- >> listProblems "-- Hidden modules: " (modNameList (hiddenMods mstate)) -}
           >> listProblems "-- Unreadable modules: " (modNameList (otherMods mstate))
           >> listProblems "-- Symbols not cached: " (occNameList (exportSyms mstate))
      else return ()
  where
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
                                  modsRead   = Set.singleton m
                                , exportSyms = eSet
                                , modSyms    = Map.empty
                                , hiddenMods = Set.empty
                                , otherMods  = Set.empty
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
      addOtherMod theMod
        | not (Set.member theMod (otherMods mstate))
        = return mstate {
                  hiddenMods = Set.insert theMod (otherMods mstate)
                }
        | otherwise
        = return mstate
      procModule m
        | Set.notMember m mods
        = getInterfaceFile m >>= readModule m
        | otherwise
        = return mstate
      readModule m (Just (iface, _)) = collectInterface (updMState m) iface
      readModule m Nothing           = return (updMState m)
      updMState m = mstate { modsRead = Set.insert m mods }
  in  if not (Set.null (exportSyms mstate))
        then gcatch (gcatch (lookupModule usedMod Nothing >>= procModule)
                            -- We can also get a SourceError if GHC can't find the module
                            (\(_ :: SourceError) -> addHiddenMod usedMod)
                    )
                    -- If module is hidden, we get an IOError exception
                    (\(_ :: IOError) -> addOtherMod usedMod)
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
                                           (Set.fromList $ map (showSDoc . ppr) (Map.keys msyms))
    matchLengths
      | Set.null missing
      = "-- Everything extracted --"
      | otherwise
      = (show $ Set.size eSet) ++ " <> " ++ (show $ Map.size msyms) ++ "\ndifference is " ++ (show missing) ++ "\n"
    modSymDump = (Fold.foldl' (showModSymData) "" (Map.toList msyms))
    showModSymData s (name, decls) = s ++ ((showSDoc . ppr) name) ++ " -> [ " ++ (Fold.foldl showModDecls "" decls) ++ "]\n"
    showModDecls s d = s ++ (show d) ++ " "   

-- | Extract the occurance name set from the Haskell interface file. This is a simple
-- list transformation
extractIfaceExports :: ModIface -> OccNameSet
extractIfaceExports iface = Fold.foldl' insertExp Set.empty [i | (_, i) <- mi_exports iface]
  where
    insertExp eSet names = Fold.foldl' insertExp' eSet names
    insertExp' eSet (Avail name) = Set.insert name eSet
    insertExp' eSet (AvailTC name mbrs) = Set.union (Set.insert name eSet) (Set.fromList mbrs)

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
                (Just msyms) -> msyms Seq.|> sym
                Nothing      -> Seq.singleton sym
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
                  (Just msyms) -> msyms Seq.|> conSym
                  Nothing      -> Seq.singleton conSym
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
                  (Just msyms) -> msyms Seq.|> sigSym
                  Nothing      -> Seq.singleton sigSym
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

