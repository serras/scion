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

-- System imports
import System.Directory
import System.Time

-- | Synonym for a set of OccNames
type OccNameSet = Set.Set OccName

-- | Update the cached modules
updateModulesCache :: Maybe BgTcCache
                   -> ScionM ()
updateModulesCache tychk =
  let updater [] cache = return cache
      updater (m:mods) cache
        | ignorable m 
        = modDebugMsg m "Ignoring "
          >> updater mods cache
        | Nothing <- Map.lookup m (modCache cache)
        = modDebugMsg m "Adding   "
          >> cacheModule m cache
          >>= (\updCache -> updater mods updCache)
        | otherwise
        = ifM (moduleChanged m (lastModTime cache))
            (modDebugMsg m "Updating "
              >> cacheModule m cache
              >>= (\updCache -> updater mods updCache))
            (modDebugMsg m "NoMod    " >> updater mods cache)
            
      -- A module is ignorable if it's part of the main package or it's unknown
      ignorable m = 
        let mpkg = modulePackageId m
        in  mpkg == mainPackageId || mpkg == unknownPackageId
      -- Predicate for detecting if the module's time/date stamp has changed
      moduleChanged m modTime = getSession >>= (\hsc -> liftIO (findExactModule hsc m >>= checkMTimes))
        where
          -- May return True or False
          checkMTimes (Found loc _) =
            modTime
            >>= (\mcMTime -> getModificationTime (ml_hi_file loc)
                             >>= (\hiMTime -> return (diffClockTimes mcMTime hiMTime == noTimeDiff)))
          -- Ensure that we leave the interface file alone if it cannot be found.
          checkMTimes _ = return False

      -- Process the dependent module list :
      processModules depmods =
        gets moduleCache
        >>= (\mc -> updater depmods mc
                    >>= (\newModCache -> modifySessionState $ updateSessionMCache newModCache))

      -- Install the new module cache in the SessionState record:
      updateSessionMCache newModCache session = session { moduleCache = newModCache } 

  in getModulesForTypecheck tychk
     >>= (\(_, depMods) -> processModules depMods)
     >> return ()

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
          Just (iface, _) ->
            extractIfaceDecls iface (modCache cache)
            >> return cache { modCache = Map.insert m NothingYet (modCache cache) }
          Nothing         -> modDebugMsg m "Could not load " >> return cache
      )
          
extractIfaceDecls :: ModIface -> ModuleCacheData -> ScionM ModuleCacheData
extractIfaceDecls iface mCache =
  mapM_ showExport (Set.toAscList exportSet) >> return mCache
  where
    exportSet :: OccNameSet
    exportSet = collectExportNames (mi_exports iface)
    showExport eName = liftIO $ putStrLn (occNameString eName)
    
    -- Collect the exported names as a set, should make filtering the
    -- decls easier.
    collectExportNames exports =
      let -- collect :: OccNameSet -> [IfaceExport] -> OccNameSet
          collect eSet [] = eSet
          collect eSet (e:exps) = collect (insertExp eSet e) exps
          -- insertExp :: OccNameSet -> IfaceExport -> OccNameSet
          insertExp eSet (_, names) = Set.unions $ map (insertExp' eSet) names
          -- insertExp' :: OccNameSet -> (GenAvailInfo OccName) -> OccNameSet
          insertExp' eSet (Avail name) = Set.insert name eSet
          insertExp' eSet (AvailTC name mbrs) = Set.union (Set.insert name eSet) (Set.fromList mbrs)
      in collect Set.empty exports
    
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
