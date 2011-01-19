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
  ( updateModulesForTypecheck
  , getModulesForTypecheck
  ) where

import Scion.Types
import Scion.Inspect

import qualified Data.Map as Map

-- GHC's imports
import GHC
import GHC.Exception()
import HscTypes
import Module
import Outputable
import Finder

-- | Update the cached modules
updateModulesForTypecheck :: Maybe BgTcCache
                          -> ScionM ()
updateModulesForTypecheck tychk =
  let updater [] cache = return cache
      updater (m:mods) cache
        | ignorable m 
        = debugAction m "Ignoring "
          >> updater mods cache
        | Nothing <- Map.lookup m (modCache cache)
        = debugAction m "Adding   "
          >> updater mods cache
        | moduleChanged m (lastModTime cache)
        = debugAction m "Updating "
          >> updater mods cache
        | otherwise
        = debugAction m "NoMod    "
          >> updater mods cache
      -- A module is ignorable if it's part of the main package or it's unknown
      ignorable m = 
        let mpkg = modulePackageId m
        in  mpkg == mainPackageId || mpkg == unknownPackageId
      -- Test the module's time/date stamp changed
      moduleChanged m modTime =
        getSession
        >>= (\hsc -> (liftIO $ findExactModule hsc m)
                     >>= (\finfo -> case finfo of
                                      Found _ _ -> return True
                                      _otherwise -> return False
                         )
            )
      debugAction m msg = logInfo (msg ++ ((moduleNameString . moduleName) m))
      
      -- Process the dependent module list :
      processModules depmods =
        gets moduleCache
        >>= (\mc -> (liftIO $ updater depmods mc)
                    >>= (\newModCache -> modifySessionState $ updateSessionMCache newModCache))

      -- Install the new module cache in the SessionState record:
      updateSessionMCache newModCache session = session { moduleCache = newModCache } 

  in getModulesForTypecheck tychk
     >>= (\(topMod, depMods) -> processModules depMods)
     >> return ()

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
