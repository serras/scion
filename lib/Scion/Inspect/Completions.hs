{-# LANGUAGE ScopedTypeVariables #-}

module Scion.Inspect.Completions
  ( getModulesFromTypecheck
  )
where

import Scion.Types
import Scion.Inspect
import Scion.Inspect.IFaceLoader

-- GHC's imports
import GHC
import HscTypes
import Outputable

import Control.Monad

-- | Get the list of modules associated with the type-checked source, updating the module cache
-- as needed.
getModulesFromTypecheck :: Maybe BgTcCache                                 -- ^ The type-checked source
                        -> ScionM (Module, [(Module, ImportDecl RdrName)]) -- ^ The list of modules

getModulesFromTypecheck (Just (Typechecked tcm)) = generateModules (tm_parsed_module tcm) >>= updateModulesCache
getModulesFromTypecheck (Just (Parsed pm))       = generateModules pm >>= updateModulesCache
-- Just keep the compiler happy! This should never get matched.
getModulesFromTypecheck Nothing = undefined

-- | Update the module cache
updateModulesCache :: (Module, [(Module, ImportDecl RdrName)])
                   -> ScionM (Module, [(Module, ImportDecl RdrName)])
updateModulesCache result@(_, depMods) =
      gets moduleCache
      >>= updateModules (map fst depMods)
      >>= (\newModCache -> modifySessionState $ updateSessionMCache newModCache)
      >> return result

-- Install the new module cache in the SessionState record:
updateSessionMCache :: ModuleCache
                    -> SessionState
		                -> SessionState
updateSessionMCache newModCache session = session { moduleCache = newModCache }

-- | Fabricate a module name that can be easily detected as bogus. The main source
-- of these "unknown" modules is the exception raised by 'modLookup' (below) when
-- GHC can't figure out to whom the module belongs. Consequently, these modules are
-- not candidates from which names are extracted.
unknownModule :: ModuleName
              -> Module
unknownModule = mkModule unknownPackageId

-- | Extract the modules referenced by the current parsed module, returning
-- the primary module's data and a list of the dependent modules
generateModules :: ParsedModule              -- ^ The current module
                -> ScionM (Module, [(Module, ImportDecl RdrName)])
                   -- ^ Primary module, dependent modules list with import decls
generateModules pm
  = getInnerModules >>= (\innerMods -> return (thisMod, innerMods))
  where
    thisModSum      = pm_mod_summary pm
    thisMod         = ms_mod thisModSum
    innerImpDecls   = map unLoc $ ms_imps thisModSum
    innerModNames   = map (unLoc . ideclName) innerImpDecls
    -- getInnerModules :: ScionM [(Module, ImportDecl RdrName)] 
    getInnerModules = zipWithM modLookup innerModNames innerImpDecls
    -- modLookup :: ModuleName -> ImportDecl RdrName -> ScionM (Module, ImportDecl RdrName)
    -- Catch the GHC source error exception when a module doesn't appear to be loaded
    modLookup mName idecl = gcatch (lookupModule mName Nothing >>= (\m -> return (m, idecl)))
                                   (\(_ :: SourceError) -> return $ (unknownModule mName, idecl))

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
