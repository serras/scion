{-# LANGUAGE ScopedTypeVariables #-}

module Scion.Inspect.Completions
  ( getTyConCompletions
  , getModulesFromTypecheck
  )
where

import Scion.Types
import Scion.Inspect
import Scion.Inspect.IFaceLoader

-- GHC's imports
import GHC
import HscTypes
import Outputable
import RdrName

import Control.Monad
import qualified Data.Map as Map
import qualified Data.List as List

type CurrentModules = (Module, [(Module, ImportDecl RdrName)])

-- | Generate the completions for type constructors
getTyConCompletions :: Maybe BgTcCache
                    -> ScionM [(String, String)]
getTyConCompletions tychk =
  getModulesFromTypecheck tychk
  >>= (\curMods -> withSessionState $ generateTyConCompletions curMods tychk) 

-- | Look through the module map and scan for completions
generateTyConCompletions :: CurrentModules
                         -> Maybe BgTcCache
                         -> SessionState
                         -> ScionM [(String, String)]
generateTyConCompletions (_topMod, depModInfo) tychk session =
  let depMods = map fst depModInfo
      usedMods = Map.filterWithKey (\k _ -> k `List.elem` depMods) $ moduleCache session
      filteredMods :: Map.Map Module ModSymData
      filteredMods   = filterMods hasMTypeDecl usedMods

      topModCompletions = extractMainPackageModuleSyms tychk
      
      depmodCompletions = concatMap (formatModSymData filteredMods onlyIETypeThings) depModInfo
      completions = topModCompletions ++ depmodCompletions
  in  return completions

-- | Filter predicate for extracting type constructors from an import entity
onlyIETypeThings :: (IE RdrName) -> Bool
onlyIETypeThings (IEThingAbs _)    = True
onlyIETypeThings (IEThingAll _)    = True
onlyIETypeThings (IEThingWith _ _) = True
onlyIETypeThings _                 = False

filterMods :: (ModSymDecls -> Bool)
           -> ModuleCache
           -> Map.Map Module ModSymData
filterMods declPred modCache = Map.map filterModCacheData modCache
  where
    filterModCacheData mcd = Map.filter declPred $ modSymData mcd

formatModSymData :: Map.Map Module ModSymData
                 -> (IE RdrName -> Bool)
                 -> (Module, ImportDecl RdrName)
                 -> [(String, String)]
formatModSymData modMap iePred (m, idecl) = 
  let modName = (moduleNameString . moduleName) m
      formatModSyms =
        case ideclHiding idecl of
          Just (hideFlag, decls) ->
            let filteredIE = map ieName $ List.filter iePred $ map unLoc decls
            in  \result key _ -> result ++ (doIEThings hideFlag filteredIE key)
          Nothing                -> \result key _ -> result ++ (formatModSym idecl modName key)

      doIEThings hideFlag decls key =
        case symIsMember hideFlag key decls of
          (Just _) -> formatModSym idecl modName key
          Nothing  -> [] 

  in  case Map.lookup m modMap of
        (Just msyms) -> Map.foldlWithKey formatModSyms [] msyms
        Nothing      -> []
        
formatModSym :: ImportDecl RdrName            -- ^ The import declaration, for symbol qualifiers, etc.
             -> String                        -- ^ The module name, as a string
             -> RdrName                       -- ^ The symbol to format
             -> [(String, String)]            -- ^ [(symbol, original module)] result
formatModSym idecl modName sym = 
  let rawSymName = (showSDoc . ppr) sym
      symName
        | (Just asModName) <- ideclAs idecl
        -- "import ... as <foo>"
        = (moduleNameString asModName) ++ "." ++ rawSymName
        -- "import qualified <mod>" 
        | ideclQualified idecl
        = modName ++ "." ++ rawSymName
        | otherwise
        = rawSymName
  in  [(symName, modName)]

symIsMember :: Bool
            -> RdrName
            -> [RdrName]
            -> Maybe RdrName
-- Terminate search
symIsMember _ _ [] = Nothing

symIsMember hideFlag sym@(Unqual symName) (result@(Unqual name):names)
  | (hideFlag && symName /= name) || (symName == name)
  = Just result
  | otherwise
  = symIsMember hideFlag sym names
symIsMember hideFlag sym@(Unqual symName) (result@(Qual _ name):names)
  | (hideFlag && symName /= name) || (symName == name)
  = Just result
  | otherwise
  = symIsMember hideFlag sym names
symIsMember hideFlag sym@(Unqual symName) (result@(Orig _ name):names)
  | (hideFlag && symName /= name) || (symName == name)
  = Just result
  | otherwise
  = symIsMember hideFlag sym names
  
-- Note: We only stash unqualified names in the module symbol data map, so if
-- these patterns are matched, we actually have some kind of a design problem:

symIsMember _hideFlag (Qual _ _) _names = undefined
symIsMember _hideFlag (Orig _ _) _names = undefined
symIsMember _hideFlag (Exact  _) _names = undefined

-- Can't do much with 'Exact' names
symIsMember hideFlag sym ((Exact _):names) = symIsMember hideFlag sym names

-- | Get the list of modules associated with the type-checked source, updating the module cache
-- as needed.
getModulesFromTypecheck :: Maybe BgTcCache       -- ^ The type-checked source
                        -> ScionM CurrentModules -- ^ The list of modules

getModulesFromTypecheck (Just (Typechecked tcm)) = generateModules (tm_parsed_module tcm) >>= updateModuleCache
getModulesFromTypecheck (Just (Parsed pm))       = generateModules pm >>= updateModuleCache
-- Just keep the compiler happy! This should never get matched.
getModulesFromTypecheck Nothing = error "getModulesFromTypecheck Nothing"

-- | Update the module cache
updateModuleCache :: CurrentModules
                  -> ScionM CurrentModules
updateModuleCache result@(_, depMods) =
      getSessionSelector moduleCache
      >>= updateModules (map fst depMods)
      >>= updateSessionCache
      >> return result

-- | Update the scion-server session's module cache.
updateSessionCache :: ModuleCache
                   -> ScionM ()
updateSessionCache newModCache = modifySessionState $ (\session -> session { moduleCache = newModCache })

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
                -> ScionM CurrentModules
                   -- ^ Primary module, dependent modules list with import decls
generateModules pm
  = getInnerModules >>= (\innerMods -> return (thisMod, innerMods))
  where
    thisModSum      = pm_mod_summary pm
    thisMod         = ms_mod thisModSum
    innerImpDecls   = map unLoc $ ms_imps thisModSum
    innerModNames   = map (unLoc . ideclName) innerImpDecls
    getInnerModules = zipWithM modLookup innerModNames innerImpDecls
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
