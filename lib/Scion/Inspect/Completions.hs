{-# LANGUAGE ScopedTypeVariables #-}

module Scion.Inspect.Completions
  ( getTypeCompletions
  , getVarIdCompletions
  , getClassTypeNameCompletions
  )
where

import Scion.Types
import Scion.Inspect.IFaceLoader

-- GHC's imports
import GHC
import HscTypes
import Outputable
import RdrName
import Module

import qualified Data.Map as Map
import qualified Data.List as List

-- | The Prelude's module name (handy for working on Prelude-related stuff)
preludeModName :: ModuleName
preludeModName  = mkModuleName "Prelude"

-- | Generate the completions for types
getTypeCompletions :: Maybe ModSummary
                    -> ScionM CompletionTuples
getTypeCompletions (Just modSum) = generateCompletions modSum onlyIETypeThings hasMTypeDecl
getTypeCompletions Nothing = return []

-- | Generate the completions for varIds
getVarIdCompletions :: Maybe ModSummary
                    -> ScionM CompletionTuples
getVarIdCompletions (Just modSum) = generateCompletions modSum onlyIEVarIdThings hasMIdDecl
getVarIdCompletions Nothing = return []

-- | Generate the completions for class type names
getClassTypeNameCompletions :: Maybe ModSummary
                            -> ScionM CompletionTuples
getClassTypeNameCompletions (Just modSum) = generateCompletions modSum onlyIEClassThings hasMClassDecl
getClassTypeNameCompletions Nothing = return []

-- | The main workhorse for generating completion tuples
generateCompletions :: ModSummary
                    -> (IE RdrName -> Bool)
                    -> (ModSymDecls -> Bool)
                    -> ScionM CompletionTuples
generateCompletions modSum ieFunc modSymFunc =                     
  let impDecls        = map unLoc $ ms_imps modSum
      innerModNames   = map (unLoc . ideclName) impDecls
      getInnerModules = mapM modLookup innerModNames
      -- Catch the GHC source error exception when a module doesn't appear to be loaded
      modLookup mName = gcatch (lookupModule mName Nothing >>= (\m -> return m ))
                               (\(_ :: SourceError) -> return (unknownModule mName))
      getCompletionTuples curMods = 
        withSessionState $ generateCompletionTuples (ms_mod modSum) curMods ieFunc modSymFunc
  in  getInnerModules >>= getCompletionTuples

-- | Look through the module map and scan for completions
generateCompletionTuples :: Module
                         -> [Module]
                         -> (IE RdrName -> Bool)
                         -> (ModSymDecls -> Bool)
                         -> SessionState
                         -> ScionM CompletionTuples
generateCompletionTuples topMod depMods ieFunc modSymFunc session =
  let mCache = moduleCache session
      usedMods = Map.filterWithKey (\k _ -> k `List.elem` depMods) mCache
      filteredMods   = filterMods modSymFunc usedMods

      impDecls = case Map.lookup topMod mCache of
                  (Just struct) -> importDecls struct
                  -- This is a punt: If we have an empty import declaration list, assume we should
                  -- "import" symbols from all modules. This may not actually be correct, but it
                  -- offers the user something.
                  Nothing       -> map (dummyImportDecl . moduleName) depMods
                  
      dummyImportDecl m = ImportDecl {
          ideclName = noLoc m
        , ideclPkgQual = Nothing
        , ideclSource = False
        , ideclQualified = False
        , ideclAs = Nothing
        , ideclHiding = Nothing
        }
        
      modDecls = zip depMods impDecls
      depmodCompletions = concatMap (formatModSymData filteredMods ieFunc) modDecls
      
      preludeCompletions  = if preludeModName `List.notElem` [ moduleName m | m <- depMods ]
                              then implicitPreludeModSyms mCache modSymFunc
                              else []
  in  return (depmodCompletions ++ preludeCompletions)

filterMods :: (ModSymDecls -> Bool)
           -> ModuleCache
           -> Map.Map Module ModSymData
filterMods declPred modCache = Map.map filterModCacheData modCache
  where
    filterModCacheData mcd = Map.filter declPred $ modSymData mcd

formatModSymData :: Map.Map Module ModSymData
                 -> (IE RdrName -> Bool)
                 -> (Module, ImportDecl RdrName)
                 -> CompletionTuples
formatModSymData modMap iePred (m, idecl) = 
  let modName = moduleName m
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
             -> ModuleName                    -- ^ The module name, as a string
             -> RdrName                       -- ^ The symbol to format
             -> CompletionTuples              -- ^ [(symbol, original module)] result
formatModSym idecl modName sym = 
  let symName
        | (Just asModName) <- ideclAs idecl
        -- "import ... as <foo>"
        = (ppr asModName) <> text "." <> (ppr sym)
        -- "import qualified <mod>" 
        | ideclQualified idecl
        = (ppr modName) <> text "." <> (ppr sym)
        | otherwise
        = ppr sym
  in  [(showSDoc symName, (showSDoc . ppr) modName)]

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

implicitPreludeModSyms :: ModuleCache
                      -> (ModSymDecls -> Bool)
                      -> CompletionTuples
implicitPreludeModSyms mCache filterFunc =
  let msyms = case Map.lookup preludeMod mCache of
                (Just mcd) -> Map.filter filterFunc (modSymData mcd)
                Nothing    -> Map.empty
      preludeMod = mkModule basePackageId preludeModName
  in  [ ((showSDoc . ppr) name, "Prelude") | name <- Map.keys msyms ]


-- | Filter predicate for extracting type constructors from an import entity
onlyIETypeThings :: (IE RdrName) -> Bool
onlyIETypeThings (IEThingAbs _)    = True
onlyIETypeThings (IEThingAll _)    = True
onlyIETypeThings (IEThingWith _ _) = True
onlyIETypeThings _                 = False

-- | Filter predicate for extracting variable identifiers from an import entity
onlyIEVarIdThings :: (IE RdrName) -> Bool
onlyIEVarIdThings (IEVar _) = True
onlyIEVarIdThings _         = False

-- | Filter predicate for extracting class type names from import entities

-- N.B.: 
onlyIEClassThings :: (IE RdrName) -> Bool
onlyIEClassThings = onlyIETypeThings
