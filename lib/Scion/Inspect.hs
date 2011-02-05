{-# LANGUAGE PatternGuards, CPP,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeSynonymInstances, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
-- |
-- Module      : Scion.Inspect
-- Copyright   : (c) Thomas Schilling 2008
-- License     : BSD-style
--
-- Maintainer  : nominolo@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Functionality to inspect Haskell programs.
--
module Scion.Inspect 
  ( typeOfResult, prettyResult, haddockType, qualifiedResult
  , typeDecls, typeDeclsParsed, classDecls, familyDecls
  , toplevelNames, outline
  , tokensArbitrary
  , tokenTypesArbitrary
  , tokenArbitraryAtPoint, tokenArbitraryPreceding
  {- , visibleConstructors -}
  , module Scion.Inspect.Find
  , module Scion.Inspect.TypeOf
  , preprocessSource
  ) where

import Scion.Ghc
import Scion.Inspect.Find
import Scion.Inspect.TypeOf
import Scion.Types.Notes
import Scion.Types.Outline
import Scion.Types

import DynFlags
import ErrUtils
import FastString
import Lexer
import Bag
import Var ( varType )
import qualified Var( varName ) 
import DataCon ( dataConUserType )
import Type ( tidyType )
import VarEnv ( emptyTidyEnv )
import GHC.SYB.Utils()
import qualified Outputable as O ( (<>), empty, dot )

import Data.Data
import Data.Generics.Biplate
import qualified Data.Generics.Str as U 
import Data.List (sortBy,isPrefixOf)
import Data.Ord (comparing)
-- import Data.Maybe
import Data.List ( foldl' )

-- import Debug.Trace (trace)

#if __GLASGOW_HASKELL__ >= 610
import StringBuffer
#endif

#ifdef SCION_DEBUG
--import FastString
import Test.QuickCheck()
import Test.GHC.Gen()


--import Debug.Trace
--import StaticFlags ( initStaticOpts )
#endif
------------------------------------------------------------------------------

-- | Extract the type of a search result.
typeOfResult :: SearchResult Id -> Maybe Type
typeOfResult (FoundId i) = Just $ tidyType emptyTidyEnv $ varType i
typeOfResult (FoundCon _ c) = Just $ dataConUserType c
typeOfResult _ = Nothing

-- | Pretty-print a search result.
prettyResult :: OutputableBndr id => SearchResult id -> SDoc
prettyResult (FoundId i) = ppr i
prettyResult (FoundName n) = ppr n
prettyResult (FoundCon _ c) = ppr c
prettyResult r = ppr r

-- | Pretty-print a search result as qualified name 
qualifiedResult :: OutputableBndr id => SearchResult id -> SDoc
qualifiedResult (FoundId i) = qualifiedName $ getName  i
qualifiedResult (FoundName n) = qualifiedName n
qualifiedResult (FoundCon _ c) = qualifiedName $ getName c
qualifiedResult r = ppr r

qualifiedName :: Name -> SDoc
qualifiedName n = maybe O.empty  (\x-> (ppr x) O.<> O.dot) (nameModule_maybe n) O.<> (ppr n)

haddockType  :: SearchResult a -> String
haddockType (FoundName n)
        | isValOcc (nameOccName n)="v"
        | otherwise= "t"
haddockType (FoundId i)
        | isValOcc (nameOccName (Var.varName i))="v"
        | otherwise= "t"
haddockType _="t"

-- Haddock: Haddock.Backend.Xhtml.Utils.spliceURL                    
--                    (name, kind) =
--    case maybe_name of
--      Nothing             -> ("","")
--      Just n | isValOcc (nameOccName n) -> (escapeStr (getOccString n), "v")
--             | otherwise -> (escapeStr (getOccString n), "t")  
------------------------------------------------------------------------------

-- | Generate the located list of type, data and type synonyms for a type-checked module.
typeDecls :: TypecheckedModule -> [LTyClDecl Name]
typeDecls m =
  let srcgrp = renamedSourceGroup `fmap` renamedSource m
      typeDecls' (Just grp) = [ t | t <- hs_tyclds grp
                                  , isDataDecl (unLoc t) 
                                  || isTypeDecl (unLoc t) 
                                  || isSynDecl (unLoc t) ]
                                  -- XXX: include families?
      typeDecls' Nothing = error "typeDecls: No renamer information available."
  in  typeDecls' srcgrp  

-- | Generate the list of types, data and type synonyms for a parsed module 
typeDeclsParsed :: ParsedModule -> [TyClDecl RdrName]
typeDeclsParsed pm =
  let -- Check HsDecl is a TyClD:
      isTyClDecl (TyClD _) = True
      isTyClDecl _         = False
      -- Extract the TyClDecl from the HsDecl
      getTyClDecl (TyClD decl) = decl
      getTyClDecl _otherwise   = error "Unexpectedly bad filtering in typeDeclsParsed"
      -- Get the declarations from the module, stripping off the location information
      modDecls = map unLoc $ (hsmodDecls . unLoc . pm_parsed_source) pm
  in  [ getTyClDecl ty | ty <- modDecls, isTyClDecl ty ]
 
classDecls :: RenamedSource -> [LTyClDecl Name]
classDecls rn_src =
    [ t | t <- hs_tyclds (renamedSourceGroup rn_src)
        , isClassDecl (unLoc t) ]

familyDecls :: RenamedSource -> [LTyClDecl Name]
familyDecls rn_src =
    [ t | t <- hs_tyclds (renamedSourceGroup rn_src)
        , isFamilyDecl (unLoc t) ]

toplevelNames :: BgTcCache -> [SDoc]
toplevelNames m  = extractNames (outline "" m)

------------------------------------------------------------------------------

typeToName :: Outputable o => [(TyClDecl o -> Bool, [Char])]
typeToName = [(isFamilyDecl, "family")
             ,(isClassDecl,  "class")
             ,(isDataDecl,   "data")
             ,(isSynDecl,    "syn")
             ,(const True,   "type")]

mkConDeclOutlineDef :: Outputable o => FilePath -> SrcSpan -> o -> Located (ConDecl o) -> [OutlineDef]
mkConDeclOutlineDef  base_dir sp n (L _ c@(ConDecl { con_name = lname})) =
  let
    L sp2 n2 = lname
    o1 = OutlineDef (Left $ ppr n2) "constructor"
                    (ghcSpanToLocation base_dir sp2) (ghcSpanToLocation base_dir sp)
                    (Just (ppr n,"data"))

    os = case con_details c of
           RecCon flds -> 
             [ OutlineDef (Left $ ppr $ n3) "field"
                          (ghcSpanToLocation base_dir sp3)
                          (ghcSpanToLocation base_dir sp)
                          (Just (ppr n2,"constructor"))
              | L sp3 n3 <- map cd_fld_name flds ]
           _ -> []
  in
    (o1:os)

mkOutlineDef :: (Eq o, Outputable o) => FilePath -> Located (TyClDecl o) -> [OutlineDef]
mkOutlineDef base_dir (L sp (TyData {tcdLName = tc_name, tcdCons = cons})) =
  let
    L sp2 n = tc_name
    o1 = OutlineDef (Left $ ppr n) "data" 
                    (ghcSpanToLocation base_dir sp2)
                    (ghcSpanToLocation base_dir sp)
                    Nothing
    os = concat $ map (mkConDeclOutlineDef base_dir sp2 n) cons
  in
    (o1:os)
mkOutlineDef base_dir (L sp (ClassDecl {tcdLName = cls_name, tcdSigs = sigs})) =
  let
    L sp2 n = cls_name
    o1 = OutlineDef (Left $ ppr n) "class"
                    (ghcSpanToLocation base_dir sp2)
                    (ghcSpanToLocation base_dir sp)
                    Nothing
    os = [OutlineDef (Left $ ppr n2) "function" 
                     (ghcSpanToLocation base_dir sp3)
                     (ghcSpanToLocation base_dir sp)
                     (Just (ppr n,"class"))
          | L _ (TypeSig (L sp3 n2) _) <- sigs]
  in
    (o1:os)
mkOutlineDef base_dir (L sp t) = 
  let
    tN = foldl' (\tn (f, result) -> 
                     if null tn 
                     then if (f t) 
                          then result
                          else tn
                     else tn)
                "" typeToName
  in
    [OutlineDef (Right $ showSDocUnqual $ ppr n) tN
                (ghcSpanToLocation base_dir sp2)
                (ghcSpanToLocation base_dir sp)
                Nothing
     | L sp2 n <- tyClDeclNames t]

valBinds :: FilePath -> HsGroup Name -> [OutlineDef]
valBinds base_dir grp =
    let ValBindsOut bind_grps _sigs = hs_valds grp
    in [ n | (_, binds0) <- bind_grps
           , L sp bind <- bagToList binds0
           , n <- mkValBind base_dir sp bind 
       ]


mkValBind :: (Outputable o, Data o) =>
                                     FilePath -> SrcSpan -> HsBindLR o t -> [OutlineDef]
mkValBind base_dir sp (FunBind {fun_id = L sp2 n}) =
                      [OutlineDef (Left $ ppr n) "function"
                                  (ghcSpanToLocation base_dir sp2)
                                  (ghcSpanToLocation base_dir sp)
                                  Nothing]
mkValBind base_dir sp (PatBind {pat_lhs = p@(L sp2 _)}) =
                      [OutlineDef (Left $ ppr n) "pattern"
                                  (ghcSpanToLocation base_dir sp2)
                                  (ghcSpanToLocation base_dir sp)
                                  Nothing
                       | n <- pat_names p]
    where
            -- return names bound by pattern
            pat_names :: (Outputable o, Data o) => LPat o -> [o]
            pat_names lpat = collectPatBinders lpat
--            pat_names pat = 
--                [ n | Just n <- map pat_bind_name 
--                                  (trace (showData Renamer 2 (pat, universe pat)) (universe pat)) ]
--            pat_bind_name :: (Outputable o, Data o) => Pat o -> Maybe o
--            pat_bind_name (VarPat id) = Just id
--           pat_bind_name (AsPat (L _ id) _) = Just id
--            pat_bind_name _ = Nothing
mkValBind _ _ _ = []

mkInstDef :: OutputableBndr o => FilePath -> Located(InstDecl o) -> OutlineDef
mkInstDef base_dir (L sp (InstDecl inst_ty _ _ _))=OutlineDef (Right $ showSDocUnqual $ ppr inst_ty) "instance"
              (ghcSpanToLocation base_dir sp)
              (ghcSpanToLocation base_dir sp) Nothing
              

instBinds :: FilePath -> HsGroup Name -> [OutlineDef]
instBinds base_dir grp = map (mkInstDef base_dir) (hs_instds grp)

-- | Generate outline view for the given module.
outline ::FilePath       -- ^ The base directory for relative source locations,
                         -- typically the project root.
        -> BgTcCache     -- ^ Background type checker's cache
        -> [OutlineDef]  -- ^ Outline definitions
        
outline base_dir (Typechecked mod) =
  let srcgroup = renamedSourceGroup `fmap` renamedSource mod
      outline' (Just grp) = concatMap (mkOutlineDef base_dir) (hs_tyclds grp)
                            ++ valBinds base_dir grp
                            ++ instBinds base_dir grp
      outline' _ = []
  in  outline' srcgroup

outline base_dir (Parsed mod) = concatMap (mkOutlineDef' base_dir) (hsmodDecls $ unLoc $ pm_parsed_source mod)

mkOutlineDef' :: FilePath          -- ^ The document's root path for relative source name
               -> LHsDecl RdrName  -- ^ The parsed source, with source locations
               -> [OutlineDef]     -- ^ Outline definitions
mkOutlineDef' base_dir (L sp (TyClD d)) = mkOutlineDef base_dir (L sp d)
mkOutlineDef' base_dir (L sp (InstD d)) = [mkInstDef base_dir (L sp d)]
mkOutlineDef' base_dir (L sp (ValD d))  = mkValBind base_dir sp d
mkOutlineDef' _ _ = []

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Completion support
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

{-
visibleConstructors :: FilePath
                    -> BgTcCache
                    -> [SDoc]

Hold onto this: will need it for converting a data type name into its constructor alternatives 
visibleConstructors projectRoot (Typechecked mod) =
  let srcgroup = renamedSourceGroup `fmap` renamedSource mod
      getDataTypes (Just grp) = [ t | (L _ t) <- hs_tyclds grp, isDataDecl t ]
      getDataTypes Nothing    = []
      iterCTors (TyData { tcdCons = lConsList }) = [ (formatCtor . unLoc) i | i <- lConsList]
      iterCTors _ = []
      formatCtor (ConDecl { con_name = name, con_qvars = vars }) =
        (ppr . unLoc) name <+> interppSP [ unLoc i | i <- vars ]
  in  concatMap iterCTors (getDataTypes srcgroup)

visibleConstructors projectRoot (Parsed mod) = [O.text "!!foobars!!"]
-}



{- FIXME: unused
tokens :: FilePath -> Module -> ScionM ([TokenDef])
tokens base_dir m = do
        ts<-getTokenStream m
        return $ catMaybes $ map (mkTokenDef base_dir) ts -}

-- | Lex the current document contents, returning either a diagnostic Note or [TokenDef] token list.
tokensArbitrary :: FilePath  -> String -> ScionM (Either Note [TokenDef])
tokensArbitrary base_dir contents =
  (\result ->
    case result of 
      Right toks -> return $ Right $ map (mkTokenDef base_dir) toks
      Left n     -> return $ Left n
  ) =<< ghctokensArbitrary base_dir contents

-- | Get a stream of tokens generated by the GHC lexer from the current document
ghctokensArbitrary :: FilePath -- ^ The file path to the document
                   -> String -- ^ The document's contents
                   -> ScionM (Either Note [Located Token])
ghctokensArbitrary base_dir contents = do
        sb <- liftIO $ stringToStringBuffer contents
        --setActiveComponent comp
        --setComponentDynFlags comp
        dflags0 <- getSessionDynFlags
        let dflags1 = foldl' dopt_set dflags0 lexerFlags
        --let dflags1 = dflags0{flags=(Opt_TemplateHaskell:(flags dflags0))}
        let prTS = lexTokenStream sb (mkSrcLoc (mkFastString "<interactive>") 1 0) dflags1
        --setSessionDynFlags dflags0
        case prTS of
                POk _ toks      -> return $ Right $ (filter ofInterest toks)
                PFailed loc msg -> return $ Left $ ghcErrMsgToNote base_dir $ mkPlainErrMsg loc msg

lexerFlags :: [DynFlag]
lexerFlags =
        [ Opt_ForeignFunctionInterface
        , Opt_PArr
        , Opt_Arrows
        , Opt_TemplateHaskell
        , Opt_QuasiQuotes
        , Opt_ImplicitParams
        , Opt_BangPatterns
        , Opt_TypeFamilies
        , Opt_Haddock
        , Opt_MagicHash
        , Opt_KindSignatures
        , Opt_RecursiveDo
        , Opt_UnicodeSyntax
        , Opt_UnboxedTuples
        , Opt_StandaloneDeriving
        , Opt_TransformListComp
        , Opt_NewQualifiedOperators
#if GHC_VERSION > 611       
        , Opt_ExplicitForAll -- 6.12
        , Opt_DoRec -- 6.12
#endif
        ]                
               
-- | Filter tokens whose span appears legitimate (start line is less than end line, start column is
-- less than end column.)
ofInterest :: Located Token -> Bool
ofInterest (L span _) =
  let  sl = srcSpanStartLine span
       sc = srcSpanStartCol span
       el = srcSpanEndLine span
       ec = srcSpanEndCol span
  in (sl < el) || (sc < ec)

toInteractive ::  Location -> Location
toInteractive l =
  let (_, sl1, sc1, el1, ec1) = viewLoc l
  in  mkLocation interactive sl1 sc1 el1 ec1

-- | Generate the interactive token list used by EclipseFP for syntax highlighting
tokenTypesArbitrary :: FilePath -> String -> Bool -> ScionM (Either Note [TokenDef])
tokenTypesArbitrary projectRoot contents literate = generateTokens projectRoot contents literate convertTokens id
  where
    convertTokens = map (tokenToType projectRoot)

-- | Extract the lexer token preceding the line/column location.  
tokenArbitraryPreceding :: FilePath    -- ^ Project root or base directory for absolute path conversion
                           -> String   -- ^ Contents to be parsed
                           -> Int      -- ^ Line
                           -> Int      -- ^ Column
                           -> Bool     -- ^ Literate source flag (True = literate, False = ordinary)
                           -> ScionM (Either Note TokenDef)
tokenArbitraryPreceding projectRoot contents line column literate =
  let -- Convert line/column to a token location
      tokLocation = mkLocPointForSource interactive line column
      -- The undefined/unknown token to indicate failure
      undefToken = TokenDef (mkTokenName (ITunknown "")) (mkNoLoc "no token")
      -- Filter tokens to find token preceding specified location
      tokenPreceding = tokenPreceding' undefToken
      -- The function that actually extracts the preceding token
      tokenPreceding' :: TokenDef -> [TokenDef] -> TokenDef
      tokenPreceding' tok [] = tok
      tokenPreceding' precToken (tok:toks)
        | TokenDef _ span <- tok
        , tokLocation <= span || overlapLoc tokLocation span
        = precToken
        | otherwise
        = tokenPreceding' tok toks
  in generateHaskellLexerTokens projectRoot contents literate tokenPreceding
  
tokenArbitraryAtPoint :: FilePath -- ^ Project root or base directory for absolute path conversion
                      -> String   -- ^ Contents to be parsed
                      -> Int      -- ^ Line
                      -> Int      -- ^ Column
                      -> Bool     -- ^ Literate source flag (True = literate, False = ordinary)
                      -> ScionM (Either Note TokenDef)
tokenArbitraryAtPoint projectRoot contents line column literate =
  let -- Convert line/column to a token location
      tokLocation = mkLocPointForSource interactive line column
      -- The undefined/unknown token to indicate failure
      undefToken = TokenDef (mkTokenName (ITunknown "")) (mkNoLoc "no token")
      -- The filter function to extract the token at point
      tokenAtPoint [] = undefToken
      tokenAtPoint (tok:toks)
        | TokenDef _ span <- tok
        , tokLocation <= span || overlapLoc tokLocation span
        = tok
        | otherwise
        = tokenAtPoint toks
  in generateHaskellLexerTokens projectRoot contents literate tokenAtPoint 

-- | Parse the current document, generating a TokenDef list, filtered by a function
generateTokens :: FilePath                        -- ^ The project's root directory
               -> String                          -- ^ The current document contents, to be parsed
               -> Bool                            -- ^ Literate Haskell flag
               -> ([Located Token] -> [TokenDef]) -- ^ Transform function from GHC tokens to TokenDefs
               -> ([TokenDef] -> a)               -- ^ The TokenDef filter function
               -> ScionM (Either Note a)
generateTokens projectRoot contents literate xform filterFunc =
  let (ppTs, ppC) = preprocessSource contents literate
  in   ghctokensArbitrary projectRoot ppC
       >>= (\result ->
             case result of 
               Right toks ->
                 let filterResult = filterFunc $ sortBy (comparing td_loc) (ppTs ++ (xform toks))
                 --liftIO $ putStrLn $ show tokenList
                 in return $ Right filterResult
               Left n-> return $ Left n
           )

-- | A variation on generateTokens, which produces a TokenDef list of Haskell lexer tokens
-- (as opposed to interactive tokens, like tokensArbitrary), which can be filtered
generateHaskellLexerTokens :: FilePath                        -- ^ The project's root directory
                           -> String                          -- ^ The current document contents, to be parsed
                           -> Bool                            -- ^ Literate Haskell flag
                           -> ([TokenDef] -> a)               -- ^ The TokenDef filter function
                           -> ScionM (Either Note a)
generateHaskellLexerTokens projectRoot contents literate filterFunc =
  let -- Convert GHC's Located Tokens to TokenDefs 
      toTokenDefList = map (mkTokenDef projectRoot)
  in generateTokens projectRoot contents literate toTokenDefList filterFunc
      
-- | Convert a GHC token to an interactive token (abbreviated token type)
tokenToType :: FilePath -> Located Token -> TokenDef
tokenToType base_dir (L sp t) = TokenDef (tokenType t) (toInteractive $ ghcSpanToLocation base_dir sp)

-- | Make a token definition from its source location and Lexer.hs token type.
mkTokenDef :: FilePath -> Located Token -> TokenDef
mkTokenDef base_dir (L sp t) = TokenDef (mkTokenName t) (ghcSpanToLocation base_dir sp)

mkTokenName :: Token -> String
mkTokenName t = showConstr $ toConstr t

interactive :: LocSource
interactive = OtherSrc "<interactive>"

-- | Preprocess some source, returning the literate and Haskell source as tuple.
preprocessSource ::  String -> Bool -> ([TokenDef],String)
preprocessSource contents literate=
        let 
                (ts1,s2)=if literate then ppSF contents ppSLit else ([],contents) 
                (ts2,s3)=ppSF s2 ppSCpp
        in (ts1++ts2,s3)
        where 
                ppSF contents2 p= let
                        linesWithCount=zip (lines contents2) [1..]
                        (ts,nc,_)=foldl' p ([],[],False) linesWithCount
                        in (reverse ts,unlines $ reverse nc)
                ppSCpp :: ([TokenDef],[String],Bool) -> (String,Int) -> ([TokenDef],[String],Bool)
                ppSCpp (ts2,l2,f) (l,c) 
                        | f = addPPToken "PP" (l,c) (ts2,l2,'\\' == (last l))
                        | ('#':_)<-l =addPPToken "PP" (l,c) (ts2,l2,'\\' == (last l)) 
                        | ("{-# LINE" `isPrefixOf` l)=addPPToken "D" (l,c) (ts2,l2,False) 
                        | otherwise =(ts2,l:l2,False)
                ppSLit :: ([TokenDef],[String],Bool) -> (String,Int) -> ([TokenDef],[String],Bool)
                ppSLit (ts2,l2,f) (l,c) 
                        | ('>':lCode)<-l, True<-literate=(ts2,(' ':lCode):l2,f)
                        | otherwise =addPPToken "DL" (l,c) (ts2,l2,f)  
                addPPToken :: String -> (String,Int) -> ([TokenDef],[String],Bool) -> ([TokenDef],[String],Bool)
                addPPToken name (l,c) (ts2,l2,f) =((TokenDef name (mkLocation interactive c 0 c (length l))):ts2,"":l2,f)

deriving instance Typeable Token
deriving instance Data Token

#if CABAL_VERSION == 106
deriving instance Typeable StringBuffer
deriving instance Data StringBuffer
#endif

------------------------------------------------------------------------------


instance Uniplate a => Biplate a a where
  biplate = uniplate

instance Uniplate (Pat n) where
  uniplate pat = case pat of
    WildPat _         -> (Zero, \Zero -> pat)
    VarPat _          -> (Zero, \Zero -> pat)
    VarPatOut _n _     -> (Zero, \Zero -> pat) --down binds (VarPatOut n)
    LazyPat (L s p)   -> (One p, \(One p') -> LazyPat (L s p'))
    AsPat n (L s p)   -> (One p, \(One p') -> AsPat n (L s p'))
    ParPat (L s p)    -> (One p, \(One p') -> ParPat (L s p'))
    BangPat (L s p)   -> (One p, \(One p') -> BangPat (L s p'))
    ListPat ps t      -> down ps (\ps' -> ListPat ps' t)
    TuplePat ps b t   -> down ps (\ps' -> TuplePat ps' b t)
    PArrPat ps t      -> down ps (\ps' -> PArrPat ps' t)
    ConPatIn c details -> down details (ConPatIn c)
    ConPatOut dcon tvs pds binds args ty -> -- also look inside binds?
              down args (\args' -> ConPatOut dcon tvs pds binds args' ty)
    ViewPat e (L s p) t -> (One p, \(One p') -> ViewPat e (L s p') t)
    QuasiQuotePat _   -> (Zero, \Zero -> pat)
    LitPat _          -> (Zero, \Zero -> pat)
    NPat _ _ _        -> (Zero, \Zero -> pat)
    NPlusKPat _ _ _ _ -> (Zero, \Zero -> pat)
    TypePat _         -> (Zero, \Zero -> pat)
    SigPatIn (L s p) t -> (One p, \(One p') -> SigPatIn (L s p') t)
    SigPatOut (L s p) t -> (One p, \(One p') -> SigPatOut (L s p') t)
    CoPat w p t       -> (One p, \(One p') -> CoPat w p' t)

instance Biplate (HsConDetails (LPat id) (HsRecFields id (LPat id))) (Pat id) where
  biplate (PrefixCon ps) = down ps PrefixCon
  biplate (RecCon fs)    = down fs RecCon
  biplate (InfixCon (L sl l) (L sr r)) =
      (Two (One l) (One r),
      \(Two (One l') (One r')) -> InfixCon (L sl l') (L sr r'))

instance (Uniplate arg) => Biplate (HsRecFields id (Located arg)) arg where
  biplate (HsRecFields flds dotdot) =
      down flds (\flds' -> HsRecFields flds' dotdot)

instance (Uniplate arg) => Biplate (HsRecField id (Located arg)) arg where
  biplate (HsRecField lid (L sl arg) b) = 
      (One arg, \(One arg') -> HsRecField lid (L sl arg') b)

instance Biplate b a => Biplate (Bag b) a where
  biplate = uniplateOnBag biplate

--instance Biplate (HsBindLr n n) 

uniplateOnBag :: (a -> (U.Str b, U.Str b -> a))
              -> Bag a -> (U.Str b, U.Str b -> Bag a)
uniplateOnBag f bag = (as, \ns -> listToBag (bs ns))
                      where (as, bs) = uniplateOnList f (bagToList bag)

down :: Biplate from to => from -> (from -> c) -> (U.Str to, U.Str to -> c)
down b f = (ps, \ps' -> f (set ps'))
  where (ps, set) = biplate b

-- BiplateType from to = (Str to, Str to -> from)

instance Biplate b a => Biplate (Located b) a where
  biplate (L s b) = down b (L s)
{-
instance Uniplate a => Biplate (Located a) a where
  biplate (L s a) = (One a, \(One a') -> L s a')
-}
instance Biplate b a => Biplate [b] a where
  biplate = uniplateOnList biplate
{-                           
fmap (fst . biplate) (listStr bs),
                strList . fmap (snd . biplate))
-}
{-                
  -- biplate :: (Str a, Str a -> [b])
  -- biplate :: (Str a, Str a -> b)
-}
{-
instance Biplate [(Located a)] a where
  biplate las = (unLoc `fmap` listStr las,
                 zipWith (\(L s _) a -> L s a) las . strList)
-}
{-
instance Biplate a b => Biplate (Bag a) b where
  biplate b = (foldBag 
-}

tokenType :: Token -> String
tokenType  ITas = "K"                         -- Haskell keywords
tokenType  ITcase = "K"
tokenType  ITclass = "K"
tokenType  ITdata = "K"
tokenType  ITdefault = "K"
tokenType  ITderiving = "K"
tokenType  ITdo = "K"
tokenType  ITelse = "K"
tokenType  IThiding = "K"
tokenType  ITif = "K"
tokenType  ITimport = "K"
tokenType  ITin = "K"
tokenType  ITinfix = "K"
tokenType  ITinfixl = "K"
tokenType  ITinfixr = "K"
tokenType  ITinstance = "K"
tokenType  ITlet = "K"
tokenType  ITmodule = "K"
tokenType  ITnewtype = "K"
tokenType  ITof = "K"
tokenType  ITqualified = "K"
tokenType  ITthen = "K"
tokenType  ITtype = "K"
tokenType  ITwhere = "K"
tokenType  ITscc = "K"                       -- ToDo: remove (we use {-# SCC "..." #-} now)

tokenType  ITforall = "EK"                    -- GHC extension keywords
tokenType  ITforeign = "EK"
tokenType  ITexport= "EK"
tokenType  ITlabel= "EK"
tokenType  ITdynamic= "EK"
tokenType  ITsafe= "EK"
tokenType  ITthreadsafe= "EK"
tokenType  ITunsafe= "EK"
tokenType  ITstdcallconv= "EK"
tokenType  ITccallconv= "EK"
#if __GLASGOW_HASKELL__ >= 612
tokenType  ITprimcallconv= "EK"
#endif
tokenType  ITmdo= "EK"
tokenType  ITfamily= "EK"
tokenType  ITgroup= "EK"
tokenType  ITby= "EK"
tokenType  ITusing= "EK"

        -- Pragmas
tokenType  (ITinline_prag {})="P"          -- True <=> INLINE, False <=> NOINLINE
#if __GLASGOW_HASKELL__ >= 612
tokenType  (ITinline_conlike_prag {})="P"  -- same
#endif
tokenType  ITspec_prag="P"                 -- SPECIALISE   
tokenType  (ITspec_inline_prag {})="P"     -- SPECIALISE INLINE (or NOINLINE)
tokenType  ITsource_prag="P"
tokenType  ITrules_prag="P"
tokenType  ITwarning_prag="P"
tokenType  ITdeprecated_prag="P"
tokenType  ITline_prag="P"
tokenType  ITscc_prag="P"
tokenType  ITgenerated_prag="P"
tokenType  ITcore_prag="P"                 -- hdaume: core annotations
tokenType  ITunpack_prag="P"
#if __GLASGOW_HASKELL__ >= 612
tokenType  ITann_prag="P"
#endif
tokenType  ITclose_prag="P"
tokenType  (IToptions_prag {})="P"
tokenType  (ITinclude_prag {})="P"
tokenType  ITlanguage_prag="P"

tokenType  ITdotdot="S"                    -- reserved symbols
tokenType  ITcolon="S"
tokenType  ITdcolon="S"
tokenType  ITequal="S"
tokenType  ITlam="S"
tokenType  ITvbar="S"
tokenType  ITlarrow="S"
tokenType  ITrarrow="S"
tokenType  ITat="S"
tokenType  ITtilde="S"
tokenType  ITdarrow="S"
tokenType  ITminus="S"
tokenType  ITbang="S"
tokenType  ITstar="S"
tokenType  ITdot="S"

tokenType  ITbiglam="ES"                    -- GHC-extension symbols

tokenType  ITocurly="SS"                    -- special symbols
tokenType  ITccurly="SS" 
tokenType  ITocurlybar="SS"                 -- "{|", for type applications
tokenType  ITccurlybar="SS"                 -- "|}", for type applications
tokenType  ITvocurly="SS" 
tokenType  ITvccurly="SS" 
tokenType  ITobrack="SS" 
tokenType  ITopabrack="SS"                   -- [:, for parallel arrays with -XParr
tokenType  ITcpabrack="SS"                   -- :], for parallel arrays with -XParr
tokenType  ITcbrack="SS" 
tokenType  IToparen="SS" 
tokenType  ITcparen="SS" 
tokenType  IToubxparen="SS" 
tokenType  ITcubxparen="SS" 
tokenType  ITsemi="SS" 
tokenType  ITcomma="SS" 
tokenType  ITunderscore="SS" 
tokenType  ITbackquote="SS" 

tokenType  (ITvarid {})="IV"        -- identifiers
tokenType  (ITconid {})="IC"
tokenType  (ITvarsym {})="IV"
tokenType  (ITconsym {})="IC"
tokenType  (ITqvarid {})="IV"
tokenType  (ITqconid {})="IC"
tokenType  (ITqvarsym {})="IV"
tokenType  (ITqconsym {})="IC"
tokenType  (ITprefixqvarsym {})="IV"
tokenType  (ITprefixqconsym {})="IC"

tokenType  (ITdupipvarid {})="EI"   -- GHC extension: implicit param: ?x

tokenType  (ITchar {})="LC"
tokenType  (ITstring {})="LS"
tokenType  (ITinteger {})="LI"
tokenType  (ITrational {})="LR"

tokenType  (ITprimchar {})="LC"
tokenType  (ITprimstring {})="LS"
tokenType  (ITprimint {})="LI"
tokenType  (ITprimword {})="LW"
tokenType  (ITprimfloat {})="LF"
tokenType  (ITprimdouble {})="LD"

  -- Template Haskell extension tokens
tokenType  ITopenExpQuote="TH"              --  [| or [e|
tokenType  ITopenPatQuote="TH"              --  [p|
tokenType  ITopenDecQuote="TH"              --  [d|
tokenType  ITopenTypQuote="TH"              --  [t|         
tokenType  ITcloseQuote="TH"                --tokenType ]
tokenType  (ITidEscape {})="TH"    --  $x
tokenType  ITparenEscape="TH"               --  $( 
tokenType  ITvarQuote="TH"                  --  '
tokenType  ITtyQuote="TH"                   --  ''
tokenType  (ITquasiQuote {})="TH" --  [:...|...|]

  -- Arrow notation extension
tokenType  ITproc="A"
tokenType  ITrec="A"
tokenType  IToparenbar="A"                 --  (|
tokenType  ITcparenbar="A"                 --tokenType )
tokenType  ITlarrowtail="A"                --  -<
tokenType  ITrarrowtail="A"                --  >-
tokenType  ITLarrowtail="A"                --  -<<
tokenType  ITRarrowtail="A"                --  >>-

#if GHC_VERSION < 611
tokenType  ITdotnet="SS"                   -- ??
tokenType  (ITpragma _) = "SS"             -- ??
#endif

tokenType  (ITunknown {})=""           -- Used when the lexer can't make sense of it
tokenType  ITeof=""                       -- end of file token

  -- Documentation annotations
tokenType  (ITdocCommentNext {})="D"     -- something beginning '-- |'
tokenType  (ITdocCommentPrev {})="D"    -- something beginning '-- ^'
tokenType  (ITdocCommentNamed {})="D"     -- something beginning '-- $'
tokenType  (ITdocSection {})="D" -- a section heading
tokenType  (ITdocOptions {})="D"    -- doc options (prune, ignore-exports, etc)
tokenType  (ITdocOptionsOld {})="D"     -- doc options declared "-- # ..."-style
tokenType  (ITlineComment {})="D"     -- comment starting by "--"
tokenType  (ITblockComment {})="D"     -- comment in {- -}
