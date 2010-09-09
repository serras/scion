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
  ( typeOfResult, prettyResult
  , typeDecls, classDecls, familyDecls
  , toplevelNames, outline, tokensArbitrary, tokenTypesArbitrary
  , module Scion.Inspect.Find
  , module Scion.Inspect.TypeOf
  , preprocessSource
  ) where

import Scion.Ghc
import Scion.Utils()
import Scion.Inspect.Find
import Scion.Inspect.TypeOf
import Scion.Types.Notes
import Scion.Types.Outline
import Scion.Types
import Scion.Session

import DynFlags
import ErrUtils
import FastString
import Lexer
import Bag
import Var ( varType )
import DataCon ( dataConUserType )
import Type ( tidyType )
import VarEnv ( emptyTidyEnv )

import Data.Data
import Data.Generics.Biplate
import qualified Data.Generics.Str as U 
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe
import GHC.SYB.Utils
import Data.List ( foldl' )

#if __GLASGOW_HASKELL__ >= 612
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

------------------------------------------------------------------------------

typeDecls :: TypecheckedMod m => m -> [LTyClDecl Name]
typeDecls m | Just grp <- renamedSourceGroup `fmap` renamedSource m =
    [ t | t <- hs_tyclds grp
        , isDataDecl (unLoc t) 
            || isTypeDecl (unLoc t) 
            || isSynDecl (unLoc t) ]
    -- XXX: include families?
typeDecls _ = error "typeDecls: No renamer information available."

classDecls :: RenamedSource -> [LTyClDecl Name]
classDecls rn_src =
    [ t | t <- hs_tyclds (renamedSourceGroup rn_src)
        , isClassDecl (unLoc t) ]

familyDecls :: RenamedSource -> [LTyClDecl Name]
familyDecls rn_src =
    [ t | t <- hs_tyclds (renamedSourceGroup rn_src)
        , isFamilyDecl (unLoc t) ]

toplevelNames :: TypecheckedMod m => m -> [Name]
toplevelNames m  = extractNames (outline "" m)

------------------------------------------------------------------------------

typeToName :: [(TyClDecl Name -> Bool, [Char])]
typeToName = [(isFamilyDecl, "family")
             ,(isClassDecl,  "class")
             ,(isDataDecl,   "data")
             ,(isSynDecl,    "syn")
             ,(const True,   "type")]

mkConDeclOutlineDef :: FilePath -> SrcSpan -> Name -> Located (ConDecl Name) -> [OutlineDef]
mkConDeclOutlineDef  base_dir sp n (L _ c@(ConDecl { con_name = lname})) =
  let
    L sp2 n2 = lname
    o1 = OutlineDef (Left n2) "constructor"
                    (ghcSpanToLocation base_dir sp2) (ghcSpanToLocation base_dir sp)
                    (Just (n,"data"))

    os = case con_details c of
           RecCon flds -> 
             [ OutlineDef (Left n3) "field"
                          (ghcSpanToLocation base_dir sp3)
                          (ghcSpanToLocation base_dir sp)
                          (Just (n2,"constructor"))
              | L sp3 n3 <- map cd_fld_name flds ]
           _ -> []
  in
    (o1:os)

mkOutlineDef :: FilePath -> Located (TyClDecl Name) -> [OutlineDef]
mkOutlineDef base_dir (L sp (TyData {tcdLName = tc_name, tcdCons = cons})) =
  let
    L sp2 n = tc_name
    o1 = OutlineDef (Left n) "data" 
                    (ghcSpanToLocation base_dir sp2)
                    (ghcSpanToLocation base_dir sp)
                    Nothing
    os = concat $ map (mkConDeclOutlineDef base_dir sp2 n) cons
  in
    (o1:os)
mkOutlineDef base_dir (L sp (ClassDecl {tcdLName = cls_name, tcdSigs = sigs})) =
  let
    L sp2 n = cls_name
    o1 = OutlineDef (Left n) "class"
                    (ghcSpanToLocation base_dir sp2)
                    (ghcSpanToLocation base_dir sp)
                    Nothing
    os = [OutlineDef (Left n2) "function" 
                     (ghcSpanToLocation base_dir sp3)
                     (ghcSpanToLocation base_dir sp)
                     (Just (n,"class"))
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
    [OutlineDef (Left n) tN
                (ghcSpanToLocation base_dir sp2)
                (ghcSpanToLocation base_dir sp)
                Nothing
     | L sp2 n <- tyClDeclNames t]

valBinds :: FilePath -> HsGroup Name -> [OutlineDef]
valBinds base_dir grp =
    let ValBindsOut bind_grps _sigs = hs_valds grp
    in [ n | (_, binds0) <- bind_grps
           , L sp bind <- bagToList binds0
           , n <- case bind of
                    FunBind {fun_id = L sp2 n} ->
                      [OutlineDef (Left n) "function"
                                  (ghcSpanToLocation base_dir sp2)
                                  (ghcSpanToLocation base_dir sp)
                                  Nothing]
                    PatBind {pat_lhs = L sp2 p} ->
                      [OutlineDef (Left n) "pattern"
                                  (ghcSpanToLocation base_dir sp2)
                                  (ghcSpanToLocation base_dir sp)
                                  Nothing
                       | n <- pat_names p]
                    _ -> []
       ]
  where
    -- return names bound by pattern
    pat_names :: Pat Name -> [Name]
    pat_names pat = 
        [ n | Just n <- map pat_bind_name 
                          (trace (showData Renamer 2 (pat, universe pat)) (universe pat)) ]

    pat_bind_name :: Pat Name -> Maybe Name
    pat_bind_name (VarPat id) = Just id
    pat_bind_name (AsPat (L _ id) _) = Just id
    pat_bind_name _ = Nothing

instBinds :: FilePath -> HsGroup Name -> [OutlineDef]
instBinds base_dir grp = 
  [OutlineDef (Right $ pretty n) "instance"
              (ghcSpanToLocation base_dir sp)
              (ghcSpanToLocation base_dir sp) Nothing
   | L sp n <- hs_instds grp]
 where
   pretty (InstDecl inst_ty _ _ _) = showSDocUnqual $ ppr inst_ty

-- | Generate outline view for the given module.
outline :: TypecheckedMod m => 
           FilePath
           -- ^ The base directory for relative source locations,
           -- typically the project root.
        -> m
        -> [OutlineDef]
outline base_dir m
  | Just grp <- renamedSourceGroup `fmap` renamedSource m =
     concatMap (mkOutlineDef base_dir) (hs_tyclds grp)
       ++ valBinds base_dir grp
       ++ instBinds base_dir grp
outline _ _ = []

tokens :: FilePath -> Module -> ScionM ([TokenDef])
tokens base_dir m = do
        ts<-getTokenStream m
        return $ catMaybes $ map (mkTokenDef base_dir) ts

tokensArbitrary :: FilePath  -> String -> ScionM (Either Note [TokenDef])
tokensArbitrary base_dir contents = do
        r<-ghctokensArbitrary base_dir contents
        case r of 
                Right ts->return $ Right $ catMaybes $ map (mkTokenDef base_dir) ts
                Left n->return $ Left n

ghctokensArbitrary :: FilePath -> String -> ScionM (Either Note [Located Token])
ghctokensArbitrary base_dir contents = do
        sb <- liftIO $ stringToStringBuffer contents
        --setActiveComponent comp
        --setComponentDynFlags comp
        dflags0 <- getSessionDynFlags
        let dflags1 = foldl' dopt_set dflags0 lexerFlags
        --let dflags1 = dflags0{flags=(Opt_TemplateHaskell:(flags dflags0))}
        let prTS=lexTokenStream sb (mkSrcLoc (mkFastString "<interactive>") 1 0) dflags1
        --setSessionDynFlags dflags0
        case prTS of
                POk _ ts        -> return $ Right $ (filter ofInterest ts)
                PFailed loc msg -> return $ Left $ ghcErrMsgToNote base_dir $ mkPlainErrMsg loc msg

lexerFlags :: [DynFlag]
lexerFlags =
        [ Opt_ForeignFunctionInterface
        , Opt_PArr,
        , Opt_Arrows,
        , Opt_TemplateHaskell,
        , Opt_QuasiQuotes,
        , Opt_ImplicitParams,
        , Opt_BangPatterns, 
        , Opt_TypeFamilies,
        , Opt_Haddock,
        , Opt_MagicHash,
        , Opt_KindSignatures,
        , Opt_RecursiveDo,
        , Opt_UnicodeSyntax,
        , Opt_UnboxedTuples,
        , Opt_StandaloneDeriving,
        , Opt_TransformListComp,
        , Opt_NewQualifiedOperators
#if GHC_VERSION > 611       
        , Opt_ExplicitForAll -- 6.12
        , Opt_DoRec -- 6.12
#endif
        ]                
                
                
ofInterest :: Located Token -> Bool
ofInterest (L sp _) | 
                sl <-(srcSpanStartLine sp),
                sc <- (srcSpanStartCol sp),
                el <- (srcSpanEndLine sp),
                ec <- (srcSpanEndCol sp)   = (sl < el) || (sc < ec)

tokenTypesArbitrary :: FilePath -> String -> Bool -> ScionM (Either Note [TokenDef])
tokenTypesArbitrary base_dir contents literate= do
        let (ppTs,ppC)=preprocessSource contents literate
        r<-ghctokensArbitrary base_dir ppC
        case r of 
                Right ts->do
                        return $ Right $ sortBy (comparing td_loc) (ppTs ++ (map (tokenToType base_dir) ts))
                Left n->return $ Left n
                 
tokenToType :: FilePath -> Located Token -> TokenDef
tokenToType base_dir (L sp t) =TokenDef (tokenType t) (ghcSpanToLocation base_dir sp)

mkTokenDef :: FilePath -> Located Token -> Maybe TokenDef
mkTokenDef base_dir (L sp t) | Just s<-mkTokenName t=Just $ TokenDef s (ghcSpanToLocation base_dir sp)
mkTokenDef _ _=Nothing

mkTokenName :: Token -> Maybe String
mkTokenName t= Just $ showConstr $ toConstr t

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
                        | otherwise =(ts2,l:l2,False)
                ppSLit :: ([TokenDef],[String],Bool) -> (String,Int) -> ([TokenDef],[String],Bool)
                ppSLit (ts2,l2,f) (l,c) 
                        | ('>':lCode)<-l, True<-literate=(ts2,(' ':lCode):l2,f)
                        | otherwise =addPPToken "DL" (l,c) (ts2,l2,f)  
                addPPToken :: String -> (String,Int) -> ([TokenDef],[String],Bool) -> ([TokenDef],[String],Bool)
                addPPToken name (l,c) (ts2,l2,f) =((TokenDef name (mkLocation (OtherSrc "<interactive>") c 0 c (length l))):ts2,"":l2,f)

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
tokenType  ITocurlybar="SS"                  -- {|, for type applications
tokenType  ITccurlybar="SS"                  -- |}, for type applications
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
