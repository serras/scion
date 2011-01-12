{-# LANGUAGE ScopedTypeVariables, CPP, PatternGuards, FlexibleContexts,
             ExistentialQuantification  #-} -- for 'Cmd'
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Scion.Server.Commands
-- Copyright   : (c) Thomas Schilling 2008
-- License     : BSD-style
--
-- Maintainer  : nominolo@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Commands provided by the server.
--
-- TODO: Need some way to document the wire protocol.  Autogenerate?
--
module Scion.Server.Commands (
  handleRequest, malformedRequest, -- allCommands, allCommands',
  -- these are reused in the vim interface
  supportedPragmas, allExposedModules,
  cmdNamesInScope
) where

import Prelude as P
import Scion.Types
import Scion.Types.Notes
import Scion.Types.Outline
import qualified Scion.Types.JSONDictionary as Dic
import Scion.Utils
import Scion.Session
import Scion.Server.Protocol
import Scion.Inspect
import Scion.Inspect.DefinitionSite
import Scion.Inspect.PackageDB
import Scion.Cabal
import Scion.Ghc hiding ( (<+>) )

import DynFlags ( supportedLanguages, allFlags )
import Exception
import FastString
import PprTyThing ( pprTypeForUser )
import qualified Outputable as O ( (<+>),alwaysQualify,neverQualify,text )

import Control.Applicative
import Data.List ( nub )
import Data.Time.Clock  ( NominalDiffTime )
import System.Exit ( ExitCode(..) )
import Text.JSON.AttoJSON (JSON,JSValue(..),fromJSON,toJSON)
import qualified Data.ByteString.Char8 as S
import qualified Data.Map as M
import qualified Data.MultiSet as MS

import GHC.SYB.Utils

#ifndef HAVE_PACKAGE_DB_MODULES
import UniqFM ( eltsUFM )
import Packages ( pkgIdMap )

import Distribution.InstalledPackageInfo
#endif

type KeepGoing = Bool

-- a scion request is JS object with 3 keys:
-- method: the method to be called
-- params: arguments to be passed
-- id    : this value will be passed back to the client
--         to identify a reply to a specific request
--         asynchronous requests will be implemented in the future
handleRequest :: JSValue -> ScionM (JSValue, KeepGoing)
handleRequest req@(JSObject _) =
  let request = do JSString method <- Dic.lookupKey req (Dic.method)
                   params <- Dic.lookupKey req (Dic.params)
                   seq_id <- Dic.lookupKey req (Dic.id)
                   return (method, params, seq_id)
  in
  case request of
    Nothing -> return (malformedRequest, True)
    Just (method, params, seq_id)
     | method == Dic.quit -> return (Dic.makeObject
                             [(Dic.version, JSString $ Dic.version01)
                             ,(Dic.result, JSNull)
                             ,(Dic.id, seq_id)], False)
     | otherwise ->
      case M.lookup (S.unpack method) allCmds of
        Nothing -> return (unknownCommand seq_id, True)
        Just (Cmd _ arg_parser) ->
          decode_params params arg_parser seq_id
 where
   decode_params JSNull arg_parser seq_id =
       decode_params (Dic.makeObject []) arg_parser seq_id
   decode_params args@(JSObject _) arg_parser seq_id =
     case unPa arg_parser args of
       Left err -> return (paramParseError seq_id err, True)
       Right act -> do
           r <- handleScionException act
           case r of
             Error msg -> return (commandExecError seq_id msg, True)
             Ok a ->
                 return (Dic.makeObject
                    [(Dic.version, str "0.1")
                    ,(Dic.id, seq_id)
                    ,(Dic.result, toJSON a)], True)
   decode_params _ _ seq_id =
     return (paramParseError seq_id "Params not an object", True)

handleRequest _ = return(malformedRequest, True)

malformedRequest :: JSValue
malformedRequest = Dic.makeObject
 [(Dic.version, JSString Dic.version01)
 ,(Dic.error, Dic.makeObject
    [(Dic.name, str "MalformedRequest")
    ,(Dic.message, str "Request was not a proper request object.")])]

unknownCommand :: JSValue -> JSValue
unknownCommand seq_id = Dic.makeObject
 [(Dic.version, JSString Dic.version01)
 ,(Dic.id, seq_id)
 ,(Dic.error, Dic.makeObject
    [(Dic.name, str "UnknownCommand")
    ,(Dic.message, str "The requested method is not supported.")])]

paramParseError :: JSValue -> String -> JSValue
paramParseError seq_id msg = Dic.makeObject
 [(Dic.version, JSString Dic.version01)
 ,(Dic.id, seq_id)
 ,(Dic.error, Dic.makeObject
    [(Dic.name, str "ParamParseError")
    ,(Dic.message, str msg)])]

commandExecError :: JSValue -> String -> JSValue
commandExecError seq_id msg = Dic.makeObject
 [(Dic.version, JSString Dic.version01)
 ,(Dic.id, seq_id)
 ,(Dic.error, Dic.makeObject
    [(Dic.name, str "CommandFailed")
    ,(Dic.message, str msg)])]

allCmds :: M.Map String Cmd
allCmds = M.fromList [ (cmdName c, c) | c <- allCommands ]

------------------------------------------------------------------------

-- | All Commands supported by this Server.
allCommands :: [Cmd]
allCommands =
    [ cmdConnectionInfo
    , cmdListSupportedLanguages
    , cmdListSupportedPragmas
    , cmdListSupportedFlags
    , cmdListCabalComponents
    , cmdListCabalConfigurations
    , cmdWriteSampleConfig
    , cmdListRdrNamesInScope
    , cmdListExposedModules
    , cmdCurrentComponent
    , cmdSetVerbosity
    , cmdGetVerbosity
    , cmdLoad
    , cmdDumpSources
    , cmdThingAtPoint
    , cmdSetGHCVerbosity
    , cmdBackgroundTypecheckFile
    , cmdBackgroundTypecheckArbitrary
    , cmdAddCmdLineFlag
    , cmdForceUnload
    , cmdDumpDefinedNames
    , cmdDefinedNames
    , cmdNameDefinitions
    , cmdIdentify
    , cmdDumpModuleGraph
    , cmdDumpNameDB
    , cmdToplevelNames
    , cmdOutline
    , cmdTokens
    , cmdTokenAtPoint
    , cmdTokenPreceding
    , cmdTokenTypes
    , cmdParseCabal
    , cmdParseCabalArbitrary
    , cmdCabalDependencies
    , cmdModuleGraph
    -- , cmdTypeNames
    , cmdNamesInScope
    ]

------------------------------------------------------------------------------

data OkErr a = Error String | Ok a

-- encode expected errors as proper return values
handleScionException :: ScionM a -> ScionM (OkErr a)
handleScionException m = ((((do
   r <- m
   return (Ok r)
  `gcatch` \(e :: SomeScionException) -> return (Error (show e)))
  `gcatch` \(e' :: GhcException) ->
               case e' of
                Panic _ -> throw e'
                InstallationError _ -> throw e'
                Interrupted -> throw e'
                _ -> return (Error (show e')))
  `gcatch` \(e :: ExitCode) ->
                -- client code may not exit the server!
                return (Error (show e)))
  `gcatch` \(e :: IOError) ->
                return (Error (show e)))
--   `gcatch` \(e :: SomeException) ->
--                 liftIO (print e) >> liftIO (throwIO e)

------------------------------------------------------------------------------

-- | Parsed argument ("Pa") type
newtype Pa a = Pa {
   unPa :: JSValue
        -> Either String a
   }

instance Monad Pa where
  return x = Pa $ \_ -> Right x
  m >>= k = Pa $ \req ->
            case unPa m req of
              Left err -> Left err
              Right a -> unPa (k a) req
  fail msg = Pa $ \_ -> Left msg

withReq :: (JSValue -> Pa a) -> Pa a
withReq f = Pa $ \req -> unPa (f req) req

reqArg' :: JSON a => String -> (a -> b) -> (b -> r) -> Pa r
reqArg' name trans f = withReq $ \req ->
    case Dic.lookupKey req (S.pack name) of
      Nothing -> fail $ "required arg missing: " ++ name
      Just x ->
          case fromJSON x of
            Nothing -> fail $ "could not decode: " ++ name  -- ++ " - " ++ m
            Just a -> return (f (trans a))

optArg' :: JSON a => String -> b -> (a -> b) -> (b -> r) -> Pa r
optArg' name dflt trans f = withReq $ \req ->
    case Dic.lookupKey req (S.pack name) of
      Nothing -> return (f dflt)
      Just x ->
          case fromJSON x of
            Nothing -> fail $ "could not decode: " ++ name -- ++ " - " ++ n
            Just a -> return (f (trans a))

reqArg :: JSON a => String -> (a -> r) -> Pa r
reqArg name f = reqArg' name id f

optArg :: JSON a => String -> a -> (a -> r) -> Pa r
optArg name dflt f = optArg' name dflt id f

-- =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
-- Commonly used arguments:
-- =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

-- | Command takes no arguments
noArgs :: r -> Pa r
noArgs = return

-- | Command takes a file name argument
fileNameArg :: (String -> r) -> Pa r
fileNameArg = reqArg "file"

-- | Command takes a document argument
docContentsArg :: (String -> r) -> Pa r
docContentsArg = reqArg' "contents" S.unpack

-- | Command takes an optional literate Haskell flag
literateFlagOpt :: (Bool -> r) -> Pa r
literateFlagOpt = optArg' "literate" False decodeBool

-- | Command takes required line and column arguments
lineColumnArgs :: (Int -> Int -> r) -> Pa r
lineColumnArgs = reqArg "line" <&> reqArg "column"

-- | Combine two arguments.
--
-- TODO: explain type

infixr 1 <&>

(<&>) :: (a -> Pa b)
      -> (b -> Pa c)
      -> a -> Pa c
a1 <&> a2 = \f -> do f' <- a1 f; a2 f'

data Cmd = forall a. JSON a => Cmd String (Pa (ScionM a))

cmdName :: Cmd -> String
cmdName (Cmd n _) = n

------------------------------------------------------------------------

-- | Used by the client to initialise the connection.
cmdConnectionInfo :: Cmd
cmdConnectionInfo = Cmd "connection-info" $ noArgs worker
  where
    worker = let pid = 0 :: Int in -- TODO for linux: System.Posix.Internals (c_getpid)
             return $ Dic.makeObject
               [(Dic.version, toJSON scionVersion)
               ,(Dic.pid,     toJSON pid)]

decodeBool :: JSValue -> Bool
decodeBool (JSBool b) = b
decodeBool _ = error "no bool"

{- Unused at the moment
decodeExtraArgs :: JSValue -> [String]
decodeExtraArgs JSNull         = []
decodeExtraArgs (JSString s)   = words (S.unpack s) -- TODO: check shell-escaping
decodeExtraArgs (JSArray arr)  = [ S.unpack s | JSString s <- arr ]
decodeExtraArgs (JSBool b)     = [ (show b) ]
decodeExtraArgs (JSNumber b)   = [ (show b) ]
decodeExtraArgs (JSObject _)   = undefined -}

instance JSON Component where
  fromJSON obj = do
    case fromJSON obj of
      Just (c :: CabalComponent) -> return $ Component c
      Nothing -> case fromJSON obj of
             Just (c :: FileComp) -> return $ Component c
             Nothing -> fail $ "Unknown component" ++ show obj

  toJSON (Component c) = toJSON c

instance JSON CompilationResult where
  toJSON (CompilationResult suc notes time) =
      Dic.makeObject [(Dic.succeeded, JSBool suc)
                 ,(Dic.notes, toJSON notes)
                 ,(Dic.duration, toJSON time)]
  fromJSON obj@(JSObject _) = do
      JSBool suc <- Dic.lookupKey obj Dic.succeeded
      notes <- fromJSON =<< Dic.lookupKey obj Dic.notes
      dur <- fromJSON =<< Dic.lookupKey obj Dic.duration
      return (CompilationResult suc notes dur)
  fromJSON _ = fail "compilation-result"

instance JSON (MS.MultiSet Note) where
  toJSON ms = toJSON (MS.toList ms)
  fromJSON o = MS.fromList <$> fromJSON o

instance JSON Note where
  toJSON (Note note_kind loc msg) =
    Dic.makeObject [(Dic.kind, toJSON note_kind)
               ,(Dic.location, toJSON loc)
               ,(Dic.message, JSString (S.pack msg))]
  fromJSON obj@(JSObject _) = do
    note_kind <- fromJSON =<< Dic.lookupKey obj Dic.kind
    loc <- fromJSON =<< Dic.lookupKey obj Dic.location
    JSString s <- Dic.lookupKey obj Dic.message
    return (Note note_kind loc (S.unpack s))
  fromJSON _ = fail "note"

instance (JSON a, JSON b)=> JSON (Either a b) where
        toJSON (Left a)=Dic.makeObject [(Dic.leftC,toJSON a)]
        toJSON (Right a)=Dic.makeObject [(Dic.rightC,toJSON a)]
        fromJSON _ = fail "Either"

--instance (JSON a)=>JSON (Maybe a) where
--        toJSON (Nothing)=Dic.makeObject [(Dic.nothingC,JSNull)]
--        toJSON (Just a)=Dic.makeObject [(Dic.justC,toJSON a)]
--        fromJSON _ = fail "Maybe"

str :: String -> JSValue
str = JSString . S.pack

instance JSON NoteKind where
  toJSON ErrorNote   = JSString Dic.error
  toJSON WarningNote = JSString Dic.warning
  toJSON InfoNote    = JSString Dic.info
  toJSON OtherNote   = JSString Dic.other
  fromJSON (JSString s) =
      case lookup s
               [(Dic.error, ErrorNote), (Dic.warning, WarningNote)
               ,(Dic.info, InfoNote), (Dic.other, OtherNote)]
      of Just x -> return x
         Nothing -> fail "note-kind"
  fromJSON _ = fail "note-kind"

instance JSON Location where
  toJSON loc | not (isValidLoc loc) =
    Dic.makeObject [(Dic.noLocation, str (noLocText loc))]
  toJSON loc | (src, l0, c0, l1, c1) <- viewLoc loc =
    Dic.makeObject [case src of
                  FileSrc f -> (Dic.file, str (toFilePath f))
                  OtherSrc s -> (Dic.other, str s)
               ,(Dic.region, JSArray (map toJSON [l0,c0,l1,c1]))]
  fromJSON obj@(JSObject _) = do
    src <- (do JSString f <- Dic.lookupKey obj Dic.file
               return (FileSrc (mkAbsFilePath "/" (S.unpack f))))
           <|>
           (do JSString s <- Dic.lookupKey obj Dic.other
               return (OtherSrc (S.unpack s)))
    JSArray ls <- Dic.lookupKey obj Dic.region
    case mapM fromJSON ls of
      Just [l0,c0,l1,c1] -> return (mkLocation src l0 c0 l1 c1)
      _ -> fail "region"
  fromJSON _ = fail "location"

instance JSON NominalDiffTime where
  toJSON t = JSNumber (fromRational (toRational t))
  fromJSON (JSNumber  n) = return $ fromRational (toRational n)
  fromJSON _ = fail "diff-time"

instance JSON OutlineDef where
  toJSON t =
    Dic.makeObject $
      [(Dic.name, str $ case od_name t of
  	                Left n -> showSDocUnqual n
  	                Right s -> s)
      ,(Dic.location, toJSON $ od_loc t)
      ,(Dic.block, toJSON $ od_block t)
      ,(Dic.typ, str $ od_type t)]
      ++
      (case od_parentName t of
  	 Just (n,typ) ->
             [(Dic.parent, Dic.makeObject [(Dic.name, str $ showSDocUnqual $ n)
                                    ,(Dic.typ, str typ)])]
  	 Nothing -> [])
  fromJSON _ = fail "OutlineDef"


cmdListSupportedLanguages :: Cmd
cmdListSupportedLanguages = Cmd "list-supported-languages" $ noArgs cmd
  where cmd = return (map S.pack supportedLanguages)

cmdListSupportedPragmas :: Cmd
cmdListSupportedPragmas =
    Cmd "list-supported-pragmas" $ noArgs $ return supportedPragmas

supportedPragmas :: [String]
supportedPragmas =
    [ "OPTIONS_GHC", "LANGUAGE", "INCLUDE", "WARNING", "DEPRECATED"
    , "INLINE", "NOINLINE", "RULES", "SPECIALIZE", "UNPACK", "SOURCE"
    , "SCC"
    , "LINE" -- XXX: only used by code generators, still include?
    ]

cmdListSupportedFlags :: Cmd
cmdListSupportedFlags =
  Cmd "list-supported-flags" $ noArgs $ return (nub allFlags)

cmdListRdrNamesInScope :: Cmd
cmdListRdrNamesInScope =
    Cmd "list-rdr-names-in-scope" $ noArgs $ cmd
  where cmd = do
          rdr_names <- getNamesInScope
          return (map (showSDoc . ppr) rdr_names)

cmdListCabalComponents :: Cmd
cmdListCabalComponents =
    Cmd "list-cabal-components" $ reqArg' "cabal-file" S.unpack $ cmd
  where cmd cabal_file = cabalProjectComponents cabal_file

cmdParseCabal :: Cmd
cmdParseCabal =
    Cmd "parse-cabal" $ reqArg' "cabal-file" S.unpack $ cmd
  where cmd _cabal_file = return (JSObject M.empty) --liftM toJSON $ cabalParse cabal_file

cmdParseCabalArbitrary :: Cmd
cmdParseCabalArbitrary =
    Cmd "parse-cabal-arbitrary" $ docContentsArg $ cmd
  where cmd cabal_contents = cabalParseArbitrary cabal_contents

cmdCabalDependencies :: Cmd
cmdCabalDependencies =
    Cmd "cabal-dependencies" $ reqArg' "cabal-file" S.unpack $ cmd
  where cmd cabal_file = do
        dep<- cabalDependencies cabal_file
        return (JSArray $ map (\(x,y)->Dic.makeObject [(S.pack x,JSArray $ map toJSON y)]) dep)

-- return all cabal configurations.
-- currently this just globs for * /setup-config
-- in the future you may write a config file describing the most common configuration settings
cmdListCabalConfigurations :: Cmd
cmdListCabalConfigurations =
    Cmd "list-cabal-configurations" $
      reqArg' "cabal-file" S.unpack <&>
      optArg' "type" "uniq" id <&>
      optArg' "scion-default" False decodeBool $ cmd
  where cmd _cabal_file _type' _scionDefault = return (JSArray []) -- liftM toJSON $ cabalConfigurations cabal_file type' scionDefault

cmdWriteSampleConfig :: Cmd
cmdWriteSampleConfig =
    Cmd "write-sample-config" $ fileNameArg cmd
  where cmd fp = liftIO $ writeSampleConfig fp

allExposedModules :: ScionM [ModuleName]
#ifdef HAVE_PACKAGE_DB_MODULES
allExposedModules = map moduleName `fmap` packageDbModules True
#else
-- This implementation requires our Cabal to be the same as GHC's.
allExposedModules = do
   dflags <- getSessionDynFlags
   let pkg_db = pkgIdMap (pkgState dflags)
   return $ P.concat (map exposedModules (filter exposed (eltsUFM pkg_db)))
#endif

cmdListExposedModules :: Cmd
cmdListExposedModules = Cmd "list-exposed-modules" $ noArgs $ cmd
  where cmd = do
          mod_names <- allExposedModules
          return $ map (showSDoc . ppr) mod_names

cmdSetGHCVerbosity :: Cmd
cmdSetGHCVerbosity =
    Cmd "set-ghc-verbosity" $ reqArg "level" $ setGHCVerbosity

cmdBackgroundTypecheckFile :: Cmd
cmdBackgroundTypecheckFile =
    Cmd "background-typecheck-file" $ reqArg' "file" S.unpack $ cmd
  where cmd fname = backgroundTypecheckFile fname

cmdBackgroundTypecheckArbitrary :: Cmd
cmdBackgroundTypecheckArbitrary =
    Cmd "background-typecheck-arbitrary" $
        reqArg' "file" S.unpack <&>
        docContentsArg $ cmd
  where cmd fname contents = backgroundTypecheckArbitrary fname contents

cmdForceUnload :: Cmd
cmdForceUnload = Cmd "force-unload" $ noArgs $ unload

cmdAddCmdLineFlag :: Cmd
cmdAddCmdLineFlag =
    Cmd "add-command-line-flag" $
      optArg' "flag" "" S.unpack <&>
      optArg' "flags" [] (map S.unpack) $ cmd
  where cmd flag flags' = do
          addCmdLineFlags $ (if flag == "" then [] else [flag]) ++ flags'
          return JSNull

cmdThingAtPoint :: Cmd
cmdThingAtPoint =
    Cmd "thing-at-point" $
      fileNameArg <&> lineColumnArgs <&> optArg' "qualify" False decodeBool <&> optArg' "typed" True decodeBool $ cmd
  where
    cmd fname line col qual typed= do
      let loc = srcLocSpan $ mkSrcLoc (fsLit fname) line col
      tc_res <- gets bgTcCache
      case tc_res of
        Just (Typechecked tcm) -> do
             let f=(if typed then (doThingAtPointTyped $ typecheckedSource tcm) else (doThingAtPointUntyped $ renamedSource tcm))
             --tap<- doThingAtPoint loc qual typed tcm (if typed then (typecheckedSource tcm) else (renamedSource tcm))
             tap<-f loc qual tcm
                --(if typed then (doThingAtPointTyped $ typecheckedSource tcm)
               -- else doThingAtPointTyped (renamedSource tcm) loc qual tcm
             return $ Just tap
        _ -> return Nothing
    doThingAtPointTyped :: Search Id a => a ->  SrcSpan -> Bool -> TypecheckedModule -> ScionM String
    doThingAtPointTyped src loc qual tcm=do
            let in_range = overlaps loc
            let r = findHsThing in_range src
            unqual <- if qual
                then return $ O.alwaysQualify
                else unqualifiedForModule tcm
            return $ case pathToDeepest r of
              Nothing -> "no info"
              Just (x,xs) ->
                case typeOf (x,xs) of
                  Just t ->
                      showSDocForUser unqual
                        (prettyResult x O.<+> dcolon O.<+>
                          pprTypeForUser True t)
                  _ -> showSDocForUser unqual (prettyResult x) --(Just (showSDocDebug (ppr x O.$$ ppr xs )))
    doThingAtPointUntyped :: (Search id a, OutputableBndr id) => a -> SrcSpan -> Bool -> TypecheckedModule  -> ScionM String
    doThingAtPointUntyped src loc qual tcm =do
            let in_range = overlaps loc
            let r = findHsThing in_range src
            unqual <- if qual
                then return $ O.neverQualify
                else unqualifiedForModule tcm
            return $ case pathToDeepest r of
              Nothing -> "no info"
              Just (x,_) ->
                if qual
                        then showSDocForUser unqual ((qualifiedResult x) O.<+> (O.text $ haddockType x))
                        else showSDocForUser unqual ((prettyResult x) O.<+> (O.text $ haddockType x))

cmdToplevelNames :: Cmd
cmdToplevelNames=
     Cmd "top-level-names" $ noArgs $ cmd
  where
    cmd =do
    tc_res <- gets bgTcCache
    case tc_res of
      Just m -> do
          return $ map showSDocDump $ toplevelNames m
      _ -> return []

cmdOutline :: Cmd
cmdOutline =
    Cmd "outline" $  optArg' "trimFile" True decodeBool $ cmd
 where
  cmd trim = do
    root_dir <- projectRootDir
    tc_res <- gets bgTcCache
    case tc_res of
      Just m -> do
        let f = if trim then trimLocationFile else id
        return $ f $ outline root_dir m
      _ -> return []

cmdTokens :: Cmd
cmdTokens =
     Cmd "tokens" $ docContentsArg cmd
  where cmd contents = do
          root_dir <- projectRootDir
          tokensArbitrary root_dir contents

cmdTokenAtPoint :: Cmd
cmdTokenAtPoint =
  Cmd "token-at-point" $ cmdArgs tokenAtPoint
  where cmdArgs = docContentsArg <&> lineColumnArgs <&> literateFlagOpt
        tokenAtPoint contents line column literate =
          projectRootDir
          >>= (\rootDir -> tokenArbitraryAtPoint rootDir contents line column literate)

cmdTokenPreceding :: Cmd
cmdTokenPreceding =
  Cmd "token-preceding" $ cmdArgs tokenPreceding
  where cmdArgs = docContentsArg <&> lineColumnArgs <&> literateFlagOpt
        -- tokPrecWork :: String -> Int -> Int -> Bool -> ScionM (Either Note TokenDef)
        tokenPreceding contents line column literate =
          projectRootDir
          >>= (\rootDir -> tokenArbitraryPreceding rootDir contents line column literate)

cmdTokenTypes :: Cmd
cmdTokenTypes =
     Cmd "token-types" $ docContentsArg <&> literateFlagOpt $ cmd
  where cmd contents literate= do
          root_dir <- projectRootDir
          tokenTypesArbitrary root_dir contents literate
          {--mb_modsum <- filePathToProjectModule fname
          case mb_modsum of
            Nothing -> do
              return $ Left "Could not find file in module graph."
            Just modsum -> do
                ts<-tokens root_dir $ ms_mod modsum
                return $ Right ts--}

cmdDumpSources :: Cmd
cmdDumpSources = Cmd "dump-sources" $ noArgs $ cmd
  where
    cmd = do
      tc_res <- gets bgTcCache
      case tc_res of
        Just (Typechecked tcm)
         | Just rn <- renamedSourceGroup `fmap` renamedSource tcm ->
          do let tc = typecheckedSource tcm
             liftIO $ putStrLn $ showSDocDump $ ppr rn
             liftIO $ putStrLn $ showData TypeChecker 2 tc
             return ()
        _ -> return ()

cmdLoad :: Cmd
cmdLoad = Cmd "load" $ reqArg "component" <&>
    optArg "options" defaultLoadOptions $ cmd
  where
    cmd comp options= do
      loadComponent' comp options

cmdSetVerbosity :: Cmd
cmdSetVerbosity =
    Cmd "set-verbosity" $ reqArg "level" $ cmd
  where cmd v = setVerbosity (intToVerbosity v)

cmdGetVerbosity :: Cmd
cmdGetVerbosity = Cmd "get-verbosity" $ noArgs $ verbosityToInt <$> getVerbosity

-- rename to GetCurrentComponent?
cmdCurrentComponent :: Cmd
cmdCurrentComponent = Cmd "current-component" $ noArgs $ getActiveComponent

cmdDumpDefinedNames :: Cmd
cmdDumpDefinedNames = Cmd "dump-defined-names" $ noArgs $ cmd
  where
    cmd = do db <- gets defSiteDB
             liftIO $ putStrLn $ dumpDefSiteDB db

cmdDefinedNames :: Cmd
cmdDefinedNames = Cmd "defined-names" $ noArgs $ cmd
  where cmd = definedNames <$> gets defSiteDB

cmdNameDefinitions :: Cmd
cmdNameDefinitions =
    Cmd "name-definitions" $ reqArg' "name" S.unpack $ cmd
  where cmd nm = do
          db <- gets defSiteDB
          let nms=comps nm
          return $ map fst
                $ filter (\(_,b)->nm == showSDocForUser alwaysQualify (ppr $ getName b))
                $ lookupDefSite db (last nms)
        comps                   :: String -> [String]
        comps s                 =  case dropWhile ('.'==) s of
                                "" -> []
                                s' -> w : comps s''
                                      where (w, s'') =
                                             break ('.'==) s'



cmdIdentify :: Cmd
cmdIdentify =
    Cmd "client-identify" $ reqArg' "name" S.unpack $ cmd
  where cmd c = modifySessionState $ \s -> s { client = c }

cmdDumpModuleGraph :: Cmd
cmdDumpModuleGraph =
   Cmd "dump-module-graph" $ noArgs $ cmd
  where
    cmd = do
      mg <- getModuleGraph
      liftIO $ printDump (ppr mg)
      return ()

cmdModuleGraph :: Cmd
cmdModuleGraph =
   Cmd "module-graph" $ noArgs $ cmd
  where
    cmd = do
      mg <- getModuleGraph
      return $ map (showSDoc . ppr . moduleName . ms_mod) mg

cmdDumpNameDB :: Cmd
cmdDumpNameDB =
  Cmd "dump-name-db" $ noArgs $ buildNameDB >>= dumpNameDB >> return ()

-- | Get the type names for the current source in the background typecheck cache,
-- both local and imported from modules.
cmdTypeNames :: Cmd
cmdTypeNames =
  Cmd "type-names" $ noArgs $ gets bgTcCache >>= getModuleTypes
  where
    getModuleTypes (Just (Typechecked tcm)) = return $ localTcmTypes tcm
    getModuleTypes (Just (Parsed pm)) = return $ localPmTypes pm
    getModuleTypes Nothing = return [("","")]
    -- Types local to the current source
    localTcmTypes tcm = map ((formatInfo (getTcmModuleName tcm)) . unLoc) $ typeDecls tcm
    localPmTypes pm   = map (formatInfo (getModuleName pm)) $ typeDeclsParsed pm
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

cmdNamesInScope :: Cmd
cmdNamesInScope = Cmd "names-in-scope" $ noArgs $ cmd
  where
    cmd = do
      tc_res <- gets bgTcCache
      case tc_res of
        Just (Typechecked tcm) ->
          let thisModSum = (pm_mod_summary . tm_parsed_module) tcm
              thisMod = ms_mod thisModSum
              innerImports = map unLoc $ ms_imps thisModSum
              innerModNames = map (unLoc . ideclName) innerImports
              getInnerModules = mapM (\m -> lookupModule m Nothing) innerModNames
          in  getInnerModules
              >>= (\innerMods -> return $ map (showSDoc . pprModule) (thisMod:innerMods))
        Just (Parsed _) -> return ["!!wombat!!"]
        Nothing -> return $ [""]
