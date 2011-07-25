{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
-- |
-- Module      : Scion.Types
-- Copyright   : (c) Thomas Schilling 2008
-- License     : BSD-style
--
-- Maintainer  : nominolo@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Types used throughout Scion. 
--
module Scion.Types 
  ( module Scion.Types
  , liftIO, MonadIO
  ) where

import Prelude hiding ( log )
import Scion.Types.Notes
import Scion.Types.ExtraInstances()
import qualified Scion.Types.JSONDictionary as Dic

import GHC
import HscTypes
import IfaceSyn
import MonadUtils ( MonadIO )
import Exception
import Outputable

import Text.JSON.AttoJSON

import qualified Data.ByteString.Char8 as S
import qualified Data.Map as Map
import qualified Data.MultiSet as MS
-- NOTE: GHC says that this is redundant, but I still want it imported as Set
import qualified Data.Set as Set
import qualified Data.Foldable as Fold

#if CABAL_VERSION > 110
import Distribution.Simple.LocalBuildInfo hiding ( Component(..) )
#else
import Distribution.Simple.LocalBuildInfo
#endif

import System.Directory
  ( setCurrentDirectory
  , getCurrentDirectory
  , getModificationTime
  )
import System.FilePath ( normalise, (</>), dropFileName )
import System.Time
import qualified System.Log.Logger as HL
import Control.Monad ( when )
import Data.IORef
import Data.Monoid
import Data.Time.Clock  ( NominalDiffTime )
import Data.Typeable
import Control.Applicative

------------------------------------------------------------------------------
-- * The Scion Monad and Session State

-- XXX: Can we get rid of some of this maybe stuff?
-- YYY: Maybe. :-)

data SessionState 
  = SessionState {
      scionVerbosity :: Verbosity,
        -- ^ Current server verbosity, defaults to 'NORMAL'. (NOTE: Should this be tied to the System.Log
        -- verbosity in some way?)
        
      initialDynFlags :: DynFlags,
        -- ^ The DynFlags as they were when Scion was started.  This is used
        -- to reset flags when opening a new project.  Arguably, the GHC API
        -- should provide calls to reset a session.

      localBuildInfo :: Maybe LocalBuildInfo,
        -- ^ Build info from current Cabal project.

      activeComponent :: Maybe Component,
        -- ^ The current active Cabal component.  This affects DynFlags and
        -- targets.  ATM, we don't support multiple active components.

      lastCompResult :: CompilationResult,

      focusedModule :: Maybe ModSummary,
        -- ^ The currently focused module for background typechecking.

      bgTcCache :: Maybe BgTcCache,
        -- ^ Cached state of the background typechecker.

      defSiteDB :: DefSiteDB,
        -- ^ Source code locations.
        
      moduleCache :: ModuleCache,
        -- ^ Module and name cache to support IDE completions. We cache both package modules and
        -- home modules. For home modules, the cached data represents "last known good"
        -- state of a module so that we have something to give the user; GHC is very unforgiving
        -- if a module doesn't parse or typecheck correctly.

      client :: String,
        -- ^ can be set by the client. Only used by vim to enable special hack
        
      userFlags :: [(String,Bool)]
        -- ^ special assignment of Cabal flags, may influence which components are buildable, etc...
    }

mkSessionState :: DynFlags -> IO (IORef SessionState)
mkSessionState dflags =
    newIORef (SessionState normal dflags Nothing Nothing mempty Nothing Nothing mempty emptyModuleCache "" mempty)

newtype ScionM a
  = ScionM { unScionM :: IORef SessionState -> Ghc a }

instance Monad ScionM where
  return x = ScionM $ \_ -> return x
  (ScionM ma) >>= fb = 
      ScionM $ \s -> do
        a <- ma s 
        unScionM (fb a) s
  fail msg = dieHard msg

instance Functor ScionM where
  fmap f (ScionM ma) =
      ScionM $ \s -> fmap f (ma s)

instance Applicative ScionM where
  pure a = ScionM $ \_ -> return a
  ScionM mf <*> ScionM ma = 
      ScionM $ \s -> do f <- mf s; a <- ma s; return (f a)

liftScionM :: Ghc a -> ScionM a
liftScionM m = ScionM $ \_ -> m

instance MonadIO ScionM where
  liftIO m = liftScionM $ liftIO m

instance ExceptionMonad ScionM where
  gcatch (ScionM act) handler =
      ScionM $ \s -> act s `gcatch` (\e -> unScionM (handler e) s)
  gblock (ScionM act) = ScionM $ \s -> gblock (act s)
  gunblock (ScionM act) = ScionM $ \s -> gunblock (act s)

instance WarnLogMonad ScionM where
  setWarnings = liftScionM . setWarnings
  getWarnings = liftScionM getWarnings

instance GhcMonad ScionM where
  getSession = liftScionM getSession
  setSession = liftScionM . setSession

-- | Modify scion's current session state by rewriting the underlying IORef.
modifySessionState :: (SessionState -> SessionState)  -- ^ Session state modification function
                   -> ScionM ()                       -- ^ Result
modifySessionState modFunc = ScionM $ \r -> liftIO $ readIORef r >>= (\s -> writeIORef r $! modFunc s)

-- | Get selector from SessionState record, e.g., getSessionSelector projectRoot
getSessionSelector :: forall a. (SessionState -> a) -> ScionM a
getSessionSelector sel = withSessionState (return . sel)

-- | Do something using the current session state
withSessionState :: forall a. (SessionState -> ScionM a) -> ScionM a
withSessionState sFunc = ScionM (\s -> liftIO $ readIORef s) >>= sFunc 

setSessionState :: SessionState -> ScionM ()
setSessionState s' = ScionM $ \r -> liftIO $ writeIORef r s'

------------------------------------------------------------------------------
-- ** Verbosity Levels

data Verbosity
  = Silent
  | Normal
  | Verbose
  | Deafening
  deriving (Eq, Ord, Show, Enum, Bounded)

intToVerbosity :: Int -> Verbosity
intToVerbosity n
  | n < 0                                = minBound
  | n > fromEnum (maxBound :: Verbosity) = maxBound
  | otherwise                            = toEnum n

verbosityToInt :: Verbosity -> Int
verbosityToInt = fromEnum

silent :: Verbosity
silent = Silent

normal :: Verbosity
normal = Normal

verbose :: Verbosity
verbose = Verbose

deafening :: Verbosity
deafening = Deafening

getVerbosity :: ScionM Verbosity
getVerbosity = getSessionSelector scionVerbosity

setVerbosity :: Verbosity -> ScionM ()
setVerbosity v = modifySessionState $ \s -> s { scionVerbosity = v }

message :: Verbosity -> String -> ScionM ()
message v s = do
  v0 <- getVerbosity
  when (v0 >= v) $ liftIO $ logInfo s

log :: HL.Priority -> String -> IO()
log = HL.logM __FILE__

logInfo, logDebug, logWarn, logError :: String -> IO()
logInfo = log HL.INFO
logDebug = log HL.DEBUG
logWarn = log HL.WARNING
logError = log HL.ERROR

------------------------------------------------------------------------
-- * Reflection into IO

-- | Reflect a computation in the 'ScionM' monad into the 'IO' monad.
reflectScionM :: ScionM a -> (IORef SessionState, Session) -> IO a
reflectScionM (ScionM f) = \(st, sess) -> reflectGhc (f st) sess

-- | Dual to 'reflectScionM'.  See its documentation.
reifyScionM :: ((IORef SessionState, Session) -> IO a) -> ScionM a
reifyScionM act = ScionM $ \st -> reifyGhc $ \sess -> act (st, sess)

------------------------------------------------------------------------------
-- * Compilation Results

data BgTcCache
  = Parsed ParsedModule
  | Typechecked TypecheckedModule

data CompilationResult = CompilationResult { 
      compilationSucceeded :: Bool,
      compilationNotes     :: MS.MultiSet Note,
      compilationTime      :: NominalDiffTime
    }

instance Monoid CompilationResult where
  mempty = CompilationResult True mempty 0
  mappend r1 r2 =
      CompilationResult 
        { compilationSucceeded = 
              compilationSucceeded r1 && compilationSucceeded r2
        , compilationNotes =
            compilationNotes r1 `MS.union` compilationNotes r2
        , compilationTime = compilationTime r1 + compilationTime r2
        }

------------------------------------------------------------------------------
-- * Exceptions

-- | Any exception raised inside Scion is a subtype of this exception.
data SomeScionException
  = forall e. (Exception e) => SomeScionException e
  deriving Typeable

instance Show SomeScionException where show (SomeScionException e) = show e
instance Exception SomeScionException

scionToException :: Exception e => e -> SomeException
scionToException = toException . SomeScionException

scionFromException :: Exception e => SomeException -> Maybe e
scionFromException x = do
  SomeScionException e <- fromException x
  cast e

-- | A fatal error.  Like 'error' but suggests submitting a bug report.
dieHard :: String -> a
dieHard last_wish = do
   error $ "************** Panic **************\n" ++ 
              last_wish ++ 
              "\nPlease file a bug report at:\n  " ++ bug_tracker_url
  where
    bug_tracker_url = "http://code.google.com/p/scion-lib/issues/list"

------------------------------------------------------------------------------
-- * Others \/ Helpers

-- | Shorthand for 'undefined'.
__ :: a
__ = undefined

-- * Go To Definition

-- | A definition site database.
--
-- This is a map from names to the location of their definition and
-- information about the defined entity.  Note that a name may refer to
-- multiple entities.
--
-- XXX: Currently we use GHC's 'TyThing' data type. However, this probably
-- holds on to a lot of stuff we don't need.  It also cannot be serialised
-- directly.  The reason it's done this way is that wrapping 'TyThing' leads
-- to a lot of duplicated code.  Using a custom type might be useful to have
-- fewer dependencies on the GHC API; however it also creates problems
-- mapping things back into GHC API data structures.  If we do this, we
-- should at least remember the 'Unique' in order to quickly look up the
-- original thing.
newtype DefSiteDB =
  DefSiteDB (Map.Map String [(Location,TyThing)])

instance Monoid DefSiteDB where
  mempty = emptyDefSiteDB
  mappend = unionDefSiteDB

-- | The empty 'DefSiteDB'.
emptyDefSiteDB :: DefSiteDB
emptyDefSiteDB = DefSiteDB Map.empty

-- | Combine two 'DefSiteDB's.   XXX: check for duplicates?
unionDefSiteDB :: DefSiteDB -> DefSiteDB -> DefSiteDB
unionDefSiteDB (DefSiteDB m1) (DefSiteDB m2) =
    DefSiteDB (Map.unionWith (++) m1 m2)

-- | Return the list of defined names (the domain) of the 'DefSiteDB'.
-- The result is, in fact, ordered.
definedNames :: DefSiteDB -> [String]
definedNames (DefSiteDB m) = Map.keys m

-- | Returns all the entities that the given name may refer to.
lookupDefSite :: DefSiteDB -> String -> [(Location, TyThing)]
lookupDefSite (DefSiteDB m) key =
  case Map.lookup key m of
    Nothing -> []
    Just xs -> xs


-- use this exception for everything else which is not important enough to
-- create a new Exception (kiss) 
-- some more Exception types are defined in Session.hs (TODO?)
data ScionError = ScionError String
     deriving (Show, Typeable)
instance Exception ScionError where
  toException  = scionToException
  fromException = scionFromException
scionError :: String -> ScionM a
scionError = liftIO . throwIO . ScionError

-- will be extended in the future
data CabalConfiguration = CabalConfiguration {
    distDir :: FilePath,
    extraArgs :: [String] -- additional args used to configure the project 
  }

type FileComponentConfiguration =
  ( FilePath, -- rel filepath to config file
    [String] -- set of flags to be used to compile that file  
  )

-- the ScionProjectConfig is a project specific configuration file 
-- The syntax must be simple and human readable. One JSON object per line.
-- Example:
-- { 'type' : 'build-configuration', 'dist-dir' : 'dist-custom', 'extra-args' : [ ] }
-- helperf functions see Utils.hs 
data ScionProjectConfig = ScionProjectConfig {
  buildConfigurations :: [CabalConfiguration],
  fileComponentExtraFlags :: [FileComponentConfiguration],
  scionDefaultCabalConfig :: Maybe String
  }

emptyScionProjectConfig :: ScionProjectConfig
emptyScionProjectConfig = ScionProjectConfig [] [] Nothing

----------------------------------------------------------------------
-- | Sets the current working directory and notifies GHC about the
-- change.
-- 
-- TODO: do we want to adjust certain flags automatically?
setWorkingDir :: FilePath -> ScionM ()
setWorkingDir home = do
  cwd <- liftIO $ getCurrentDirectory
  message deafening $ "Setting working directory: " ++ home ++ " (old: " ++ cwd ++ ")"
  liftIO $ setCurrentDirectory home
  cwd' <- liftIO $ getCurrentDirectory -- to avoid normalisation issues
  when (cwd /= cwd') $ do
    message deafening $ "(Working directory changed.)"
    workingDirectoryChanged



class (Show c, Eq c, JSON c) => IsComponent c where
  componentInit    :: c -> ScionM (Maybe String) --error msg
  componentTargets :: c -> ScionM [Target]
  componentOptions :: c -> ScionM [String]
  componentClean :: c -> ScionM ()
 
  componentInit _ = return Nothing
  componentClean _ = return ()

data Component = forall c. IsComponent c => Component c

instance Eq Component where
  Component c1 == Component c2 = show c1 == show c2

instance Show Component where
  show (Component c) = show c

data FileComp = FileComp FilePath deriving (Eq, Show)

instance IsComponent FileComp where
  componentInit (FileComp f) = do
    wd <- liftIO $ getCurrentDirectory
    let dir = normalise $ wd </> dropFileName f
    setWorkingDir dir
    return Nothing

  componentTargets (FileComp f) = do
    return [ Target (TargetFile f Nothing)
                    True
                    Nothing ]
  
  componentOptions (FileComp _f) = do
    --cfg <- io getCurrentDirectory
    -- liftM projectConfigFileFromDir $ 
    --config <- parseScionProjectConfig cfg
    return []
    -- let config = []
    -- return $ fromMaybe [] $ 
    --   lookup (takeFileName f) [] --(fileComponentExtraFlags config)

instance JSON FileComp where
  fromJSON obj@(JSObject _)
    | Just (JSString s) <- Dic.lookupKey obj Dic.file =
        return $ FileComp (S.unpack s)
    | otherwise = fail "file slot missing"
  fromJSON j = fail $ "filecomp not an object" ++ show j
  toJSON (FileComp n) =
      Dic.makeObject [(Dic.file, JSString (S.pack n))]

defaultLoadOptions :: LoadOptions
defaultLoadOptions=LoadOptions False False

data LoadOptions=LoadOptions {
        lo_output :: Bool,
        lo_forcerecomp :: Bool
        }
        deriving (Show,Read)
        
instance JSON LoadOptions where
   fromJSON obj@(JSObject _)
     | Just (JSBool ob)  <- Dic.lookupKey obj Dic.output,
       Just (JSBool rb)  <- Dic.lookupKey obj Dic.forcerecomp
         = return $ LoadOptions ob rb
   fromJSON j = fail $ "LoadOptions not an object" ++ show j    
   toJSON (LoadOptions ob rb) =
      Dic.makeObject [(Dic.output, JSBool ob),(Dic.forcerecomp, JSBool rb)]    

-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=
-- Module cache
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=

-- * Module cache and IDE completion

-- | The module cache used to generate IDE completions.
--
-- NOTE: System.Time is deprecated, yet System.Directory still uses it.

-- | The module cache is a map indexed by modules storing module symbol data extracted from
-- the Haskell interface file
type ModuleCache = Map.Map Module ModCacheData

-- | Name to module symbol data associations
data ModCacheData = 
  ModCacheData {
    lastModTime :: ClockTime         -- ^ Last modified time for Haskell interface files 
  , modSymData  :: ModSymData           -- ^ Module symbol data
  , importDecls :: [ImportDecl RdrName] -- ^ Import declarations for home modules
  , tyCons      :: CompletionTuples     -- ^ Last known good type constructor completion tuples
  }

-- | Associations between symbol name and declaration data
type ModSymData = Map.Map RdrName ModSymDecls
-- | Sequence of declaration data
type ModSymDecls = Set.Set ModDecl
-- | Declaration data (note: would have like to use GHC's IfaceDecl here, but need
-- to extract information inside the IfaceDecl to populate additional 'ModSymData'
-- associations.)
data ModDecl =
    MIdDecl
  | MTypeDecl  IfaceDecl
  | MConDecl   IfaceConDecl
  | MClassDecl IfaceDecl
  | MClassOp   IfaceClassOp

instance Eq ModDecl where
  MIdDecl        == MIdDecl          = True
  (MTypeDecl a)  == (MTypeDecl b)    = (ifName a) == (ifName b)
  (MConDecl  a)  == (MConDecl  b)    = (ifConOcc a) == (ifConOcc b)
  (MClassDecl a) == (MClassDecl b)   = (ifName a) == (ifName b)
  (MClassOp (IfaceClassOp a _ _)) == (MClassOp (IfaceClassOp b _ _)) = (a == b)
  _ == _ = False
  
instance Ord ModDecl where
  compare MIdDecl MIdDecl = EQ
  compare MIdDecl _       = GT
  compare (MTypeDecl _) MIdDecl       = GT
  compare (MTypeDecl a) (MTypeDecl b) = (ifName a) `compare` (ifName b)
  compare (MTypeDecl _) _             = LT
  compare (MConDecl _)  MIdDecl       = GT
  compare (MConDecl _)  (MTypeDecl _) = GT
  compare (MConDecl a)  (MConDecl b)  = (ifConOcc a) `compare` (ifConOcc b)
  compare (MConDecl _)  _             = LT
  compare (MClassDecl _) MIdDecl        = GT
  compare (MClassDecl _) (MTypeDecl _)  = GT
  compare (MClassDecl _) (MConDecl _)   = GT
  compare (MClassDecl a) (MClassDecl b) = (ifName a) `compare` (ifName b)
  compare (MClassDecl _) (MClassOp _)   = LT
  compare (MClassOp (IfaceClassOp a _ _)) (MClassOp (IfaceClassOp b _ _)) = a `compare` b
  compare (MClassOp _) _ = GT
  
instance Show ModDecl where
  show MIdDecl = "MIdDecl"
  show (MTypeDecl a) = showSDoc $ text "MTypeDecl" <+> ppr (ifName a)
  show (MConDecl a) = showSDoc $ text "MConDecl" <+> ppr (ifConOcc a)
  show (MClassDecl a) = showSDoc $ text "MClassDecl" <+> ppr (ifName a)
  show (MClassOp (IfaceClassOp a _ _)) = showSDoc $ text "MClassOp" <+> ppr a 
  
type CompletionTuples = [(String, String)]
  
emptyModuleCache :: ModuleCache
emptyModuleCache = Map.empty

emptyModCacheData :: ModCacheData
emptyModCacheData =
  ModCacheData {
    lastModTime = TOD 0 0
  , modSymData  = Map.empty
  , importDecls = []
  , tyCons      = []
  }

-- | Make a new module cache record
mkModCacheData :: FilePath -> ModSymData -> IO ModCacheData
mkModCacheData fpath msymData =do
        mt<-getModificationTime fpath
        return $
          ModCacheData {
            lastModTime = mt
          , modSymData  = msymData
          , importDecls = []
          , tyCons      = []
          }

moduleCacheSize :: ModuleCache -> Int
moduleCacheSize mc=foldr (\(ModCacheData _ msd ids tc) cnt->cnt+(sz msd)+(length ids)+(length tc)) 0 (Map.elems mc)
        where sz msd=foldr (\s cnt2->cnt2+(Set.size s)) 0 (Map.elems msd)
-- Various predicates for 'ModDeclSymbols'

-- | Does the mod declaration set have a 'MTypeDecl'?
hasMTypeDecl :: ModSymDecls
             -> Bool
hasMTypeDecl decls =
  let hasMTypeDecl' (MTypeDecl _) = True
      hasMTypeDecl' _             = False
  in Fold.and $ Set.map hasMTypeDecl' decls

-- | Does the mod declarations have a 'MIdDecl'?
hasMIdDecl :: ModSymDecls
           -> Bool
hasMIdDecl decls =
  let hasMIdDecl' MIdDecl = True
      hasMIdDecl' _       = False
  in  Fold.and $ Set.map hasMIdDecl' decls

-- | Does the mod declaration set have a 'MClassDecl'?
hasMClassDecl :: ModSymDecls
              -> Bool
hasMClassDecl decls =
  let hasMClassDecl' (MClassDecl _) = True
      hasMClassDecl' _              = False
  in  Fold.and $ Set.map hasMClassDecl' decls
