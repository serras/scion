{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, CPP#-}
-- |
-- Module      : Scion.Cabal
-- Copyright   : (c) Thomas Schilling 2008
-- License     : BSD-style
-- 
-- Maintainer  : nominolo@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- Cabal-related functionality.
-- 
module Scion.Cabal 
  ( CabalComponent(..), CabalPackage(..),
    scionDistDir, cabalProjectComponents, cabalParse, cabalParseArbitrary, cabalDependencies,
    cabalConfigurations, preprocessPackage, dependencies, cabalClean )
  where

import Scion.Types
import Scion.Packages

import Exception
import GHC hiding ( load, TyCon )
import GHC.Paths  ( ghc, ghc_pkg )
import Data.Typeable ()

import Text.JSON.AttoJSON (JSON,JSValue(..),fromJSON,toJSON)
import qualified Data.ByteString.Char8 as S
import qualified Scion.Types.JSONDictionary as Dic

import Control.Monad
import Data.Data

import Data.Function (on)
import Data.List        ( intercalate,sortBy,partition )
import Data.Maybe
import qualified Data.Map as DM
import System.Directory ( doesFileExist, getDirectoryContents,
                          getModificationTime, removeFile )
import System.FilePath ( (</>), dropFileName, takeExtension, dropExtension, (<.>) )
import System.Exit ( ExitCode(..) )

import qualified Distribution.ModuleName as PD
                       ( ModuleName, components )
-- FIXME: unused import Distribution.Simple.Configure
import Distribution.Simple.GHC ( ghcOptions )
#if CABAL_VERSION > 110
import Distribution.Simple.LocalBuildInfo hiding ( libdir, Component(..) )
#else
import Distribution.Simple.LocalBuildInfo hiding ( libdir )
#endif
import Distribution.Simple.Build ( initialBuildSteps )
import Distribution.Simple.BuildPaths ( exeExtension )
#if CABAL_VERSION < 112
import Distribution.Simple.PreProcess ( knownSuffixHandlers )
#endif
import qualified Distribution.PackageDescription as PD
import Distribution.Package
import Distribution.InstalledPackageInfo
import Distribution.Version

import qualified Distribution.PackageDescription.Parse as PD

import Distribution.Simple.Configure
import Distribution.PackageDescription.Parse
                        ( readPackageDescription )
import qualified Distribution.Verbosity as V 
                        ( normal, deafening, silent )
import Distribution.Simple.Program 
                        ( defaultProgramConfiguration,
                          userSpecifyPaths )
import Distribution.Simple.Setup 
                        ( defaultConfigFlags, ConfigFlags(..),
                          Flag(..) )
import Distribution.Text

-- * Exception Types
-- also see ScionError Exception 

data CannotOpenCabalProject = CannotOpenCabalProject String
     deriving (Show, Typeable)
instance Exception CannotOpenCabalProject where
  toException  = scionToException
  fromException = scionFromException

data CannotListPackages = CannotListPackages String
     deriving (Show, Typeable)
instance Exception CannotListPackages where
  toException  = scionToException
  fromException = scionFromException

data CabalComponent
  = Library {cabalFile :: FilePath
        , buildable :: Bool}
  | Executable {cabalFile :: FilePath
        , exe_name :: String
        , buildable :: Bool}
  | TestSuite {cabalFile :: FilePath
        , test_name :: String
        , buildable :: Bool}      
  deriving (Eq, Show)

data CabalPackage=CabalPackage {
        cp_name::String,
        cp_version::String,
        cp_exposed::Bool,
        cp_dependent::[CabalComponent],
        cp_exposedModules::[String]
        }
   deriving (Eq, Show)

instance IsComponent CabalComponent where

  componentInit = cabalComponentInit
  componentTargets = cabalTargets
  componentOptions = cabalDynFlags
  componentClean = cabalClean . cabalFile    

scionDistDir :: FilePath
scionDistDir = ".dist-scion"

-- | Set up a Cabal component, (re-)configuring it if necessary.
--
-- Checks whether an existing configuration result exists on disk and
-- configures the project if not.  Similarly, if the existing config
-- is outdated the project is reconfigured.
--
cabalComponentInit :: CabalComponent -> ScionM (Maybe String)
cabalComponentInit c = do
  -- TODO: verify that components exist in cabal file
  let cabal_file = cabalFile c
  r<-cabalInit cabal_file
  return $ case r of
        Left err-> Just err
        Right _ ->Nothing
  
cabalClean :: FilePath -> ScionM ()
cabalClean cabal_file = do
  ok <- liftIO $ doesFileExist cabal_file
  when ok (do
     let root_dir = dropFileName cabal_file
     let setup_config = localBuildInfoFile (root_dir </> scionDistDir)
     liftIO $ removeFile setup_config
      )
  
cabalInit :: FilePath -> ScionM (Either String LocalBuildInfo)  
cabalInit cabal_file = do
  ok <- liftIO $ doesFileExist cabal_file
  if not ok then return (Left ".cabal file does not exist") else do
   let root_dir = dropFileName cabal_file
   let setup_config = localBuildInfoFile (root_dir </> scionDistDir)
   conf'd <- liftIO $ doesFileExist setup_config
   if not conf'd then do
      message deafening "Configuring: for first time" 
      do_configure root_dir else do
     cabal_time <- liftIO $ getModificationTime cabal_file
     conf_time <- liftIO $ getModificationTime setup_config
     if cabal_time >= conf_time then do
       message deafening "Reconfiguring: .cabal file is newer"
       do_configure root_dir
      else do
        mb_lbi <- liftIO $ maybeGetPersistBuildConfig 
                         (root_dir </> scionDistDir)
        case mb_lbi of
          Nothing -> do
            message deafening "Reconfiguring: Cabal says so"
            do_configure root_dir
          Just _lbi -> do
            setWorkingDir root_dir
            return $ Right _lbi
 where
   do_configure root_dir = do
     r <- gtry $ configureCabalProject root_dir scionDistDir
     case r of
       Left (err :: IOException) -> return (Left (show err))
       Right lbi -> return $ Right lbi

withCabal :: FilePath -> (LocalBuildInfo -> ScionM (a))-> ScionM (Either String a)  
withCabal cabal_file f=do
        r<-cabalInit cabal_file
        case r of
                Left err-> return $ Left err
                Right lbi ->liftM Right (f lbi)
                

--cabalFile :: CabalComponent -> FilePath
--cabalFile (Library f) = f
--cabalFile (Executable f _) = f

-- | Return GHC 'Target's corresponding to this component.
cabalTargets :: CabalComponent -> ScionM [Target]
cabalTargets Library{cabalFile=f} = do
  pd <- cabal_package f
#if CABAL_VERSION < 107
  let modnames = PD.libModules pd
#else
  let modnames | Just lib <- PD.library pd = PD.libModules lib
               | otherwise = []
#endif
  return (map cabalModuleNameToTarget modnames)
cabalTargets Executable{exe_name=name,cabalFile=f} = do
  pd <- cabal_package f
  let ex0 = filter ((name==) . PD.exeName) (PD.executables pd)
  case ex0 of
    [] -> error $ "cabalTargets no executable with name: " ++ name --noExeError n
    (_:_:_) -> error $ "Multiple executables with name: " ++ name
    [exe] -> do
      let proj_root = dropFileName f
      let others = PD.otherModules (PD.buildInfo exe)
      let main_mods =
              [ (if (search_path /= ".") then proj_root </> search_path else proj_root) </> PD.modulePath exe  
              | search_path <- PD.hsSourceDirs (PD.buildInfo exe)]
      existing_main_mods <- filterM (liftIO . doesFileExist) main_mods
      let targets = map (\main_mod -> Target (TargetFile main_mod Nothing) True Nothing) (take 1 existing_main_mods)
             ++
              map cabalModuleNameToTarget others
      return targets
#if CABAL_VERSION > 108
cabalTargets TestSuite{test_name=name,cabalFile=f} = do
  pd <- cabal_package f
  let ex0 = filter ((name==) . PD.testName) (PD.testSuites pd)
  case ex0 of
    [] -> error $ "cabalTargets no test-suite with name: " ++ name --noExeError n
    (_:_:_) -> error $ "Multiple test-suites with name: " ++ name
    [ts] -> do
      let proj_root = dropFileName f
      let others = PD.otherModules (PD.testBuildInfo ts)
      mainTgts<-case PD.testInterface ts of
          PD.TestSuiteExeV10 _ fp-> do
                let main_mods =
                        [ (if (search_path /= ".") then proj_root </> search_path else proj_root) </> fp
                           | search_path <- PD.hsSourceDirs (PD.testBuildInfo ts)]
                existing_main_mods <- filterM (liftIO . doesFileExist) main_mods
                return $ map (\main_mod -> Target (TargetFile main_mod Nothing) True Nothing) (take 1 existing_main_mods)
          PD.TestSuiteLibV09 _ mn->return [cabalModuleNameToTarget mn]
          PD.TestSuiteUnsupported _->return [] 
          
      return $ mainTgts ++ (map cabalModuleNameToTarget others)
#endif




cabal_package :: FilePath -> ScionM PD.PackageDescription   
cabal_package f = do
  let root_dir = dropFileName f
  lbi <- liftIO $ getPersistBuildConfig (root_dir </> scionDistDir)
  return $ localPkgDescr lbi

cabal_build_info :: FilePath -> ScionM LocalBuildInfo
cabal_build_info f = do
  let root_dir = dropFileName f
  liftIO $ getPersistBuildConfig (root_dir </> scionDistDir)

-- | Return command line flags for the component.
cabalDynFlags :: CabalComponent -> ScionM [String]
cabalDynFlags component = do
   lbi <- cabal_build_info (cabalFile component)
   bi <- component_build_info component (localPkgDescr lbi)
   let odir0 = buildDir lbi
   let odir
         | Executable {exe_name=exeName'} <- component
           = odir0 </> dropExtension exeName'
#if CABAL_VERSION > 108
         | TestSuite {test_name=testName'} <- component
           = odir0 </> dropExtension testName'
#endif
         | otherwise
           = odir0
#if CABAL_VERSION < 107
   let opts = ghcOptions lbi bi odir
#else
       clbi
         | Executable {exe_name=exeName'} <- component
           = fromJustD "executable" $ lookup exeName' (executableConfigs lbi)
#endif
#if CABAL_VERSION > 108
         | TestSuite {test_name=testName'} <- component
           = fromJustD ("testsuite:"++testName'++":"++(show $ map fst $ testSuiteConfigs lbi)) $ lookup testName' (testSuiteConfigs lbi)
#endif     
#if CABAL_VERSION >= 107      
         | otherwise
           = fromJustD "library" $ libraryConfig lbi
   let opts = ghcOptions lbi bi clbi odir
#endif
   o_file_opts <- output_file_opts odir
   return $ opts ++ o_file_opts
 where
   component_build_info Library{} pd
     | Just lib <- PD.library pd = return (PD.libBuildInfo lib)
     | otherwise                 = error "no lib" --noLibError
   component_build_info Executable{exe_name=n} pd =
     case [ exe | exe <- PD.executables pd, PD.exeName exe == n ] of
       [ exe ] -> return (PD.buildInfo exe)
       [] -> error "cabalDynFlags no exe" --noExeError n
       _ -> error $ "Multiple executables, named \"" ++ n ++ 
                    "\" found.  This is weird..."
#if CABAL_VERSION > 108
   component_build_info TestSuite{test_name=n} pd =
     case [ exe | exe <- PD.testSuites pd, PD.testName exe == n ] of
       [ exe ] -> return (PD.testBuildInfo exe)
       [] -> error "cabalDynFlags no testsuite" --noExeError n
       _ -> error $ "Multiple testsuites, named \"" ++ n ++ 
                    "\" found.  This is weird..."
#endif
   output_file_opts odir =
     case component of
       Executable{exe_name=exeName'} -> 
         return ["-o", odir </> exeName' <.> (if null $ takeExtension exeName'
                                                 then exeExtension
                                                 else "")]
#if CABAL_VERSION > 108
       TestSuite{test_name=test_name',cabalFile=f} -> do
         pd <- cabal_package f
         let ex0 = filter ((test_name' ==) . PD.testName) (PD.testSuites pd)
         case ex0 of
           [ts] -> case PD.testInterface ts of
                     PD.TestSuiteExeV10 _ _ -> return ["-o", odir </> test_name' <.>
                                                               (if null $ takeExtension test_name'
                                                                   then exeExtension
                                                                   else "")]
                     _                      -> return []
           _    -> error "Zero or more than one testsuites with the same name"
#endif
       _ -> return []

fromJustD :: [Char] -> Maybe a -> a
fromJustD msg Nothing=error msg
fromJustD _ (Just a)=a

-- | Return all components of the specified Cabal file.
-- 
-- Throws:
-- 
--  * 'CannotOpenCabalProject' if an error occurs (e.g., .cabal file
--    does not exist or could not be parsed.).
-- 
cabalProjectComponents :: FilePath -- ^ The .cabal file
                       -> ScionM (Either String [Component])
cabalProjectComponents cabal_file = do
    withCabal cabal_file (
        return . map Component . cabalComponentsFromDescription cabal_file . localPkgDescr
        )

cabalComponentsFromDescription :: FilePath -> PD.PackageDescription -> [CabalComponent]
cabalComponentsFromDescription cabal_file pd= 
      (if isJust (PD.library pd) then [Library cabal_file (PD.buildable $ PD.libBuildInfo $ fromJust (PD.library pd))] else []) ++
      [ Executable cabal_file (PD.exeName e) (PD.buildable $ PD.buildInfo e)
      | e <- PD.executables pd ]
#if CABAL_VERSION > 108
       ++ [ TestSuite cabal_file (PD.testName e) (PD.buildable $ PD.testBuildInfo e)
        | e <- PD.testSuites pd ]
#endif

cabalParse :: FilePath -> ScionM PD.GenericPackageDescription
cabalParse cabal_file = do  
  ghandle (\(_ :: ExitCode) ->
                liftIO $ throwIO $ CannotOpenCabalProject cabal_file) $ do
    gpd <- liftIO $ PD.readPackageDescription V.silent cabal_file 
    return gpd

{--runScion $ cabalDependencies "D:\\dev\\haskell\\jp-github\\runtime-New_configuration\\Haskell0\\Haskell0.cabal"--}
cabalDependencies :: FilePath -> ScionM (Either String [(FilePath,[CabalPackage])])
cabalDependencies cabal_file = 
     withCabal cabal_file (\lbi-> do
        ghandle (\(e :: IOError) ->
               liftIO $ throwIO $ CannotListPackages $ show e) $ do
            pkgs<-liftIO $ getPkgInfos
            return $ dependencies cabal_file (localPkgDescr lbi) pkgs
            )
    
    {--        
dependencies :: FilePath -> PD.GenericPackageDescription -> [(FilePath,[InstalledPackageInfo])] -> [(FilePath,[CabalPackage])]
dependencies cabal_file gpd pkgs=let
        pkgsMap=foldr buildPkgMap DM.empty pkgs -- build the map of package by name with ordered version (more recent first)
        pd = PD.flattenPackageDescription gpd
        allC= (if isJust (PD.library pd) then [Library cabal_file] else []) ++
                      [ Executable cabal_file (PD.exeName e)
                      | e <- PD.executables pd ]
                      
        gdeps=PD.buildDepends pd
            -- list of all dependencies
            -- flatten in fact pushes up all dependencies, so for the moment we can't distinguish between different components
        --deps=(maybe [] (\l->map (\d->(Library cabal_file,d)) $ PD.targetBuildDepends $ PD.libBuildInfo l) $ PD.library pd)  -- library 
              --        ++ (concatMap (\e->map (\d->(Executable cabal_file (PD.exeName e),d)) $ PD.targetBuildDepends $ PD.buildInfo e) $ PD.executables pd) -- executables
               --       ++ 
        deps= concatMap (\c->map (\d->(c,d)) gdeps) allC -- project level
        cpkgs=(map (\(fp,pkgMap)->(fp,DM.map (\ipis->getDep ipis deps []) pkgMap)) pkgsMap) :: [(FilePath,DM.Map String [CabalPackage])]
        in ((map (\(fp,cpMap)->(fp,concat $ DM.elems cpMap)) cpkgs)::[(FilePath,[CabalPackage])])
        where 
                buildPkgMap :: (String,[InstalledPackageInfo]) -> DM.Map String [(String,InstalledPackageInfo)] -> DM.Map String  [(String,InstalledPackageInfo)]
                buildPkgMap ipis=DM.map (sortBy (flip (compare `on` (pkgVersion . sourcePackageId)))) $ DM.fromListWith (++) (map (\i->((display $ pkgName $ sourcePackageId i),[i]) ) ipis) --concatenates all version and sort them, most recent first
                getDep :: [InstalledPackageInfo] -> [(CabalComponent,Dependency)]-> [CabalPackage] -> [CabalPackage]
                getDep [] _ acc= acc
                getDep (InstalledPackageInfo{sourcePackageId=i,exposed=e}:xs) deps acc= let
                        (ds,deps2)=partition (\(_,Dependency n v)->((pkgName i)==n) && withinRange (pkgVersion i) v) deps -- find if version is referenced, remove the referencing component so that it doesn't match an older version
                        in getDep xs deps2 (CabalPackage (display $ pkgName i) (display $ pkgVersion i) e (map fst ds): acc) -- build CabalPackage structure
        --}
        
dependencies :: FilePath -> PD.PackageDescription -> [(FilePath,[InstalledPackageInfo])] -> [(FilePath,[CabalPackage])]
dependencies cabal_file pd pkgs=let
        pkgsMap=foldr buildPkgMap DM.empty pkgs -- build the map of package by name with ordered version (more recent first)
        allC= cabalComponentsFromDescription cabal_file pd
        gdeps=PD.buildDepends pd
        cpkgs=concat $ DM.elems $ DM.map (\ipis->getDep allC ipis gdeps []) pkgsMap
        in DM.assocs $ DM.fromListWith (++) $ ((map (\(a,b)->(a,[b])) cpkgs) ++ (map (\(a,_)->(a,[])) pkgs))
        where 
#if CABAL_VERSION == 106
                sourcePackageId = package
#endif
                buildPkgMap :: (FilePath,[InstalledPackageInfo]) -> DM.Map String [(FilePath,InstalledPackageInfo)] -> DM.Map String  [(FilePath,InstalledPackageInfo)]
                buildPkgMap (fp,ipis) m=foldr (\i dm->let
                        key=display $ pkgName $ sourcePackageId i
                        vals=DM.lookup key dm
                        newvals=case vals of
                                Nothing->[(fp,i)]
                                Just l->sortBy (flip (compare `on` (pkgVersion . sourcePackageId . snd))) ((fp,i):l)
                        in DM.insert key newvals dm
                        ) m ipis
                getDep :: [CabalComponent] -> [(FilePath,InstalledPackageInfo)] -> [Dependency]-> [(FilePath,CabalPackage)] -> [(FilePath,CabalPackage)]
                getDep _ [] _ acc= acc
#if CABAL_VERSION == 106
                getDep allC ((fp,InstalledPackageInfo{package=i,exposed=e,exposedModules=ems}):xs) deps acc= let
#else
                getDep allC ((fp,InstalledPackageInfo{sourcePackageId=i,exposed=e,exposedModules=ems}):xs) deps acc= let
#endif
                        (ds,deps2)=partition (\(Dependency n v)->((pkgName i)==n) && withinRange (pkgVersion i) v) deps -- find if version is referenced, remove the referencing component so that it doesn't match an older version
                        cps=if null ds then [] else allC
                        mns=map display ems
                        in getDep allC xs deps2 ((fp,CabalPackage (display $ pkgName i) (display $ pkgVersion i) e cps mns): acc) -- build CabalPackage structure
                
                --DM.map (sortBy (flip (compare `on` (pkgVersion . sourcePackageId . snd)))) $ DM.fromListWith (++) (map (\i->((display $ pkgName $ sourcePackageId i),[i]) ) ipis) --concatenates all version and sort them, most recent first
        


cabalParseArbitrary :: String -> ScionM (PD.ParseResult PD.GenericPackageDescription)
cabalParseArbitrary cabal_contents = do  
  let pr = PD.parsePackageDescription cabal_contents 
  return pr

-- returns a list of cabal configurations
-- dist: those who have been configured * /setup-config 
-- config: those from the .scion-config project configuration file
-- all: both
-- uniq: both, but prefer config items
cabalConfigurations :: FilePath -- ^ The .cabal file
                       -> String -- ^ one of "dist" "config" "all"
                       -> Bool -- only show scion default? 
                       -> ScionM [CabalConfiguration]
cabalConfigurations _cabal _type' _scionDefaultOnly = do
  undefined
{-
  let allowed = ["dist", "config", "all", "uniq"]
  when (not $ elem type' allowed) $ scionError $ "invalid value for type, expected: one of " ++ (show allowed)
  let dir = takeDirectory cabal
  existingDists <- liftIO $ filterM (doesFileExist . (\c -> dir </> c </> "setup-config"))
  =<< liftM (filter (not . (`elem` [".", ".."]))) (getDirectoryContents dir)
  config <- parseScionProjectConfig $ projectConfigFileFromDir dir
  let list = (if type' `elem` ["all", "config", "uniq"] then buildConfigurations config else [])
          -- TODO read flags from setup-config files 
          ++ (if type' `elem` ["all", "dist",  "uniq"] then map (\ a-> CabalConfiguration a []) existingDists else [])
  let f = if type' == "uniq" then nubBy (\a b -> distDir a == distDir b) else id
  -- apply filter 
  let list' = f list
  let d = scionDefaultCabalConfig config
  let scionDefault = filter ( ((fromJust d) ==) . distDir) list'
  return $ if isJust d && scionDefaultOnly && (not . null) scionDefault
    then scionDefault
    else list'
-}
-- | Run the steps that Cabal would call before building.
--
-- The main purpose is to run various pre-processors like @c2hs@,
-- @alex@, @happy@, etc.
-- 
preprocessPackage :: FilePath
                  -> ScionM ()
preprocessPackage dist_dir = do
  lbi <- liftIO $ getPersistBuildConfig (localBuildInfoFile dist_dir)
  let pd = localPkgDescr lbi
#if CABAL_VERSION > 110
  liftIO $ initialBuildSteps dist_dir pd lbi V.normal
#else
  liftIO $ initialBuildSteps dist_dir pd lbi V.normal knownSuffixHandlers
#endif
  return ()

cabalModuleNameToTarget :: PD.ModuleName -> Target
cabalModuleNameToTarget name =
   Target { targetId = TargetModule (mkModuleName
                                     (cabal_mod_to_string name))
          , targetAllowObjCode = True
          , targetContents = Nothing }
  where
    cabal_mod_to_string m =
        intercalate "." (PD.components m)

-- | Configure a Cabal project using the Cabal library.
-- 
-- This is roughly equivalent to calling "./Setup configure" on the
-- command line.  The difference is that this makes sure to use the
-- same version of Cabal and the GHC API that Scion was built against.
-- This is important to avoid compatibility problems.
-- 
-- If configuration succeeded, sets it as the current project.
-- 
-- TODO: Figure out a way to report more helpful error messages.
-- 
-- Throws:
-- 
--  * 'CannotOpenCabalProject' if configuration failed.
-- 
configureCabalProject :: 
     FilePath -- ^ The project root.  (Where the .cabal file resides)
  -> FilePath -- ^ dist dir, i.e., directory where to put generated
              -- files.
  -> ScionM (LocalBuildInfo)
configureCabalProject root_dir dist_dir = do
   cabal_file <- find_cabal_file
   gen_pkg_descr <- liftIO $ readPackageDescription V.normal cabal_file
   let prog_conf =
         userSpecifyPaths [("ghc", ghc), ("ghc-pkg", ghc_pkg)]
           defaultProgramConfiguration
   user_flags <- getSessionSelector userFlags     
   let config_flags = 
         (defaultConfigFlags prog_conf)
           { configDistPref = Flag dist_dir
           , configVerbosity = Flag V.deafening
           , configUserInstall = Flag True
#if CABAL_VERSION > 108
           , configTests = Flag True
#endif
           , configConfigurationsFlags = map (\(n,v)->(PD.FlagName n,v)) user_flags
           }
                   
   setWorkingDir root_dir
   ghandle (\(e :: IOError) ->
               liftIO $ throwIO $ 
                CannotOpenCabalProject ("Failed to configure: " ++ (show e))) $ do
#if CABAL_VERSION < 107
     lbi <- liftIO $ configure (Left gen_pkg_descr, (Nothing, []))
                           config_flags
#else
     lbi <- liftIO $ configure (gen_pkg_descr, (Nothing, []))
                           config_flags
#endif
     liftIO $ writePersistBuildConfig dist_dir lbi
#if CABAL_VERSION > 110
     liftIO $ initialBuildSteps dist_dir (localPkgDescr lbi) lbi V.normal
#else
     liftIO $ initialBuildSteps dist_dir (localPkgDescr lbi) lbi V.normal knownSuffixHandlers
#endif
     return lbi
 where
   find_cabal_file = do
      fs <- liftIO $ getDirectoryContents root_dir
      case [ f | f <- fs, takeExtension f == ".cabal" ] of
        [f] -> return $ root_dir </> f
        [] -> liftIO $ throwIO $ CannotOpenCabalProject "no .cabal file"
        _ -> liftIO $ throwIO $ 
               CannotOpenCabalProject "Too many .cabal files"

instance JSON CabalComponent where
  fromJSON obj@(JSObject _)
    | Just JSNull <- Dic.lookupKey obj (Dic.library),
      Just (JSString f) <- Dic.lookupKey obj Dic.cabalfile,
      Just (JSBool b) <- Dic.lookupKey obj Dic.buildable =
        return $ Library (S.unpack f) b
    | Just (JSString s) <- Dic.lookupKey obj (Dic.executable),
      Just (JSString f) <- Dic.lookupKey obj Dic.cabalfile,
      Just (JSBool b) <- Dic.lookupKey obj Dic.buildable =
        return $ Executable (S.unpack f) (S.unpack s) b
    | Just (JSString s) <- Dic.lookupKey obj (Dic.testsuite),
      Just (JSString f) <- Dic.lookupKey obj Dic.cabalfile,
      Just (JSBool b) <- Dic.lookupKey obj Dic.buildable =
        return $ TestSuite (S.unpack f) (S.unpack s) b        
  fromJSON _ = fail "component"

  toJSON (Library f b) =
    Dic.makeObject [(Dic.library, JSNull),
                (Dic.cabalfile, JSString (S.pack f)),
                (Dic.buildable, JSBool b)]
  toJSON (Executable f n b) =
      Dic.makeObject [(Dic.executable, JSString (S.pack n)),
                  (Dic.cabalfile, JSString (S.pack f)),
                (Dic.buildable, JSBool b)]
  toJSON (TestSuite f n b) =
      Dic.makeObject [(Dic.testsuite, JSString (S.pack n)),
                  (Dic.cabalfile, JSString (S.pack f)),
                (Dic.buildable, JSBool b)]
                
instance JSON CabalPackage where
        fromJSON obj@(JSObject _) | 
                Just (JSString n) <- Dic.lookupKey obj Dic.name,
                Just (JSString v) <- Dic.lookupKey obj Dic.version=do
                        JSBool e <- Dic.lookupKey obj Dic.exposed
                        ds <- fromJSON =<< Dic.lookupKey obj Dic.dependent
                        mns <- fromJSON =<< Dic.lookupKey obj Dic.modules
                        return $ CabalPackage (S.unpack n) (S.unpack v) e ds mns
        fromJSON _ = fail "CabalPackage"
        toJSON (CabalPackage n v e ds mns)=Dic.makeObject [(Dic.name,JSString (S.pack n)),(Dic.version,JSString (S.pack v)),(Dic.exposed,JSBool e),(Dic.dependent,toJSON ds),(Dic.modules,toJSON mns)]

--instance (Data a) => JSON (PD.ParseResult a) where
--        fromJSON _= undefined
--        toJSON (PD.ParseFailed pf)=Dic.makeObject [(Dic.error,JSString (S.pack $ show pf))]
--        toJSON (PD.ParseOk wrns _)=Dic.makeObject [(Dic.warnings,JSArray (map (JSString . S.pack . show) wrns)),
--                (Dic.result,JSObject DM.empty)] --toJSON a
