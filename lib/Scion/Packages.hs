{-# LANGUAGE ForeignFunctionInterface, CPP #-}
-- |
-- Module      : Scion.Packages
-- Author      : Thiago Arrais
-- Copyright   : (c) Thiago Arrais 2009
-- License     : BSD-style
-- Url         : http://stackoverflow.com/questions/1522104/how-to-programmatically-retrieve-ghc-package-information
-- 
-- Maintainer  : nominolo@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- Cabal-related functionality.
module Scion.Packages ( getPkgInfos ) where

import Prelude hiding (Maybe)
import qualified Config
import qualified System.Info
import Data.List
import Data.Maybe
import Control.Monad
import Distribution.InstalledPackageInfo
import System.Directory
import System.Environment (getEnv)
import System.FilePath
import System.IO
import System.IO.Error

import GHC.Paths

#if CABAL_VERSION < 108
import Distribution.Text
import qualified Control.Exception as Exception

-- This was borrowed from the ghc-pkg source:
type InstalledPackageInfoString = InstalledPackageInfo_ String
#endif

-- | Fetch the installed package info from the global and user package.conf
-- databases, mimicking the functionality of ghc-pkg.

getPkgInfos :: IO [(FilePath,[InstalledPackageInfo])]
getPkgInfos = 
  let
    err_msg = "Unable to determine the location of global package.conf\n"
    
    -- | Test for package database's presence in a given directory
    -- NB: The directory is returned for later scanning by listConf,
    -- which parses the actual package database file(s).
    lookForPackageDBIn :: FilePath -> IO (Maybe FilePath)
    lookForPackageDBIn dir =
      let
        path_dir = dir </> "package.conf.d"
        path_file = dir </> "package.conf"
      in do
        exists_dir <- doesDirectoryExist path_dir
        if exists_dir
          then return (Just path_dir)
          else do
            exists_file <- doesFileExist path_file
            if exists_file
              then return (Just dir)
              else return Nothing

  in do

    -- Get the global package configuration database:
    global_conf <- do
      proposed <- getLibDir
      case proposed of
        Nothing  -> ioError $ userError err_msg
        Just dir -> do
          r <- lookForPackageDBIn dir
          case r of
            Nothing     -> ioError $ userError ("Can't find package database in " ++ dir)
            Just dbDir  -> return dbDir

    global_confs <- readContents global_conf

    -- Get the user package configuration database
    e_appdir <- try $ getAppUserDataDirectory "ghc"
    user_conf <- do
         case e_appdir of
           Left _       -> return []
           Right appdir -> do
             let subdir = currentArch ++ '-':currentOS ++ '-':ghcVersion
                 dir = appdir </> subdir
             r <- lookForPackageDBIn dir
             case r of
               Nothing     -> return []
               Just dbDir  -> readContents dbDir

    -- Process GHC_PACKAGE_PATH, if present:
    e_pkg_path <- try (getEnv "GHC_PACKAGE_PATH")
    env_stack <- do 
      case e_pkg_path of
        Left _     -> return []
        Right path -> do
          pkgs <- mapM readContents (parseSearchPath path)
          return $ concat pkgs

    -- Send back the combined installed packages list:
    return (env_stack ++ user_conf ++ global_confs)

parseSearchPath :: String -> [FilePath]
parseSearchPath path = split path
  where
    split :: String -> [String]
    split s =
      case rest' of
        []     -> [chunk]
        _:rest -> chunk : split rest
      where
        chunk =
          case chunk' of
#ifdef mingw32_HOST_OS
            ('\"':xs@(_:_)) | last xs == '\"' -> init xs
#endif
            _                                 -> chunk'

        (chunk', rest') = break isSearchPathSeparator s

-- | Read the contents of the given directory, searching for ".conf" files, and parse the
-- package contents. Returns a singleton list (directory, [installed packages])
readContents :: FilePath                                        -- ^ The directory to examine
                -> IO [(FilePath, [InstalledPackageInfo])]      -- ^ Installed packages
readContents fp =
  let
    -- List package configuration files that might live in the given directory
    listConf :: FilePath -> IO [[Char]]
    listConf fp' = do
            global_conf_dir_exists <- doesDirectoryExist fp'
            if global_conf_dir_exists
                then do files <- getDirectoryContents fp'
                        return  [ fp' </> file | file <- files, isSuffixOf ".conf" file]
                else return []

    -- Read a file, ensuring that UTF8 coding is used for GCH >= 6.12
    readUTF8File :: FilePath -> IO String
    readUTF8File file = do
      h <- openFile file ReadMode
#if __GLASGOW_HASKELL__ >= 612
      -- fix the encoding to UTF-8
      hSetEncoding h utf8
#endif
      hGetContents h

#if CABAL_VERSION < 108
    -- This function was lifted directly from ghc-pkg. Its sole purpose is
    -- parsing an input package description string and producing an
    -- InstalledPackageInfo structure.
    convertPackageInfoIn :: InstalledPackageInfoString -> InstalledPackageInfo
    convertPackageInfoIn
        (pkgconf@(InstalledPackageInfo { exposedModules = e,
                                         hiddenModules = h })) =
            pkgconf{ exposedModules = map convert e,
                     hiddenModules  = map convert h }
        where convert = fromJust . simpleParse

    pkgInfoReader f = do
	pkgStr <- readUTF8File f
	let pkgs = map convertPackageInfoIn $ read pkgStr
	Exception.evaluate pkgs
	  `catchError` \e-> ioError $ "error while parsing " ++ f ++ ": " ++ (show e)

    -- Utility function that just flips the arguments to Control.Exception.catch
    catchError :: IO a -> (String -> IO a) -> IO a
    catchError io handler = io `Exception.catch` handler'
        where handler' (Exception.ErrorCall err) = handler err
#else
    -- Slightly different approach in Cabal 1.8 series, with the package.conf.d directories,
    -- where individual package configuration files are association pairs:
    pkgInfoReader f = do
      pkgStr <- readUTF8File f
      let pkgInfo = parseInstalledPackageInfo pkgStr
      case pkgInfo of
	ParseOk _ info -> return [info]
	-- Serious FIXME: You'd want to return the error here.
        ParseFailed _  -> return [emptyInstalledPackageInfo]
#endif
  in do
        confs <- listConf fp
        pkgInfoList <- mapM pkgInfoReader confs
        return [(fp, join pkgInfoList)]

-- GHC.Path sets libdir for us...
getLibDir :: IO (Maybe String)
getLibDir = return (Just libdir)
        
currentArch :: String
currentArch = System.Info.arch

currentOS :: String
currentOS = System.Info.os

ghcVersion :: String
ghcVersion = Config.cProjectVersion
