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
module Scion.Packages ( getPkgInfos) where

import qualified Config
import qualified System.Info
import Data.List
import Data.Maybe
import Control.Monad
import Distribution.InstalledPackageInfo
import Distribution.Text
import qualified Control.Exception as Exception
import GHC.Paths
import System.Directory
import System.Environment (getEnv)
import System.FilePath
import System.IO
import System.IO.Error

#if __GLASGOW_HASKELL__ < 612 || defined(mingw32_HOST_OS)
-- mingw32 needs these for getExecDir, GHC <6.12 needs them for openNewFile
import Foreign
import Foreign.C
#endif

-- This was borrowed from the ghc-pkg source:
type InstalledPackageInfoString = InstalledPackageInfo_ String

-- | Fetch the installed package info from the global and user package.conf
-- databases, mimicking the functionality of ghc-pkg.

getPkgInfos :: IO [(FilePath,[InstalledPackageInfo])]
getPkgInfos = 
  let
    -- | Test for package database's presence in a given directory
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
              then return (Just path_file)
              else return Nothing

  in do
    let err_msg = "Unable to determine the location of global package.conf\n"

    -- Get the global package configuration database:
    global_conf <- do
      proposed <- getLibDir
      case proposed of
        Nothing  -> ioError $ userError err_msg
        Just dir -> do
          r <- lookForPackageDBIn dir
          case r of
            Nothing -> ioError $ userError ("Can't find package database in " ++ dir)
            Just _  -> return dir

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
               Nothing -> return []
               Just _  -> readContents dir

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

    -- Utility function that just flips the arguments to Control.Exception.catch
    catchError :: IO a -> (String -> IO a) -> IO a
    catchError io handler = io `Exception.catch` handler'
        where handler' (Exception.ErrorCall err) = handler err
  in do
        confs <- listConf fp
        let pkgInfoReader f = do
                pkgStr <- readUTF8File f
                let pkgs = map convertPackageInfoIn $ read pkgStr
                Exception.evaluate pkgs
                  `catchError` \e-> ioError $ userError ("error while parsing " ++ f ++ ": " ++ (show e))
        pkgInfoList <- mapM pkgInfoReader confs
        return [(fp, join pkgInfoList)]

-----------------------------------------
-- Adapted from GHC's util/ghc-pkg/Main.hs:
-----------------------------------------

#if defined(mingw32_HOST_OS)
subst :: Char -> Char -> String -> String
subst a b ls = map (\ x -> if x == a then b else x) ls

unDosifyPath :: FilePath -> FilePath
unDosifyPath xs = subst '\\' '/' xs

getLibDir :: IO (Maybe String)
getLibDir = fmap (fmap (</> "lib")) $ getExecDir "/bin/ghc-pkg.exe"

-- (getExecDir cmd) returns the directory in which the current
--                  executable, which should be called 'cmd', is running
-- So if the full path is /a/b/c/d/e, and you pass "d/e" as cmd,
-- you'll get "/a/b/c" back as the result
getExecDir :: String -> IO (Maybe String)
getExecDir cmd =
    getExecPath >>= maybe (return Nothing) removeCmdSuffix
    where initN n = reverse . drop n . reverse
          removeCmdSuffix = return . Just . initN (length cmd) . unDosifyPath

getExecPath :: IO (Maybe String)
getExecPath =
     allocaArray len $ \buf -> do
         ret <- getModuleFileName nullPtr buf len
         if ret == 0 then return Nothing
                     else liftM Just $ peekCString buf
    where len = 2048 -- Plenty, PATH_MAX is 512 under Win32.

foreign import stdcall unsafe "GetModuleFileNameA"
    getModuleFileName :: Ptr () -> CString -> Int -> IO Int32

#else
-- Assuming this is Unix or Mac OS X, then ghc-paths should set libdir
-- for us.
getLibDir :: IO (Maybe String)
getLibDir = return (Just libdir)
#endif
        
currentArch :: String
currentArch = System.Info.arch

currentOS :: String
currentOS = System.Info.os

ghcVersion :: String
ghcVersion = Config.cProjectVersion
