{-# LANGUAGE CPP #-}
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
module Scion.Packages where

import qualified Config
import qualified System.Info
import Data.List
import Distribution.InstalledPackageInfo
import GHC.Paths
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.IO.Error

#if GHC_VERSION < 611
getPkgInfos :: IO [(FilePath,[InstalledPackageInfo])]
getPkgInfos = do
    global_conf <-
        catch (getEnv "GHC_PKGCONF")
              (\err ->  if isDoesNotExistError err
                            then do let dir = takeDirectory $ takeDirectory ghc_pkg
                                        path1 = libdir </> "package.conf"
                                        path2 = libdir </> ".." </> ".." </> ".."
                                                       </> "inplace-datadir"
                                                       </> "package.conf"
                                        path3 = dir </> "package.conf"
                                        path4 = dir </> ".." </> ".." </> ".."
                                                    </> "inplace-datadir"
                                                    </> "package.conf"
                                        searched = [ path1, path2, path3, path4 ]
                                    exists1 <- doesFileExist path1
                                    exists2 <- doesFileExist path2
                                    exists3 <- doesFileExist path3
                                    exists4 <- doesFileExist path4
                                    if exists1 then return path1
                                       else if exists2 then return path2
                                       else if exists3 then return path3
                                       else if exists4 then return path4
                                       else ioError $ userError ("Can't find package.conf, searched " ++ (foldl1 (++) searched))
                            else ioError err)

    let global_conf_dir = global_conf ++ ".d"
    global_conf_dir_exists <- doesDirectoryExist global_conf_dir
    global_confs <-
        if global_conf_dir_exists
            then do files <- getDirectoryContents global_conf_dir
                    return  [ global_conf_dir ++ '/' : file
                            | file <- files
                            , isSuffixOf ".conf" file]
            else return []

    user_conf <-
        try (getAppUserDataDirectory "ghc") >>= either
            (\_ -> return [])
            (\appdir -> do
                let subdir = currentArch ++ '-':currentOS ++ '-':ghcVersion
                    user_conf = appdir </> subdir </> "package.conf"
                user_exists <- doesFileExist user_conf
                return (if user_exists then [user_conf] else []))

    let pkg_dbs = user_conf ++ global_confs ++ [global_conf]
    return =<< mapM (\a->do
        c<-readFile a
        return (a,read c)) pkg_dbs
        
#else
getPkgInfos :: IO [(FilePath,[InstalledPackageInfo])]
getPkgInfos = do
        global_conf <- do 
             let dir =(takeDirectory $ takeDirectory ghc_pkg) </> "lib"
             r <- lookForPackageDBIn dir
             case r of
               Nothing -> ioError $ userError ("Can't find package database in " ++ dir)
               Just path -> return path
        global_confs<-readContents global_conf
        e_appdir <- try $ getAppUserDataDirectory "ghc"
        user_conf <- do
             case e_appdir of
               Left _    -> return []
               Right appdir -> do
                 let subdir = currentArch ++ '-':currentOS ++ '-':ghcVersion
                     dir = appdir </> subdir
                 r <- lookForPackageDBIn dir
                 case r of
                   Nothing -> return []
                   Just f  -> readContents f
        return (user_conf ++ global_confs)

listConf :: FilePath -> IO [[Char]]
listConf fp=do
        global_conf_dir_exists <- doesDirectoryExist fp
        if global_conf_dir_exists
            then do files <- getDirectoryContents fp
                    return  [ fp ++ '/' : file
                            | file <- files
                            , isSuffixOf ".conf" file]
            else return []

readContents :: FilePath -> IO [(FilePath, [InstalledPackageInfo])]
readContents fp = do
        confs<-listConf fp
        --pis<-mapM ((>>= return.read).readFile) confs
        pis<-mapM (\f->do
                c<-readUTF8File f
                let ParseOk _ i=(parseInstalledPackageInfo c)
                return i) confs
        return [(fp,pis)]

readUTF8File :: FilePath -> IO String
readUTF8File file = do
  h <- openFile file ReadMode
#if __GLASGOW_HASKELL__ >= 612
  -- fix the encoding to UTF-8
  hSetEncoding h utf8
#endif
  hGetContents h

lookForPackageDBIn :: FilePath -> IO (Maybe FilePath)
lookForPackageDBIn dir = do
  let path_dir = dir </> "package.conf.d"
  exists_dir <- doesDirectoryExist path_dir
  if exists_dir then return (Just path_dir) else do
  let path_file = dir </> "package.conf"
  exists_file <- doesFileExist path_file
  if exists_file then return (Just path_file) else return Nothing
#endif
        
        
        
currentArch :: String
currentArch = System.Info.arch

currentOS :: String
currentOS = System.Info.os

ghcVersion :: String
ghcVersion = Config.cProjectVersion
