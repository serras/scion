
module Test.CabalTest where

import Scion.Cabal

import Data.Function
import Data.List
import Data.Maybe

import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Parse as PD
import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Text

import Test.HUnit

cabalTests :: Test
cabalTests=TestList [testDependenciesOneVersion,testDependenciesOneComponent,testDependenciesTwoComponents,
        testDependenciesDifferentVersionSameDB,testDependenciesDifferentVersionDifferentDB]

sampleCabalContents::String
sampleCabalContents=unlines [
        "name: SampleCabal",
        "build-type:      Simple",
        "",
        "library",
        "  build-depends:   array,containers,mtl",
        "  hs-source-dirs:  lib",
        ""--,
        --"executable sample1",
        --"  main-is:  Main.hs",
        --"  build-depends: mtl"
        ]

sampleCabal :: PD.GenericPackageDescription
sampleCabal = case PD.parsePackageDescription sampleCabalContents of
                ParseOk _ pd->pd
                ParseFailed err -> error $ show err

cabalFileName :: FilePath
cabalFileName = "sample.cabal"

testIPI :: String -> String -> InstalledPackageInfo
testIPI name version=emptyInstalledPackageInfo { sourcePackageId  = PackageIdentifier (PackageName name) (fromJust $ simpleParse version)}

testDependenciesOneVersion :: Test
testDependenciesOneVersion=TestLabel "testDependenciesOneVersion" (TestCase (
        do
        let deps=dependencies cabalFileName sampleCabal [("user.pkg",[]),("system.pkg",[testIPI "array" "1.0.0.2"])]
        assertEqual "not 2 dependency" 2 (length deps)
        let (user,system)=partition (\(x,_)->x=="user.pkg") deps
        assertEqual "user not present" 1 (length user)
        assertEqual "system not present" 1 (length system)
        let (fp,pkgs)=head system
        assertEqual "not system.pkg" "system.pkg" fp
        assertEqual "not 1 package" 1 (length pkgs)
        let pkg=head pkgs
        assertEqual "not array" "array" (cp_name pkg)
        assertEqual "not 1.0.0.2" "1.0.0.2" (cp_version pkg)
        let cpDep=cp_dependent pkg
        assertEqual "not 1 dependent" 1 (length cpDep)
        assertEqual "not library" (Library cabalFileName True) (head cpDep)
        let (fpU,pkgsU)=head user
        assertEqual "not user.pkg" "user.pkg" fpU
        assertEqual "not 0 package" 0 (length pkgsU)
        ))

testDependenciesOneComponent :: Test
testDependenciesOneComponent=TestLabel "testDependenciesOneComponent" (TestCase (
        do
        let deps=dependencies cabalFileName sampleCabal [("user.pkg",[]),("system.pkg",[testIPI "array" "1.0.0.2",testIPI "containers" "2.0.0.1"])]
        assertEqual "not 2 dbs" 2 (length deps)
        let (user,system)=partition (\(x,_)->x=="user.pkg") deps
        assertEqual "user not present" 1 (length user)
        assertEqual "system not present" 1 (length system)
        let (fp,pkgs)=head system
        assertEqual "not system.pkg" "system.pkg" fp
        assertEqual "not 2 package" 2 (length pkgs)
        let (pkgArr:pkgCont:[])=sortBy (compare `on` cp_name) pkgs
        assertEqual "not array" "array" (cp_name pkgArr)
        assertEqual "not 1.0.0.2" "1.0.0.2" (cp_version pkgArr)
        let cpDepArr=cp_dependent pkgArr
        assertEqual "not 1 dependent" 1 (length cpDepArr)
        assertEqual "not library" (Library cabalFileName True) $ head cpDepArr
        
        assertEqual "not containers" "containers" (cp_name pkgCont)
        assertEqual "not 2.0.0.1" "2.0.0.1" (cp_version pkgCont)
        let cpDepCont=cp_dependent pkgCont
        assertEqual "not 1 dependent" 1 (length cpDepCont)
        assertEqual "not library" (Library cabalFileName True) $ head cpDepArr
        
        let (fpU,pkgsU)=head user
        assertEqual "not user.pkg" "user.pkg" fpU
        assertEqual "not 0 package" 0 (length pkgsU)
        ))

testDependenciesTwoComponents :: Test
testDependenciesTwoComponents=TestLabel "testDependenciesTwoComponents" (TestCase (
        do
        let ParseOk _ pd=PD.parsePackageDescription (sampleCabalContents ++ (unlines [
                "executable sample",
                "  main-is:  Main.hs",
                "  build-depends: mtl"
                ]))
        let deps=dependencies cabalFileName pd [("user.pkg",[]),("system.pkg",[testIPI "mtl" "1.0.0.2"])]
        assertEqual "not 2 dbs" 2 (length deps)
        let (user,system)=partition (\(x,_)->x=="user.pkg") deps
        assertEqual "user not present" 1 (length user)
        assertEqual "system not present" 1 (length system)
        let (fp,pkgs)=head system
        assertEqual "not system.pkg" "system.pkg" fp
        assertEqual "not 1 package" 1 (length pkgs)
        let (pkg:[])=pkgs
        assertEqual "not mtl" "mtl" (cp_name pkg)
        assertEqual "not 1.0.0.2" "1.0.0.2" (cp_version pkg)
        let cpDep=cp_dependent pkg
        assertEqual "not 2 dependents" 2 (length cpDep)
        assertBool "not library" (elem (Library cabalFileName True) cpDep) 
        assertBool "not executable" (elem (Executable cabalFileName "sample" True) cpDep) 

        let (fpU,pkgsU)=head user
        assertEqual "not user.pkg" "user.pkg" fpU
        assertEqual "not 0 package" 0 (length pkgsU)
        ))

testDependenciesDifferentVersionSameDB :: Test
testDependenciesDifferentVersionSameDB=TestLabel "testDependenciesDifferentVersionSameDB" (TestCase (
        do
        let deps=dependencies cabalFileName sampleCabal [("user.pkg",[]),("system.pkg",[testIPI "array" "1.0.0.1",testIPI "array" "1.0.0.2"])]
        assertEqual "not 2 dbs" 2 (length deps)
        let (user,system)=partition (\(x,_)->x=="user.pkg") deps
        assertEqual "user not present" 1 (length user)
        assertEqual "system not present" 1 (length system)
        let (fp,pkgs)=head system
        assertEqual "not system.pkg" "system.pkg" fp
        assertEqual "not 2 packages" 2 (length pkgs)
        let ((yes:[]),(no:[]))=partition (\x->"1.0.0.2"== cp_version x) pkgs
        assertEqual "not array" "array" (cp_name yes)
        assertEqual "not 1.0.0.2" "1.0.0.2" (cp_version yes)
        let cpDep=cp_dependent yes
        assertEqual "not 1 dependent" 1 (length cpDep)
        assertEqual "not library" (Library cabalFileName True) (head cpDep)
        assertEqual "not array" "array" (cp_name no)
        assertEqual "not 1.0.0.1" "1.0.0.1" (cp_version no)
        let cpDepN=cp_dependent no
        assertEqual "not 0 dependent" 0 (length cpDepN)
        ))

testDependenciesDifferentVersionDifferentDB :: Test
testDependenciesDifferentVersionDifferentDB=TestLabel "testDependenciesDifferentVersionDifferentDB" (TestCase (
        do
        let deps=dependencies cabalFileName sampleCabal [("user.pkg",[testIPI "array" "1.0.0.1"]),("system.pkg",[testIPI "array" "1.0.0.2"])]
        assertEqual "not 2 dbs" 2 (length deps)
        let (user,system)=partition (\(x,_)->x=="user.pkg") deps
        assertEqual "user not present" 1 (length user)
        assertEqual "system not present" 1 (length system)
        let (fp,pkgs)=head system
        assertEqual "not system.pkg" "system.pkg" fp
        assertEqual "not 1 package" 1 (length pkgs)
        let pkg=head pkgs
        assertEqual "not array" "array" (cp_name pkg)
        assertEqual "not 1.0.0.2" "1.0.0.2" (cp_version pkg)
        let cpDep=cp_dependent pkg
        assertEqual "not 1 dependent" 1 (length cpDep)
        assertEqual "not library" (Library cabalFileName True) (head cpDep)
        let (fpU,pkgsU)=head user
        assertEqual "not user.pkg" "user.pkg" fpU
        assertEqual "not 1 package" 1 (length pkgsU)
        let pkgU=head pkgsU
        assertEqual "not array" "array" (cp_name pkgU)
        assertEqual "not 1.0.0.1" "1.0.0.1" (cp_version pkgU)
        let cpDepU=cp_dependent pkgU
        assertEqual "not 0 dependent" 0 (length cpDepU)

        ))
