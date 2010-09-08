
module Test.InspectTest where

import Scion
import Scion.Inspect
import Scion.Types
import Scion.Types.Notes
import Scion.Types.Outline

import System.Directory
import System.FilePath

import Test.HUnit

inspectTests :: Test
inspectTests=TestList [testNoPreproc,testPreproc,testPreproc2Lines,testLiterate]

testNoPreproc:: Test
testNoPreproc=TestLabel "testNoPreproc" (TestCase (
        do
        let s="module Main\nwhere\nimport Data.Map\nmain=undefined\n"
        let (tt,s2)=preprocessSource s False
        assertBool "tt is not empty" (null tt)
        assertEqual ("content has changed: "++ s2) s s2
        ))
        
testPreproc:: Test
testPreproc=TestLabel "testPreproc" (TestCase (
        do
        let s="\nmodule Main\nwhere\n#if GHC_VERSION=612\nimport Data.Map\n#endif\nmain=undefined\n"
        let (tt,s2)=preprocessSource s False
        assertEqual "tt is not 2" 2 (length tt)
        let (t1:t2:[])=tt
        assertEqual "first tt is not correct" (TokenDef "PP" (mkLocation (OtherSrc "<interactive>") 4 0 4 19)) (t1)
        assertEqual "second tt is not correct" (TokenDef "PP" (mkLocation (OtherSrc "<interactive>") 6 0 6 6)) (t2)
        assertEqual ("content is not what expected: "++ s2) "\nmodule Main\nwhere\n\nimport Data.Map\n\nmain=undefined\n" s2
        ))       

testPreproc2Lines:: Test
testPreproc2Lines=TestLabel "testPreproc2Lines" (TestCase (
        do
        let s="\nmodule Main\nwhere\n#if GHC_VERSION\\\n=612\nimport Data.Map\n#endif\nmain=undefined\n"
        let (tt,s2)=preprocessSource s False
        assertEqual "tt is not 3" 3 (length tt)
        let (t1:t2:t3:[])=tt
        putStrLn $ show tt
        assertEqual "first tt is not correct" (TokenDef "PP" (mkLocation (OtherSrc "<interactive>") 4 0 4 16)) (t1)
        assertEqual "second tt is not correct" (TokenDef "PP" (mkLocation (OtherSrc "<interactive>") 5 0 5 4)) (t2)
        assertEqual "third tt is not correct" (TokenDef "PP" (mkLocation (OtherSrc "<interactive>") 7 0 7 6)) (t3)
        assertEqual ("content is not what expected: "++ s2) "\nmodule Main\nwhere\n\n\nimport Data.Map\n\nmain=undefined\n" s2
        ))       
        
testLiterate:: Test
testLiterate= TestLabel "testLiterate" (TestCase (
        do
        let s="comment for literate module\n> module Main\n2nd comment\n> where\n> import Data.Map\n"
        let (tt,s2)=preprocessSource s True
        assertEqual "tt is not 2" 2 (length tt)
        let (t1:t2:[])=tt
        assertEqual "first tt is not correct" (TokenDef "DL" (mkLocation (OtherSrc "<interactive>") 1 0 1 27)) (t1)
        assertEqual "second tt is not correct" (TokenDef "DL" (mkLocation (OtherSrc "<interactive>") 3 0 3 11)) (t2)
        assertEqual ("content is not what expected: "++ s2) "\n  module Main\n\n  where\n  import Data.Map\n" s2
        ))

perf:: IO()
perf = do
        base_dir <- getCurrentDirectory
        putStrLn base_dir
        let file= base_dir </> "lib" </> "Scion" </> "Cabal.hs"
        contents<-readFile file
        putStrLn (show $ length $ contents)
        r<-runScion $ do
                r<-tokenTypesArbitrary base_dir contents False
                return r
        case r of 
                Left n -> putStrLn (show n)
                Right tts-> putStrLn (show $ length $ tts)
