
module Test.InspectTest where

import Scion
import Scion.Inspect
import Scion.Types
import Scion.Types.Notes
import Scion.Types.Outline

import Text.JSON.AttoJSON
import FastString
import SrcLoc
import Scion.Ghc hiding ( (<+>) )

import System.Directory
import System.FilePath
import qualified Data.ByteString.Char8 as S
import Test.HUnit
import qualified Outputable as O ( (<+>),neverQualify,text )

inspectTests :: Test
inspectTests=TestList [testTokenTypesSimple,testTokenTypesPreproc,testTokenTypesPreproc2Lines,testTokenTypesLiteral,
        testNoPreproc,testPreproc,testPreproc2Lines,testLiterate,testResolveFunctionWithTypeClass,testResolveFunctionWithoutTypeClass]


testTokenTypesSimple :: Test
testTokenTypesSimple = TestLabel "testTokenTypesSimple" (TestCase (
        do
        let contents="module Main\nwhere\nimport Data.Map\nmain=undefined\n"
        base_dir <- getCurrentDirectory
        Right tks<-runScion $ do
                tokenTypesArbitrary base_dir contents False
        let expected=S.pack "[[\"K\", 1, 0, 1, 6], [\"IC\", 1, 7, 1, 11], [\"K\", 2, 0, 2, 5], [\"K\", 3, 0, 3, 6], [\"IC\", 3, 7, 3, 15], [\"IV\", 4, 0, 4, 4], [\"S\", 4, 4, 4, 5], [\"IV\", 4, 5, 4, 14]]"
        let actual=showJSON $ toJSON tks
        assertEqual "unexpected content" expected actual
        ))

testTokenTypesPreproc :: Test
testTokenTypesPreproc = TestLabel "testTokenTypesPreproc" (TestCase (
        do
        let contents="module Main\nwhere\n#if GHC_VERSION=612\nimport Data.Map\n#endif\nmain=undefined\n"
        base_dir <- getCurrentDirectory
        Right tks<-runScion $ do
                tokenTypesArbitrary base_dir contents False
        let expected=S.pack "[[\"K\", 1, 0, 1, 6], [\"IC\", 1, 7, 1, 11], [\"K\", 2, 0, 2, 5], [\"PP\", 3, 0, 3, 19], [\"K\", 4, 0, 4, 6], [\"IC\", 4, 7, 4, 15], [\"PP\", 5, 0, 5, 6], [\"IV\", 6, 0, 6, 4], [\"S\", 6, 4, 6, 5], [\"IV\", 6, 5, 6, 14]]"
        let actual=showJSON $ toJSON tks
        assertEqual "unexpected content" expected actual
        ))

testTokenTypesPreproc2Lines :: Test
testTokenTypesPreproc2Lines = TestLabel "testTokenTypesPreproc2Lines" (TestCase (
        do
        let contents="module Main\nwhere\n#if GHC_VERSION\\\n=612\nimport Data.Map\n#endif\nmain=undefined\n"
        base_dir <- getCurrentDirectory
        Right tks<-runScion $ do
                tokenTypesArbitrary base_dir contents False
        let expected=S.pack "[[\"K\", 1, 0, 1, 6], [\"IC\", 1, 7, 1, 11], [\"K\", 2, 0, 2, 5], [\"PP\", 3, 0, 3, 16], [\"PP\", 4, 0, 4, 4], [\"K\", 5, 0, 5, 6], [\"IC\", 5, 7, 5, 15], [\"PP\", 6, 0, 6, 6], [\"IV\", 7, 0, 7, 4], [\"S\", 7, 4, 7, 5], [\"IV\", 7, 5, 7, 14]]"
        let actual=showJSON $ toJSON tks
        assertEqual "unexpected content" expected actual
        ))


testTokenTypesLiteral :: Test
testTokenTypesLiteral = TestLabel "testTokenTypesLiteral" (TestCase (
        do
        let contents="comment for literate module\n> module Main\n2nd comment\n> where\n> import Data.Map\n"
        base_dir <- getCurrentDirectory
        Right tks<-runScion $ do
                tokenTypesArbitrary base_dir contents True
        let expected=S.pack "[[\"DL\", 1, 0, 1, 27], [\"K\", 2, 2, 2, 8], [\"IC\", 2, 9, 2, 13], [\"DL\", 3, 0, 3, 11], [\"K\", 4, 2, 4, 7], [\"K\", 5, 2, 5, 8], [\"IC\", 5, 9, 5, 17]]"
        let actual=showJSON $ toJSON tks
        assertEqual "unexpected content" expected actual
        ))

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

testResolveFunctionWithTypeClass :: Test
testResolveFunctionWithTypeClass  = TestLabel "testResolveFunctionWithTypeClass" (TestCase (do
        r<-functionAtLine 12
        assertEqual "" "TestR.f2 v" r
        )) 
        
testResolveFunctionWithoutTypeClass :: Test
testResolveFunctionWithoutTypeClass  = TestLabel "testResolveFunctionWithoutTypeClass" (TestCase (do
        r<-functionAtLine 11
        assertEqual "" "TestR.f1 v" r
        ))         
        
functionAtLine :: Int -> IO (String)
functionAtLine line=do
        base_dir <- getCurrentDirectory
        let file= base_dir </> "tests" </> "TestR.hs"
        r<-runScion $ do
                loadComponent' (Component $ FileComp file) (LoadOptions False False)
                backgroundTypecheckFile file
                let loc = srcLocSpan $ mkSrcLoc (fsLit file) line (scionColToGhcCol 13)
                tc_res <- getSessionSelector bgTcCache
                let s= showSDocForUser O.neverQualify  --showSDocDebug
                l<-case tc_res of
                        Just (Typechecked tcm) -> do
                            let psrc= renamedSource tcm
                            let in_range = overlaps loc
                            let r = findHsThing in_range psrc
                            return $ case pathToDeepest r of
                              Nothing -> ""
                              Just (x,_) -> (s  $ ((qualifiedResult x)O.<+> (O.text $ haddockType x)))   -- ++"->"++ (concat $ map (("\n\t" ++) . s . ppr) l1)
                        Nothing -> return ""
                return l
        setCurrentDirectory base_dir
        return r
