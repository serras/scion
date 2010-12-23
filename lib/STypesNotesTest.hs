-- | Test harness for Scion.Types.Notes
module Main where

import Scion.Types.Notes
import Test.HUnit

-- | The main harness
main :: IO Counts
main = do
  runTestTT tests

-- | The unit tests to run.
tests :: Test
tests = TestList [
    TestLabel "mkNoLoc equality" testEqNoLoc
  , TestLabel "mkLocPoint equality" testEqPoint
  , TestLabel "overlapLoc tests" overlapLocTests
  ]

overlapLocTests :: Test
overlapLocTests = TestList [
    TestLabel "multiLineLoc1 overlap self" testMultiOverlap_self
  , TestLabel "multLineLoc1/2 overlap" testMultiOverlap_12
  , TestLabel "multLineLoc1/3 overlap" testMultiOverlap_13
  , TestLabel "multLineLoc1/4 overlap" testMultiOverlap_14
  , TestLabel "multLineLoc1/5 overlap" testMultiOverlap_15
  , TestLabel "multLineLoc1/6 overlap" testMultiOverlap_16
  , TestLabel "multLineLoc1/7 overlap" testMultiOverlap_17
  , TestLabel "multLineLoc1/8 overlap" testMultiOverlap_18
  ]
  
-- | Test that two LocNone objects are equal
testEqNoLoc :: Test
testEqNoLoc = TestCase (assertBool (show noLoc) (noLoc == (mkNoLoc "a")))
  where noLoc = (mkNoLoc "a")

-- | Test that two point locations are equal
testEqPoint :: Test
testEqPoint = TestCase (assertBool (show ptLoc) (ptLoc == (mkLocPoint baseDir subDir 1 20)))
  where ptLoc = (mkLocPoint baseDir subDir 1 20)
        subDir  = "/subdir"
        
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~
-- overlapLoc tests:
-- =~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~

testMultiOverlap_self :: Test
testMultiOverlap_self = TestCase (assertBool (show multiLineLoc1) (overlapLoc multiLineLoc1 multiLineLoc1))

testMultiOverlap_12 :: Test
testMultiOverlap_12 = TestCase (assertBool ((show multiLineLoc1) ++ " " ++ (show multiLineLoc2))
                                           (overlapLoc multiLineLoc1 multiLineLoc2))

testMultiOverlap_13 :: Test
testMultiOverlap_13 = TestCase (assertBool ((show multiLineLoc1) ++ " " ++ (show multiLineLoc3))
                                           (overlapLoc multiLineLoc1 multiLineLoc3))

testMultiOverlap_14 :: Test
testMultiOverlap_14 = TestCase (assertBool ((show multiLineLoc1) ++ " " ++ (show multiLineLoc4))
                                           (overlapLoc multiLineLoc1 multiLineLoc4))

testMultiOverlap_15 :: Test
testMultiOverlap_15 = TestCase (assertBool ((show multiLineLoc1) ++ " " ++ (show multiLineLoc5))
                                           (not (overlapLoc multiLineLoc1 multiLineLoc5)))

testMultiOverlap_16 :: Test
testMultiOverlap_16 = TestCase (assertBool ((show multiLineLoc1) ++ " " ++ (show multiLineLoc6))
                                           (not (overlapLoc multiLineLoc1 multiLineLoc6)))

testMultiOverlap_17 :: Test
testMultiOverlap_17 = TestCase (assertBool ((show multiLineLoc1) ++ " " ++ (show multiLineLoc7))
                                           (not (overlapLoc multiLineLoc1 multiLineLoc7)))

testMultiOverlap_18 :: Test
testMultiOverlap_18 = TestCase (assertBool ((show multiLineLoc1) ++ " " ++ (show multiLineLoc8))
                                           (overlapLoc multiLineLoc1 multiLineLoc8))
                                                  
-- | A multiline Location
multiLineLoc1 :: Location
multiLineLoc1 = mkMultiLineLoc dummyOtherSrc 1 5 10 79

-- | Another multiline Location, overlapping with multiLineLoc1's end
multiLineLoc2 :: Location
multiLineLoc2 = mkMultiLineLoc dummyOtherSrc 5 5 78 82

-- | Another multiline Location, overlapping with the edge multiLineLoc1's start
multiLineLoc3 :: Location
multiLineLoc3 = mkMultiLineLoc dummyOtherSrc 1 1 0 10

-- | Another multiline Location, overlapping with multiLineLoc1's start
multiLineLoc4 :: Location
multiLineLoc4 = mkMultiLineLoc dummyOtherSrc 1 2 0 0

-- | A multiline Location that occurs in front of multiLineLoc1
multiLineLoc5 :: Location
multiLineLoc5 = mkMultiLineLoc dummyOtherSrc 1 1 0 9

-- | A multiline Location that occurs after multiLineLoc1
multiLineLoc6 :: Location
multiLineLoc6 = mkMultiLineLoc dummyOtherSrc 5 5 79 82

-- | Another multiline Location that occurs after multiLineLoc1
multiLineLoc7 :: Location
multiLineLoc7 = mkMultiLineLoc dummyOtherSrc 100 200 24 0

-- | A multiline Location that occurs within multiLineLoc1
multiLineLoc8 :: Location
multiLineLoc8 = mkMultiLineLoc dummyOtherSrc 1 5 10 21 

-- | A base directory absolute file path that satisfies System.Path.Posix.isAbsolute
baseDir :: String
#if defined(mingw32_HOST_OS)
baseDir = "c:\\base\\dir"
#else
baseDir = "/base/dir"
#endif

-- | Dummy other LocSource location
dummyOtherSrc :: LocSource
dummyOtherSrc = OtherSrc "<dummy>"

-- | Other LocSrc location that's not the dummy
dummyOtherSrc2 :: LocSource
dummyOtherSrc2 = OtherSrc "<dummy-other>"
