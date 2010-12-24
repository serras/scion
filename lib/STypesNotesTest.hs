-- | Test harness for Scion.Types.Notes
module Main where

import IO
import Scion.Types.Notes
import Test.HUnit

-- | The main harness
main :: IO Counts
main = do
  putStrLn "Running all tests..."
  runTestTT tests
  
-- | Overlap test harness, without progress reports
overlapLocTest :: IO (Counts, Int)
overlapLocTest = do
  putStrLn "Running overlapLoc tests..."
  runTestText (putTextToHandle stdout False) overlapLocTests

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
  , TestLabel "multLineLoc1/5 no overlap" testMultiOverlap_15
  , TestLabel "multLineLoc1/6 no overlap" testMultiOverlap_16
  , TestLabel "multLineLoc1/7 no overlap" testMultiOverlap_17
  , TestLabel "multLineLoc1/8 overlap" testMultiOverlap_18
  , TestLabel "multiLineLoc1/oneLineLoc1 overlap" testMultiSingleOverlap_1
  , TestLabel "oneLineLoc1/oneLineLoc1a overlap" testSingleOverlap_1a
  , TestLabel "oneLineLoc1/oneLineLoc2 no overlap" testSingleOverlap_12
  , TestLabel "oneLineLoc1/oneLineLoc2 no overlap" testSingleOverlap_13
  , TestLabel "oneLineLoc1/oneLineLoc2 no overlap" testSingleOverlap_14
  ]
  where
    testMultiOverlap_self = TestCase (assertBool (show multiLineLoc1) (overlapLoc multiLineLoc1 multiLineLoc1))
    testMultiOverlap_12 = TestCase (assertBool ((show multiLineLoc1) ++ " " ++ (show multiLineLoc2))
                                               (   (overlapLoc multiLineLoc1 multiLineLoc2)
                                                && (overlapLoc multiLineLoc2 multiLineLoc1)))
    testMultiOverlap_13 = TestCase (assertBool ((show multiLineLoc1) ++ " " ++ (show multiLineLoc3))
                                               (   (overlapLoc multiLineLoc1 multiLineLoc3)
                                                && (overlapLoc multiLineLoc3 multiLineLoc1)))
    testMultiOverlap_14 = TestCase (assertBool ((show multiLineLoc1) ++ " " ++ (show multiLineLoc4))
                                               (   (overlapLoc multiLineLoc1 multiLineLoc4)
                                                && (overlapLoc multiLineLoc1 multiLineLoc4)))
    testMultiOverlap_15 = TestCase (assertBool ((show multiLineLoc1) ++ " " ++ (show multiLineLoc5))
                                               (   (not (overlapLoc multiLineLoc1 multiLineLoc5))
                                                && (not (overlapLoc multiLineLoc5 multiLineLoc1))))
    testMultiOverlap_16 = TestCase (assertBool ((show multiLineLoc1) ++ " " ++ (show multiLineLoc6))
                                               (   (not (overlapLoc multiLineLoc1 multiLineLoc6))
                                                && (not (overlapLoc multiLineLoc6 multiLineLoc1))))
    testMultiOverlap_17 = TestCase (assertBool ((show multiLineLoc1) ++ " " ++ (show multiLineLoc7))
                                               (   (not (overlapLoc multiLineLoc1 multiLineLoc7))
                                                && (not (overlapLoc multiLineLoc7 multiLineLoc1))))
    testMultiOverlap_18 = TestCase (assertBool ((show multiLineLoc1) ++ " " ++ (show multiLineLoc8))
                                               (   (overlapLoc multiLineLoc1 multiLineLoc8)
                                                && (overlapLoc multiLineLoc8 multiLineLoc1)))
    testMultiSingleOverlap_1 = TestCase (assertBool ((show multiLineLoc1) ++ " " ++ (show oneLineLoc1))
                                                    (   (overlapLoc multiLineLoc1 oneLineLoc1)
                                                     && (overlapLoc oneLineLoc1 multiLineLoc1)))
    testSingleOverlap_1a = TestCase (assertBool ((show multiLineLoc1) ++ " " ++ (show oneLineLoc1))
                                                (   (overlapLoc multiLineLoc1 oneLineLoc1)
                                                 && (overlapLoc oneLineLoc1 multiLineLoc1)))
    testSingleOverlap_12 = TestCase (assertBool ((show oneLineLoc1) ++ " " ++ (show oneLineLoc2))
                                                (   (not (overlapLoc oneLineLoc1 oneLineLoc2))
                                                 && (not (overlapLoc oneLineLoc2 oneLineLoc1))))
    testSingleOverlap_13 = TestCase (assertBool ((show oneLineLoc1) ++ " " ++ (show oneLineLoc3))
                                                (   (overlapLoc oneLineLoc1 oneLineLoc3)
                                                 && (overlapLoc oneLineLoc3 oneLineLoc1)))
    testSingleOverlap_14 = TestCase (assertBool ((show oneLineLoc1) ++ " " ++ (show oneLineLoc4))
                                                (   (overlapLoc oneLineLoc1 oneLineLoc4)
                                                 && (overlapLoc oneLineLoc4 oneLineLoc1)))

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

                                                 
-- | A multiline Location
multiLineLoc1 :: Location
multiLineLoc1 = mkMultiLineLoc dummyOtherSrc 1 5 10 79

-- | Another multiline Location, overlapping with multiLineLoc1's end
multiLineLoc2 :: Location
multiLineLoc2 = mkMultiLineLoc dummyOtherSrc 5 5 78 82

-- | Another multiline Location, overlapping with the edge multiLineLoc1's start
multiLineLoc3 :: Location
multiLineLoc3 = mkMultiLineLoc dummyOtherSrc 1 1 0 11

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

-- | A single line Location
oneLineLoc1 :: Location
oneLineLoc1 = mkLineLoc dummyOtherSrc 1 10 20

-- | A single line Location that doesn't overlap with multiLineLoc_1
oneLineLoc1a :: Location
oneLineLoc1a = mkLineLoc dummyOtherSrc 10 1 5

-- | A single line Location that begins before oneLineLoc1
oneLineLoc2 :: Location
oneLineLoc2 = mkLineLoc dummyOtherSrc 1 0 5

-- | A single line Location that overlaps the beginning of oneLineLoc1
oneLineLoc3 :: Location
oneLineLoc3 = mkLineLoc dummyOtherSrc 1 5 15

-- | A single line Location that completely overlaps oneLineLoc1
oneLineLoc4 :: Location
oneLineLoc4 = mkLineLoc dummyOtherSrc 1 10 24

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
