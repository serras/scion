
module Main where

import Test.CabalTest

import Test.HUnit

main :: IO Counts
main = do
        putStrLn "Testing HUnit..."
        runTestTT cabalTests
