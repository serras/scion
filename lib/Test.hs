
module Main where

import Test.CabalTest

import Test.HUnit

main=do
        putStrLn "Testing HUnit..."
        runTestTT cabalTests