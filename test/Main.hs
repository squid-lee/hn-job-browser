module Main where

import FeaturesTest (tests)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

main :: IO ()
main = do
  results <- runTestTT tests
  if failures results > 0 || errors results > 0
    then exitFailure
    else exitSuccess
