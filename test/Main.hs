module Main where

import JEPFormulaTests(tests)
import Test.HUnit
import Control.Monad
import System.Exit

main :: IO ()
main = do
  c <- runTestTT tests
  when (errors c /= 0 || failures c /= 0)
    exitFailure
  return ()
