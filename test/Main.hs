module Main where

import RestrictionTests(restrictionTests)
import JEPFormulaTests(formulaTests)
import BonusTests(bonusTests)
import Test.HUnit
import Control.Monad
import System.Exit

tests :: [Test]
tests = [ formulaTests
        , restrictionTests
        , bonusTests
        ]

validate :: Test -> IO ()
validate t = do
  c <- runTestTT t
  when (errors c /= 0 || failures c /= 0)
    exitFailure
  return ()

main :: IO ()
main = forM_ tests validate
