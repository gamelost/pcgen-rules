{-# LANGUAGE OverloadedStrings #-}

module JEPFormulaTests where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Test.HUnit
import JEPFormula
import Common

parseJEP :: T.Text -> Formula
parseJEP contents = parseResult "parseJEP" $ parse parseFormula contents

testInt = parseJEP "2" @?=
          Number 2

testSignedInt = parseJEP "-4" @?=
                Number (-4)

testVarFunc = parseJEP "floor(SynergyBonus/2)" @?=
              Function (BuiltIn "floor") [Function Divide [Variable "SynergyBonus", Number 2]]

testNestedInfixFunc = parseJEP "max(0,Reputation-INT)" @?=
                      Function (BuiltIn "max") [Number 0, Function Subtract [Variable "Reputation", Variable "INT"]]

-- max(0,floor((var("SKILLRANK=Tumble")-5)/20))*SynergyBonus
-- max(0,floor((var("SKILLRANK=Concentration")-5)/20))*SynergyBonus

tests :: Test
tests = TestList [ "parse integer" ~: testInt
                 , "parse unsigned integer" ~: testSignedInt
                 , "parse function with variable" ~: testVarFunc
                 , "parse function with nested infix function" ~: testNestedInfixFunc
                 ]
