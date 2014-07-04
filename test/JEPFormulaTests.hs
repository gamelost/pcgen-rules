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

testFuncWithVar = parseJEP "floor(SynergyBonus/2)" @?=
                  Function (BuiltIn "floor")
                               [Function Divide [Variable "SynergyBonus",
                                                 Number 2]]

testNestedInfixFunc = parseJEP "max(0,Reputation-INT)" @?=
                      Function (BuiltIn "max")
                                   [Number 0,
                                    Function Subtract
                                                 [Variable "Reputation",
                                                  Variable "INT"]]

testVarFunc = parseJEP "var(\"SKILL.Perception (Dim Light).MISC\")" @?=
              LookupVariable "SKILL.Perception (Dim Light).MISC"

testSkillInfoFunc = do
  (parseJEP "skillinfo(\"TOTALRANK\", \"Perception\")" @?=
   LookupSkill (TOTALRANK, "Perception"))
  (parseJEP "skillinfo(\"TOTAL\",\"Martial Arts\")" @?=
   LookupSkill (TOTAL, "Martial Arts"))

-- max(0,floor((var("SKILLRANK=Tumble")-5)/20))*SynergyBonus
-- max(0,floor((var("SKILLRANK=Concentration")-5)/20))*SynergyBonus

tests :: Test
tests = TestList [ "parse integer" ~: testInt
                 , "parse unsigned integer" ~: testSignedInt
                 , "parse var(...) function" ~: testVarFunc
                 , "parse skillinfo(...) function" ~: testSkillInfoFunc
                 , "parse function with variable" ~: testFuncWithVar
                 , "parse function with nested infix function" ~: testNestedInfixFunc
                 ]
