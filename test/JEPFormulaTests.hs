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
                    [ Function Divide [ Variable "SynergyBonus"
                                      , Number 2 ] ]

testNestedInfixFunc = parseJEP "max(0,Reputation-INT)" @?=
                      Function (BuiltIn "max")
                        [ Number 0
                        , Function Subtract
                          [ Variable "Reputation"
                          , Variable "INT" ] ]

testVarFunc = parseJEP "var(\"SKILL.Perception (Dim Light).MISC\")" @?=
              LookupVariable "SKILL.Perception (Dim Light).MISC"

testSkillInfoFunc = do
  parseJEP "skillinfo(\"TOTALRANK\", \"Perception\")" @?=
    LookupSkill (TOTALRANK, "Perception")
  parseJEP "skillinfo(\"TOTAL\",\"Martial Arts\")" @?=
    LookupSkill (TOTAL, "Martial Arts")

testNestedFunc = parseJEP "floor((var(\"MOVE[Walk]\")-30)/10)*4" @?=
                 Function Multiply [
                   Function (BuiltIn "floor")
                     [ Function Divide
                       [ Group
                         (Function Subtract [ LookupVariable "MOVE[Walk]"
                                            , Number 30 ])
                         , Number 10 ] ]
                   , Number 4 ]

testNestedFunc2 = parseJEP "max(floor((var(\"SKILLRANK=Concentration\")-5)/20))*SynergyBonus" @?=
                  Function Multiply
                    [ Function (BuiltIn "max")
                      [ Function (BuiltIn "floor")
                        [ Function Divide
                          [ Group
                            (Function Subtract [LookupVariable "SKILLRANK=Concentration"
                                               , Number 5])
                          , Number 20 ] ] ]
                    , Variable "SynergyBonus"]

formulaTests :: Test
formulaTests = TestList [ "parse integer" ~: testInt
                        , "parse unsigned integer" ~: testSignedInt
                        , "parse var(...) function" ~: testVarFunc
                        , "parse skillinfo(...) function" ~: testSkillInfoFunc
                        , "parse function with variable" ~: testFuncWithVar
                        , "parse function with nested groups" ~: testNestedFunc
                        , "parse function with more nested groups" ~: testNestedFunc2
                        , "parse function with nested infix function" ~: testNestedInfixFunc
                        ]
