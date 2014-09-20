module JEPFormulaTests where

import qualified Data.Map.Strict as M
import Text.Parsec.Prim
import Control.Applicative
import Test.HUnit
import JEPFormula
import Common

parseJEP :: String -> Formula
parseJEP = parseResult parseFormula "parseJEP"

parseQS :: String -> String
parseQS = parseResult parseQuotedString "parseQS"

testQS = parseQS "\"SKILL.Perception (Dim Light).MISC\"" @?=
         "SKILL.Perception (Dim Light).MISC"

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
              Variable "SKILL.Perception (Dim Light).MISC"

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
                         (Function Subtract [ Variable "MOVE[Walk]"
                                            , Number 30 ])
                         , Number 10 ] ]
                   , Number 4 ]

testNestedFunc2 = parseJEP "max(floor((var(\"SKILLRANK=Concentration\")-5)/20))*SynergyBonus" @?=
                  Function Multiply
                    [ Function (BuiltIn "max")
                      [ Function (BuiltIn "floor")
                        [ Function Divide
                          [ Group
                            (Function Subtract [ Variable "SKILLRANK=Concentration"
                                               , Number 5])
                          , Number 20 ] ] ]
                    , Variable "SynergyBonus"]

testMisc = do
  parseJEP "(1+skillinfo(\"TOTAL\",\"Martial Arts\"))/2" @?=
    Function Divide
      [ Group (Function Add
          [ Number 1
          , LookupSkill ( TOTAL, "Martial Arts" ) ] )
      , Number 2 ]
  parseJEP "skillinfo(\"MODIFIER\", \"Jump\")-STR" @?=
    Function Subtract
      [ LookupSkill ( MODIFIER , "Jump" )
      , Variable "STR" ]
  parseJEP "INT-DEX" @?=
    Function Subtract [ Variable "INT", Variable "DEX" ]
  parseJEP "-CHA+max(CHA,STR)" @?=
    Function Add
      [ Function Subtract [ Number 0, Variable "CHA"]
      , Function (BuiltIn "max") [ Variable "CHA",Variable "STR" ] ]

-- "max(floor((var(\"SKILLRANK=Concentration\")-5)/20))*SynergyBonus"
testEvaluation = evalJEPFormula
                 (M.fromList [  ("SKILLRANK=Concentration", 20)
                             , ("SynergyBonus", 2) ])
                 (Function Multiply
                    [ Function (BuiltIn "max")
                      [ Function (BuiltIn "floor")
                        [ Function Divide
                          [ Group
                            (Function Subtract [ Variable "SKILLRANK=Concentration"
                                               , Number 5])
                          , Number 20 ] ] ]
                    , Variable "SynergyBonus"]) @?=
                 0

formulaTests :: Test
formulaTests = TestList [ "parse integer" ~: testInt
                        , "parse unsigned integer" ~: testSignedInt
                        , "parse var(...) function" ~: testVarFunc
                        , "parse skillinfo(...) function" ~: testSkillInfoFunc
                        , "parse function with variable" ~: testFuncWithVar
                        , "parse function with nested groups" ~: testNestedFunc
                        , "parse function with more nested groups" ~: testNestedFunc2
                        , "parse function with nested infix function" ~: testNestedInfixFunc
                        , "parse miscellaneous formulas" ~: testMisc
                        , "parse quoted string" ~: testQS
                        , "evaluate formula" ~: testEvaluation
                        ]
