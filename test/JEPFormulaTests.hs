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
                Negate (Number 4)

testFuncWithVar = parseJEP "floor(SynergyBonus/2)" @?=
                  Function "floor"
                    [ Arithmetic Divide (Variable "SynergyBonus")
                                        (Number 2) ]

testNestedInfixFunc = parseJEP "max(0,Reputation-INT)" @?=
                      Function "max"
                        [ Number 0
                        , Arithmetic Subtract
                          (Variable "Reputation")
                          (Variable "INT") ]

testVarFunc = parseJEP "var(\"SKILL.Perception (Dim Light).MISC\")" @?=
              Variable "SKILL.Perception (Dim Light).MISC"

testSkillInfoFunc = do
  parseJEP "skillinfo(\"TOTALRANK\", \"Perception\")" @?=
    LookupSkill (TOTALRANK, "Perception")
  parseJEP "skillinfo(\"TOTAL\",\"Martial Arts\")" @?=
    LookupSkill (TOTAL, "Martial Arts")

testNestedFunc = parseJEP "floor((var(\"MOVE[Walk]\")-30)/10)*4" @?=
                 Arithmetic Multiply
                   (Function "floor"
                     [ Arithmetic Divide
                       (Group
                         (Arithmetic Subtract (Variable "MOVE[Walk]")
                                              (Number 30)))
                         (Number 10) ])
                   (Number 4)

testNestedFunc2 = parseJEP "max(floor((var(\"SKILLRANK=Concentration\")-5)/20))*SynergyBonus" @?=
                  Arithmetic Multiply
                    (Function "max"
                      [ Function "floor"
                        [ Arithmetic Divide
                          (Group
                            (Arithmetic Subtract (Variable "SKILLRANK=Concentration")
                                                 (Number 5)))
                          (Number 20) ] ])
                    (Variable "SynergyBonus")

testMisc = do
  parseJEP "(1+skillinfo(\"TOTAL\",\"Martial Arts\"))/2" @?=
    Arithmetic Divide
      (Group (Arithmetic Add
        (Number 1)
        (LookupSkill ( TOTAL, "Martial Arts" ))))
      (Number 2)
  parseJEP "skillinfo(\"MODIFIER\", \"Jump\")-STR" @?=
    Arithmetic Subtract
      (LookupSkill ( MODIFIER , "Jump" ))
      (Variable "STR")
  parseJEP "INT-DEX" @?=
    Arithmetic Subtract (Variable "INT") (Variable "DEX")
  parseJEP "-CHA+max(CHA,STR)" @?=
    Arithmetic Add
      (Negate (Variable "CHA"))
      (Function "max" [Variable "CHA",Variable "STR"])

-- "max(floor((var(\"SKILLRANK=Concentration\")-5)/20))*SynergyBonus"
testEvaluation = evalJEPFormula
                 (M.fromList [  ("SKILLRANK=Concentration", 20)
                             , ("SynergyBonus", 2) ])
                 (Arithmetic Multiply
                    (Function "max"
                      [ Function "floor"
                        [ Arithmetic Divide
                          (Group
                            (Arithmetic Subtract (Variable "SKILLRANK=Concentration") (Number 5)))
                          (Number 20) ] ])
                    (Variable "SynergyBonus")) @?=
                 0

testNArity = parseJEP "10+(STR/2)+WIS" @?=
    Arithmetic Add
      (Arithmetic Add
         (Number 10)
         (Group (Arithmetic Divide (Variable "STR") (Number 2))))
      (Variable "WIS")

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
                        , "parse n-arity" ~: testNArity
                        , "evaluate formula" ~: testEvaluation
                        ]
