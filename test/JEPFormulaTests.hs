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

testParseInt = parseJEP "2" @?= Number 2

testParseSignedInt = parseJEP "-4" @?= Number (-4)

-- floor(SynergyBonus/2)
-- max(0,Reputation-INT)
-- max(0,floor((var("SKILLRANK=Tumble")-5)/20))*SynergyBonus
-- max(0,floor((var("SKILLRANK=Concentration")-5)/20))*SynergyBonus

tests :: Test
tests = TestList [ "parse integer" ~: testParseInt
                 , "parse unsigned integer" ~: testParseSignedInt
                 ]
