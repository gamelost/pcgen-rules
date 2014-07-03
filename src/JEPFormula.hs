{-# LANGUAGE OverloadedStrings #-}

module JEPFormula where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Common

-- simplistic, for now
data Formula = Variable T.Text
             | Number Int
               deriving Show

-- since we don't have variables parsed yet, add some to get past the
-- parser verification process
listOfVars :: [String]
listOfVars = ["SynergyBonus"]

parseNumber :: Parser Formula
parseNumber = Number <$> parseSignedNumber where
  parseSignedNumber = sign <*> (textToInt <$> manyNumbers)
  sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)

parseVariable :: Parser Formula
parseVariable = Variable <$> choice varParsers where
  varParsers = map (string . T.pack) listOfVars

parseFormula :: Parser Formula
parseFormula = parseNumber <|> parseVariable
