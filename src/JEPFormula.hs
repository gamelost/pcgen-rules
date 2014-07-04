{-# LANGUAGE OverloadedStrings #-}

module JEPFormula ( Formula(..)
                  , parseFormula
                  ) where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Common
import Debug.Trace(trace)

-- simplistic, for now
data Formula = Variable T.Text
             | Number Int
               deriving (Show, Eq)

-- since we don't have variables parsed yet, add some to get past the
-- parser verification process
listOfVars :: [String]
listOfVars = [ "SynergyBonus"
             , "Reputation" ]

listOfFunctions :: [String]
listOfFunctions = [ "floor"
                  , "max"
                  , "var" ]

listOfAttributes :: [String]
listOfAttributes = [ "INT", "DEX", "CHA", "CON", "WIS", "STR" ]

parseNumber :: Parser Formula
parseNumber = Number <$> parseSignedNumber where
  parseSignedNumber = sign <*> (textToInt <$> manyNumbers)
  sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)

parseVariable :: Parser Formula
parseVariable = Variable <$> choice varParsers where
  varParsers = map (string . T.pack) listOfVars

parseFormula :: Parser Formula
parseFormula = parseNumber <|> parseVariable
-- parseFormula = traceFormula

traceFormula :: Parser Formula
traceFormula = do
  v <- takeTill (== '|')
  _ <- trace ("** Formula was " ++ T.unpack v) $ return ()
  return $ Variable v
