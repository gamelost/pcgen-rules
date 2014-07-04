{-# LANGUAGE OverloadedStrings #-}

module JEPFormula ( Formula(..)
                  , Operand(..)
                  , parseFormula
                  ) where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Common
import Debug.Trace(trace)

data Operand = BuiltIn T.Text
             | Divide
             | Multiply
             | Subtract
             | Add
               deriving (Show, Eq)

data Formula = Variable T.Text
             | LookupVariable T.Text
             | Number Int
             | Function Operand [Formula]
               deriving (Show, Eq)

-- since we don't have variables yet, hard-code some to get past the
-- parser verification process
listOfVars :: [String]
listOfVars = [ "SynergyBonus"
             , "Reputation"
             , "INT" -- should this be an attribute instead?
             ]

listOfFunctions :: [String]
listOfFunctions = [ "floor"
                  , "max"
                  ]

parseNumber :: Parser Formula
parseNumber = Number <$> parseSignedNumber where
  parseSignedNumber = sign <*> (textToInt <$> manyNumbers)
  sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)

parseVariable :: Parser Formula
parseVariable = Variable <$> choice varParsers where
  varParsers = map (string . T.pack) listOfVars

-- treat the var() function specially
parseVarFunction :: Parser Formula
parseVarFunction = LookupVariable <$> (string "var(\"" >> parseString <* string "\")")

parseInfixFunction :: Parser Formula
parseInfixFunction = do
  -- only support infix 2 for now
  first <- parseVariable <|> parseNumber
  op <- choice $ map char ['/', '*', '-', '+']
  second <- parseVariable <|> parseNumber
  return $ Function (operandMap op) [first, second] where
    operandMap :: Char -> Operand
    operandMap '/' = Divide
    operandMap '*' = Multiply
    operandMap '-' = Subtract
    operandMap '+' = Add
    operandMap _ = error "No such infix function"

parseFunction :: Parser Formula
parseFunction = do
  f <- BuiltIn <$> choice funcParsers
  args <- char '(' >> parseFormula `sepBy` char ',' <* char ')'
  return $ Function f args where
    funcParsers = map (string . T.pack) listOfFunctions

parseFormula :: Parser Formula
parseFormula = parseFunction
           <|> parseInfixFunction
           <|> parseVarFunction
           <|> parseNumber
           <|> parseVariable
-- parseFormula = traceFormula

traceFormula :: Parser Formula
traceFormula = do
  v <- takeTill (== '|')
  _ <- trace ("** Formula was " ++ T.unpack v) $ return ()
  return $ Variable v
