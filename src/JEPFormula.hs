{-# LANGUAGE OverloadedStrings #-}

module JEPFormula ( Formula(..)
                  , Operand(..)
                  , SkillType(..)
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

data SkillType = RANK
               | TOTALRANK
               | MODIFIER
               | STAT
               | MISC
               | TOTAL
                 deriving (Show, Eq)

data Formula = Number Int
             | Variable T.Text
             | LookupVariable T.Text
             | LookupSkill (SkillType, T.Text)
             | Group Formula
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

parseGroup :: Parser Formula
parseGroup = do
  Group <$> (char '(' >> parseFormula <* char ')')

-- may want to make sure there are no unterminated quotes!
parseQuotedString :: Parser T.Text
parseQuotedString = char '"' *> takeTill (== '"') <* char '"'

-- treat the var() function specially
parseVarFunction :: Parser Formula
parseVarFunction = LookupVariable <$> (string "var(" >> parseQuotedString <* string ")")

-- treat the skillinfo() function specially
parseSkillInfoFunction :: Parser Formula
parseSkillInfoFunction = do
  prop <- string "skillinfo(" *> parseQuotedString
  _ <- char ',' >> many space
  var <- parseQuotedString <* char ')'
  return $ LookupSkill (parseProperty prop, var) where
    parseProperty "RANK" = RANK
    parseProperty "TOTALRANK" = TOTALRANK
    parseProperty "MODIFIER" = MODIFIER
    parseProperty "STAT" = STAT
    parseProperty "MISC" = MISC
    parseProperty "TOTAL" = TOTAL
    parseProperty _ = error "No such skillinfo property"

parseInfixFunction :: Parser Formula
parseInfixFunction = do
  -- only support infix 2 for now
  first <- parseVariable <|> parseNumber <|> parseVarFunction <|> parseFunction <|> parseGroup
  op <- many space >> choice (map char "/*-+") <* many space
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
parseFormula = parseInfixFunction
           <|> parseFunction
           <|> parseGroup
           <|> parseVarFunction
           <|> parseSkillInfoFunction
           <|> parseNumber
           <|> parseVariable
-- parseFormula = traceFormula

traceFormula :: Parser Formula
traceFormula = do
  v <- takeTill (\x -> x == '|' || x == '\t' || x == '\r' || x == '\n')
  _ <- trace ("** Formula was " ++ T.unpack v) $ return ()
  return $ Variable v
