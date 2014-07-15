{-# LANGUAGE OverloadedStrings #-}

module JEPFormula ( Formula(..)
                  , Operand(..)
                  , SkillType(..)
                  , parseFormula
                  ) where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Control.Applicative hiding (optional)
import Common
import Debug.Trace(trace)

data Operand = BuiltIn String
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
             | Variable String
             | LookupVariable String
             | LookupSkill (SkillType, String)
             | Group Formula
             | Function Operand [Formula]
               deriving (Show, Eq)

-- since we don't have variables yet, hard-code some to get past the
-- parser verification process
listOfVars :: [String]
listOfVars = [ "SynergyBonus"
             , "Reputation"
             , "INT" -- should these be attributes instead?
             , "DEX"
             , "STR"
             , "CON"
             , "WIS"
             , "CHA"
             ]

listOfFunctions :: [String]
listOfFunctions = [ "floor"
                  , "max"
                  , "min"
                  , "ceil"
                  ]

parseNumber :: Parser Formula
parseNumber = Number <$> parseSignedNumber where
  parseSignedNumber = sign <*> (textToInt <$> manyNumbers)
  sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)

parseVariable :: Parser Formula
parseVariable = Variable <$> choice varParsers where
  varParsers = map string listOfVars

-- ugly; unfortunately, this does show up.
parseNegativeVariable :: Parser Formula
parseNegativeVariable = char '-' >> choice varParsers >>= embed where
    varParsers = map string listOfVars
    embed v = return $ Function Subtract [ Number 0, Variable v ]

parseGroup :: Parser Formula
parseGroup = Group <$> (char '(' >> parseFormula <* char ')')

-- may want to make sure there are no unterminated quotes!
parseQuotedString :: Parser String
parseQuotedString = char '"' *> (manyTill anyChar $ satisfy (== '"')) <* char '"'

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
  first <- parsers
  op <- many space >> choice (map char "/*-+") <* many space
  second <- parsers
  return $ Function (operandMap op) [first, second] where
    operandMap :: Char -> Operand
    operandMap '/' = Divide
    operandMap '*' = Multiply
    operandMap '-' = Subtract
    operandMap '+' = Add
    operandMap _ = error "No such infix function"
    parsers = parseVariable
          <|> parseNegativeVariable
          <|> parseNumber
          <|> parseVarFunction
          <|> parseSkillInfoFunction
          <|> parseFunction
          <|> parseGroup

parseFunction :: Parser Formula
parseFunction = do
  f <- BuiltIn <$> choice funcParsers
  args <- char '(' >> parseFormula `sepBy` char ',' <* char ')'
  return $ Function f args where
    funcParsers = map string listOfFunctions

parseFormula :: Parser Formula
parseFormula = parseInfixFunction
           <|> parseFunction
           <|> parseGroup
           <|> parseVarFunction
           <|> parseSkillInfoFunction
           <|> parseNumber
           <|> parseVariable
           <|> parseNegativeVariable
-- parseFormula = _traceFormula

_traceFormula :: Parser Formula
_traceFormula = do
  v <- manyTill anyChar $ satisfy (\x -> x == '|' || x == '\t' || x == '\r' || x == '\n')
  _ <- trace ("** Formula was " ++ v) $ return ()
  return $ Variable v
