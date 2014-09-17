{-# LANGUAGE OverloadedStrings #-}

module JEPFormula ( Formula(..)
                  , Operand(..)
                  , SkillType(..)
                  , parseFormula
                  , parseQuotedString
                  , evalJEPFormula
                  ) where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding ((<|>))
import Control.Applicative hiding (optional, many)
import Control.Monad.State
import qualified Data.Map as M
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
varBuiltins :: [String]
varBuiltins = [ "SynergyBonus"
              , "Reputation"
              , "INT"
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

getVariables :: PParser [String]
getVariables = do
  vars <- get
  return $ M.keys vars ++ varBuiltins

variableParsers :: PParser String
variableParsers = msum . tryStrings =<< getVariables

functionParsers :: [PParser String]
functionParsers = tryStrings listOfFunctions

-- for now.
evalJEPFormula :: Formula -> Int
evalJEPFormula f = 0

parseNumber :: PParser Formula
parseNumber = Number <$> parseSignedNumber where
  parseSignedNumber = sign <*> (textToInt <$> manyNumbers)
  sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)

parseVariable :: PParser Formula
parseVariable = Variable <$> variableParsers

-- ugly; unfortunately, this does show up.
parseNegativeVariable :: PParser Formula
parseNegativeVariable = char '-' >> variableParsers >>= embed where
    embed v = return $ Function Subtract [ Number 0, Variable v ]

parseGroup :: PParser Formula
parseGroup = Group <$> (char '(' >> parseFormula <* char ')')

-- we treat all known unquoted variables specially (for now) -- not
-- sure what they refer to, just yet.
--
-- NB: may want to make sure there are no unterminated quotes!
parseQuotedString :: PParser String
parseQuotedString = labeled "ARMOR.0.ACCHECK"
                <|> labeled "SPELLSTAT"
                <|> char '"' *> untilQuote where
  untilQuote = manyTill anyChar $ satisfy (== '"')

-- treat the var() function specially
parseVarFunction :: PParser Formula
parseVarFunction = LookupVariable <$> (labeled "var(" >> parseQuotedString <* labeled ")")

-- treat the skillinfo() function specially
parseSkillInfoFunction :: PParser Formula
parseSkillInfoFunction = do
  prop <- labeled "skillinfo(" *> parseQuotedString
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

parseInfixFunction :: PParser Formula
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
    parsers = try parseVariable
          <|> try parseNegativeVariable
          <|> try parseNumber
          <|> try parseVarFunction
          <|> try parseSkillInfoFunction
          <|> try parseFunction
          <|> parseGroup

parseFunction :: PParser Formula
parseFunction = do
  f <- BuiltIn <$> choice functionParsers
  args <- char '(' >> parseFormula `sepBy` char ',' <* char ')'
  return $ Function f args

parseFormula :: PParser Formula
parseFormula = try parseInfixFunction
           <|> try parseFunction
           <|> try parseGroup
           <|> try parseVarFunction
           <|> try parseSkillInfoFunction
           <|> try parseNumber
           <|> try parseVariable
           <|> parseNegativeVariable
-- parseFormula = _traceFormula

_traceFormula :: PParser Formula
_traceFormula = do
  v <- manyTill anyChar $ satisfy (\x -> x == '|' || x == '\t' || x == '\r' || x == '\n')
  _ <- trace ("** Formula was " ++ v) $ return ()
  return $ Variable v
