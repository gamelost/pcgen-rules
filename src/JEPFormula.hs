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
             | LookupSkill (SkillType, String)
             | Group Formula
             | Function Operand [Formula]
               deriving (Show, Eq)

-- since we don't fully parse all variables yet, hard-code some to get
-- past the parser verification process
varBuiltins :: [String]
varBuiltins = [ "SynergyBonus"
              , "Reputation"
              , "DomainLVL"
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

-- NB: PCGen rounds down to the nearest integer the results of each
-- formula. The problem is, of course, determining *when* exactly it
-- does this...
evalJEPFormula :: Variables -> Formula -> Int
evalJEPFormula vars f = floor $ evalJEPFormulae vars f

fdivide :: [Rational] -> Rational
fdivide arr = head arr * product (map (\x -> 1/x) $ tail arr)

fsubtract :: [Rational] -> Rational
fsubtract arr = head arr + sum (map negate $ tail arr)

-- Because of the way we parse the infix functions, we are
-- guaranteed there are only two parameters.
actualFunction :: Operand -> [Rational] -> Rational
actualFunction Divide = fdivide
actualFunction Multiply = product
actualFunction Subtract = fsubtract
actualFunction Add = sum
-- Built-in functions, however, can have any arity.
actualFunction (BuiltIn f) = builtInFunction f

builtInFunction :: String -> [Rational] -> Rational
builtInFunction "min" = minimum
builtInFunction "max" = maximum
builtInFunction "floor" = \x ->
  case length x of
    1 -> (toRational :: Int -> Rational) . floor $ head x
    _ -> error "floor was called with incorrect arity"
builtInFunction "ceil" = \x ->
  case length x of
    1 -> (toRational :: Int -> Rational) . ceiling $ head x
    _ -> error "ceil was called with incorrect arity"
builtInFunction _ = error "No such built-in function"

evalJEPFormulae :: Variables -> Formula -> Rational
evalJEPFormulae _ (Number x) = toRational x
evalJEPFormulae _ (LookupSkill _) =
  warning "evaluating skillinfo() is not implemented"
  0
evalJEPFormulae vars (Function operand formulas) =
  let f = actualFunction operand in
  let args = map (evalJEPFormulae vars) formulas in
  f args
evalJEPFormulae vars (Group f) = evalJEPFormulae vars f
evalJEPFormulae vars (Variable v) =
  case M.lookup v vars of
    Just n -> toRational n
    Nothing ->
      (warning $ "variable \"" ++ v ++ "\" was not found")
      0

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

-- since the var() function just allows the variable to contain
-- characters which are not valid JEP syntax, we just treat it as a
-- regular variable.
parseVarFunction :: PParser Formula
parseVarFunction = Variable <$> (labeled "var(" >> parseQuotedString <* labeled ")")

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
