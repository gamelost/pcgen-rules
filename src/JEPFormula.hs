{-# LANGUAGE OverloadedStrings #-}

module JEPFormula ( Formula(..)
                  , Operand(..)
                  , SkillType(..)
                  , Roll(..)
                  , parseFormula
                  , parseQuotedString
                  , parseRoll
                  , evalJEPFormula
                  ) where

import qualified Data.Text as T
import qualified Data.Map as M

import Text.Parsec.Char (char, space, anyChar, satisfy)
import Text.Parsec.Combinator (manyTill, choice, sepBy, optional, option)
import Text.Parsec.Expr (Assoc(..), Operator(..), buildExpressionParser)
import Text.Parsec.Prim (many, try)
import Control.Monad.State (State, get, msum)
import ClassyPrelude hiding (try, minimum, maximum, head)
import Prelude(minimum, maximum, head, read)

import Common

data Roll = Roll { number :: Int
                 , sides :: Int
                 , modifier :: Int }
            deriving (Show, Eq)

data Operand = Divide
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
             | Floating Float
             | Variable String
             | LookupSkill (SkillType, String)
             | Function String [Formula]
             | Group Formula
             | Negate Formula
             | Arithmetic Operand Formula Formula
               deriving (Show, Eq)

-- since we don't fully parse all variables yet, hard-code some to get
-- past the parser verification process
varBuiltins :: [String]
varBuiltins = [ "SynergyBonus"
              , "Reputation"
              , "DomainLVL"
              , "DomainPowerTimes"
              , "DomainAbilityTriggerLVL"
              , "Insanity"
              , "AlignmentAuraBase"
              , "ATWILL"
              , "SHIELDACCHECK"
              , "TL"
              , "CL"
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
evalJEPFormulae _ (Floating x) = toRational x
evalJEPFormulae _ (LookupSkill _) =
  warning "evaluating skillinfo() is not implemented"
  0
evalJEPFormulae vars (Negate f) =
  negate $ evalJEPFormulae vars f
evalJEPFormulae vars (Arithmetic op f1 f2) =
  (case op of
     Divide -> (/)
     Multiply -> (*)
     Subtract -> (-)
     Add -> (+))
  (evalJEPFormulae vars f1)
  (evalJEPFormulae vars f2)
evalJEPFormulae vars (Function what formulas) =
  let f = builtInFunction what in
  let args = map (evalJEPFormulae vars) formulas in
  f args
evalJEPFormulae vars (Group f) = evalJEPFormulae vars f
evalJEPFormulae vars (Variable v) =
  case M.lookup v vars of
    Just n -> toRational n
    Nothing ->
      (warning $ "variable \"" ++ v ++ "\" was not found")
      0

parseRoll :: PParser Roll
parseRoll = do
  number <- textToInt <$> manyNumbers
  _ <- optional $ char 'd'
  sides <- option 0 $ textToInt <$> manyNumbers
  _ <- optional $ char '+'
  modifier <- option 0 $ textToInt <$> manyNumbers
  return Roll { .. }

parseNumber :: PParser Formula
parseNumber = Number <$> parseSignedNumber where
  parseSignedNumber = sign <*> (textToInt <$> manyNumbers)
  sign = (negate <$ char '-') <|> (id <$ optional (char '+'))

parseFloat :: PParser Formula
parseFloat = Floating <$> parseSignedNumber where
  parseSignedNumber = sign <*> parseFloating
  sign = (negate <$ char '-') <|> (id <$ optional (char '+'))
  parseFloating = do
    n <- manyNumbers
    d <- char '.'
    r <- manyNumbers
    return $ rd (n ++ d : r)
  rd = read :: String -> Float

parseVariable :: PParser Formula
parseVariable = Variable <$> variableParsers

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

parseFunction :: PParser Formula
parseFunction = do
  f <- choice functionParsers
  args <- char '(' >> parseFormula `sepBy` char ',' <* char ')'
  return $ Function f args

table :: [[Operator T.Text () (State Variables) Formula]]
table = [ [ Prefix negateFormula ]
        , [ Infix multiplyFormula AssocLeft, Infix divideFormula AssocLeft ]
        , [ Infix addFormula AssocLeft, Infix subtractFormula AssocLeft ]
        ] where
  multiplyFormula = operand '*' Multiply
  subtractFormula = operand '-' Subtract
  divideFormula = operand '/' Divide
  addFormula = operand '+' Add
  operand c o = try $ Arithmetic o <$ char c
  negateFormula = try $ Negate <$ char '-'

parseFormula :: PParser Formula
parseFormula = buildExpressionParser table parseExpression
-- parseFormula = _traceFormula

parseExpression :: PParser Formula
parseExpression = try parseFunction
              <|> try parseGroup
              <|> try parseVarFunction
              <|> try parseSkillInfoFunction
              <|> try parseFloat
              <|> try parseNumber
              <|> try parseVariable

_traceFormula :: PParser Formula
_traceFormula = do
  v <- manyTill anyChar $ satisfy (\x -> x == '|' || x == '\t' || x == '\r' || x == '\n')
  _ <- trace ("** Formula was " ++ v) $ return ()
  return $ Variable v
