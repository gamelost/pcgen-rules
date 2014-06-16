{-# LANGUAGE OverloadedStrings #-}

module Restrictions where

import Prelude hiding (takeWhile, GT, EQ, LT)
import Control.Monad(liftM)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Common

data Operator = EQ | GT | GTEQ | LT | LTEQ | NEQ deriving Show

data PreVar = PreVar { operator :: Operator
                     , definition :: T.Text
                     , comparator :: Int } deriving Show

data PreClass = PreClass { passNumber :: Int
                         , classRequisites :: [(T.Text, Int)] } deriving Show

data PreAbility = PreAbility { abilityNumber :: Int
                             , categoryName :: T.Text
                             , abilities :: [T.Text] } deriving Show

data Restriction = PreClassRestriction PreClass
                 | PreVarRestriction PreVar
                 | PreAbilityRestriction PreAbility deriving Show

textToInt :: T.Text -> Int
textToInt t = read (T.unpack t) :: Int

parseEqual :: Parser (T.Text, Int)
parseEqual = do
  x <- parseString
  _ <- char '='
  n <- manyNumbers
  return (x, textToInt n)

-- PRECLASS:x,y=z,y=z,y=z...
--   x is number of classes to pass
--   y is class name or class type (TYPE.y) or SPELLCASTER. or SPELLCASTER.y
--   z is number, class level
parsePreClass :: Parser PreClass
parsePreClass = do
  _ <- string "PRECLASS:"
  n <- manyNumbers
  _ <- char ','
  classes <- parseEqual `sepBy` char ','
  return PreClass { passNumber = textToInt n
                  , classRequisites = classes }

convertOperator :: T.Text -> Operator
convertOperator "EQ" = EQ
convertOperator "GT" = GT
convertOperator "GTEQ" = GTEQ
convertOperator "LT" = LT
convertOperator "LTEQ" = LTEQ
convertOperator "NEQ" = NEQ
convertOperator _ = error "incorrect PREVAR operator"

-- PREVARx:y,z
--   x is EQ, GT, GTEQ, LT, LTEQ, NEQ
--   y is text (must be in DEFINE: or BONUS:VAR)
--   z is number to be compared to
parsePreVar :: Parser PreVar
parsePreVar = do
  _ <- string "PREVAR"
  op <- choice ["EQ", "GT", "GTEQ", "LT", "LTEQ", "NEQ"]
  _ <- char ':'
  definition <- parseWord
  _ <- char ','
  n <- manyNumbers
  return PreVar { operator = convertOperator op
                , definition = definition
                , comparator = textToInt n }

-- PREABILITY:x,CATEGORY=y,z,z,z...
--   x is the number of abilities needed
--   y is category name or ALL
--   z is ability name, ability type (TYPE.z), or ALL
parsePreAbility :: Parser PreAbility
parsePreAbility = do
  _ <- string "PREABILITY:"
  n <- manyNumbers
  _ <- string ",CATEGORY="
  category <- parseWord
  _ <- char ','
  abilities <- parseString `sepBy` char ','
  return PreAbility { abilityNumber = textToInt n
                    , categoryName = category
                    , abilities = abilities }

-- restriction <- option Nothing parseRestriction <* tabs
parseRestriction = liftM Just (liftM PreClassRestriction parsePreClass <|>
                               liftM PreVarRestriction parsePreVar <|>
                               liftM PreAbilityRestriction parsePreAbility)
