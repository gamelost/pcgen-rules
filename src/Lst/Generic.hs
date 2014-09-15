{-# LANGUAGE OverloadedStrings #-}

module Lst.Generic where

import Prelude hiding (takeWhile)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Modifications
import Restrictions
import Common

-- generic lst placeholder while we implement specific lst types

data LSTDefinition = Name String
                   | Key (String, String)
                   | Restricted Restriction
                     deriving Show

parseName :: PParser String
parseName = manyTill anyChar $ noneOf "\t\n\r"

parseLSTTag :: PParser LSTDefinition
parseLSTTag = do
  a <- allCaps
  v <- char ':' >> parseName
  return $ Key (a, v)

parseGenericLSTLine :: String -> PParser [LSTDefinition]
parseGenericLSTLine name = do
  keys <- parseLSTTag `sepBy` tabs
  return $ keys ++ [Name name]

instance LSTObject LSTDefinition where
  parseLine = parseGenericLSTLine
