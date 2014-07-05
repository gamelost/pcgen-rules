{-# LANGUAGE OverloadedStrings #-}

module Lst.Generic where

import Prelude hiding (takeWhile)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Modifications
import Restrictions
import Common

-- generic lst placeholder while we implement specific lst types

data LSTDefinition = Name T.Text
                   | Key (T.Text, T.Text)
                   | Restricted Restriction
                     deriving Show

parseName :: Parser T.Text
parseName = takeWhile1 $ notInClass "\t\n\r"

parseLSTTag :: Parser LSTDefinition
parseLSTTag = do
  a <- allCaps
  v <- ":" .*> parseName
  return $ Key (a, v)

parseGenericLSTLine :: T.Text -> Parser [LSTDefinition]
parseGenericLSTLine name = do
  keys <- parseLSTTag `sepBy` tabs
  return $ keys ++ [Name name]

instance LSTObject LSTDefinition where
  parseLine = parseGenericLSTLine
