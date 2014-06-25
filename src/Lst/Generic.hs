{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lst.Generic where

import Prelude hiding (takeWhile)
import Control.Monad(liftM)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Modifications
import Common

-- generic lst placeholder while we implement specific lst types

data LSTDefinition = LSTDefinition { name :: T.Text
                                   , keys :: [(T.Text, T.Text)] } deriving Show

parseName :: Parser T.Text
parseName = takeWhile1 $ notInClass "\t\n\r"

parseLSTTab :: Parser (T.Text, T.Text)
parseLSTTab = do
  a <- allCaps
  v <- ":" .*> parseName
  return (a, v)

parseGenericLSTLine :: T.Text -> Parser LSTDefinition
parseGenericLSTLine name = do
  keys <- parseLSTTab `sepBy` tabs
  return LSTDefinition { .. }

instance LSTObject LSTDefinition where
  parseLine = parseGenericLSTLine
