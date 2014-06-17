{-# LANGUAGE OverloadedStrings #-}

module Lst.Generic where

import Prelude hiding (takeWhile)
import Control.Monad(liftM)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Common

-- generic lst placeholders while we implement specific lst types

type Attribute = (T.Text, T.Text)
type Sequence = (T.Text, [T.Text])
type Progression = (Int, [Sequence])

data LSTTag = LSTDefinition T.Text [Attribute]
            | LSTAttributes [Attribute]
            | LSTAttribute Attribute
            | LSTStandalone T.Text
            | LSTProgression Progression
            | LSTDeleted T.Text
            | LSTMod T.Text Attribute deriving Show

parseDescriber :: Parser T.Text
parseDescriber = takeWhile1 (\c -> c /= '\t' && c /= ':')

parseName :: Parser T.Text
parseName = takeWhile1 $ notInClass "\t\n\r"

parseAttribute :: Parser Attribute
parseAttribute = do
  a <- allCaps
  v <- ":" .*> parseName
  return (a, v)

--- we may want (eventually) to embed this into a data structure
diceRoll :: Parser T.Text
diceRoll = do
  rolls <- manyNumbers
  d <- string "d"
  faces <- manyNumbers
  return $ T.concat [rolls, d, faces]

parseSequenceCommas :: Parser [T.Text]
parseSequenceCommas = (diceRoll <|> manyNumbers <|> parseName) `sepBy1` ","

parseSequence :: Parser Sequence
parseSequence = do
  a <- allCaps
  v <- ":" .*> parseSequenceCommas
  return (a, v)

parseLSTSequence :: Parser LSTTag
parseLSTSequence = do
  n <- manyNumbers <* tabs
  seqs <- some $ parseSequence <* tabs
  return $ LSTProgression (read $ T.unpack n :: Int, seqs)

parseLSTEmptySequence :: Parser LSTTag
parseLSTEmptySequence = do
  n <- manyNumbers <* tabs
  return $ LSTProgression (read $ T.unpack n :: Int, [])

parseLSTDeleted :: Parser LSTTag
parseLSTDeleted = do
  x <- eatTillPeriod <*. ".FORGET"
  return $ LSTDeleted x
  where eatTillPeriod = takeWhile1 $ notInClass "."

parseLSTMod :: Parser LSTTag
parseLSTMod = do
  x <- eatTillPeriod <*. ".MOD" <* tabs
  a <- parseAttribute
  return $ LSTMod x a
  where eatTillPeriod = takeWhile1 $ notInClass "."

parseLSTAttribute :: Parser LSTTag
parseLSTAttribute = liftM LSTAttribute parseAttribute

parseLSTStandalone :: Parser LSTTag
parseLSTStandalone = do
  s <- takeWhile1 $ notInClass ":\n\r\t"
  return $ LSTStandalone s

parseLSTData :: Parser LSTTag
parseLSTData = do
  describer <- parseDescriber <* tabs
  attributes <- parseAttribute `sepBy1` tabs <* tabs
  return $ LSTDefinition describer attributes

parseLSTAttributes :: Parser LSTTag
parseLSTAttributes = do
  attributes <- parseAttribute `sepBy1` tabs
  return $ LSTAttributes attributes

parseGenericLine :: Parser LSTTag
parseGenericLine = parseLSTDeleted <|>
                   parseLSTSequence <|>
                   parseLSTEmptySequence <|>
                   parseLSTAttributes <|>
                   parseLSTData <|>
                   parseLSTAttribute <|>
                   parseLSTMod <|>
                   parseLSTStandalone
