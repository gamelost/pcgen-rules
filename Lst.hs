{-# LANGUAGE OverloadedStrings #-}

module Lst where

import Prelude hiding (takeWhile)
import Control.Monad(liftM)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Util

type Attribute = (T.Text, T.Text)
type Sequence = (T.Text, [T.Text])
type Progression = (Int, [Sequence])

data LSTTag = LSTDefinition T.Text [Attribute]
            | LSTStandalone Attribute
            | LSTProgression Progression
            | LSTDeleted T.Text
            | LSTComment T.Text deriving Show

type LSTTags = [LSTTag]

tabs :: Parser ()
tabs = skipWhile (== '\t')

parseComment :: Parser LSTTag
parseComment = do
  c <- commentedLine
  return $ LSTComment c

parseDescriber :: Parser T.Text
parseDescriber = takeWhile1 (/= '\t')

parseName :: Parser T.Text
parseName = takeWhile1 $ notInClass "\t\n\r"

parseAttribute :: Parser Attribute
parseAttribute = do
  a <- allCaps
  v <- ":" .*> parseName
  return (a, v)

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

parseLSTStandalone :: Parser LSTTag
parseLSTStandalone = liftM LSTStandalone parseAttribute

parseLSTData :: Parser LSTTag
parseLSTData = do
  describer <- parseDescriber <* tabs
  attributes <- parseAttribute `sepBy1` tabs
  return $ LSTDefinition describer attributes

parseLSTLine :: Parser LSTTags
parseLSTLine = many $ (parseComment <|>
                       parseLSTDeleted <|>
                       parseLSTSequence <|>
                       parseLSTEmptySequence <|>
                       parseLSTData <|>
                       parseLSTStandalone) <* many endOfLine

parseLST :: FilePath -> IO LSTTags
parseLST lstName = do
  contents <- readContents lstName
  print $ "** parsing LST " ++ lstName
  let result = parse parseLSTLine contents in
    return $ parseResult lstName result
