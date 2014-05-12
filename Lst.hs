{-# LANGUAGE OverloadedStrings #-}

module Lst where

import Prelude hiding (takeWhile)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Util

type Attribute = (T.Text, T.Text)

data LSTTag = LSTDefinition T.Text [Attribute]
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

parseAttribute :: Parser Attribute
parseAttribute = do
  a <- allCaps
  v <- ":" .*> eatTillDelimiter
  return (a, v)
  -- avoid a stupid hlint warning (it is confused about precedence)
  where eatTillDelimiter = takeWhile1 $ notInClass "\t\n\r"

parseLSTData :: Parser LSTTag
parseLSTData = do
  describer <- parseDescriber <* tabs
  attributes <- parseAttribute `sepBy1` tabs
  return $ LSTDefinition describer attributes

parseLSTLine :: Parser LSTTags
parseLSTLine = many $ (parseComment <|> parseLSTData) <* many endOfLine

parseLST :: FilePath -> IO LSTTags
parseLST lstName = do
  contents <- readContents lstName
  print $ "** parsing LST " ++ lstName
  let result = parse parseLSTLine contents in
    return $ parseResult lstName result
