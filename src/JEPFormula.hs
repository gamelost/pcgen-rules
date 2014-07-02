{-# LANGUAGE OverloadedStrings #-}

module JEPFormula where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Common

-- simplistic, for now
data Formula = Variable T.Text
             | Number Int
               deriving Show

sign :: Parser (Int -> Int)
sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)

anyNumber :: Parser Int
anyNumber = sign <*> (textToInt <$> manyNumbers)

parseNumber :: Parser Formula
parseNumber = Number <$> anyNumber

parseFormula :: Parser Formula
parseFormula = parseNumber
