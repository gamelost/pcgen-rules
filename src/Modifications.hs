{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Modifications where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Common
import Data.Maybe

data Operation = Add | Copy | Modify | Forget deriving Show

data LSTLine a = LSTLine { operation :: Operation
                         , tags :: [a] }
                 deriving Show

class LSTObject a where
  parseLine :: T.Text -> Parser [a]

  parseLSTLine :: Parser (LSTLine a)
  parseLSTLine = do
    (name, operation) <- parseStart <* tabs
    tags <- parseLine name
    _ <- endOfLine <|> endOfInput
    return LSTLine { .. }

parseStart :: Parser (T.Text, Operation)
parseStart = do
  what <- parseString
  return . fromJust $ matchSuffixes what where
    matchSuffixes str = (\x -> (x, Modify)) <$> T.stripSuffix str ".MOD"
                    <|> (\x -> (x, Forget)) <$> T.stripSuffix str ".FORGET"
                    <|> (\x -> (x, Copy)) <$> T.stripSuffix str ".COPY"
                    <|> return (str, Add)
