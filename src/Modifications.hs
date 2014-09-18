{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Modifications where

import Text.Parsec.Combinator(eof)
import Control.Applicative
import Common
import Data.Maybe

data Operation = Add | Copy | Modify | Forget deriving Show

data LSTLine a = LSTLine { operation :: Operation
                         , tags :: [a] }
                 deriving Show

class LSTObject a where
  parseLine :: String -> PParser [a]

  parseLSTLine :: PParser (LSTLine a)
  parseLSTLine = do
    (name, operation) <- parseStart <* tabs
    tags <- parseLine name <* ending
    return LSTLine { .. } where
      ending = eol <|> (eof >> return '\0')

parseStart :: PParser (String, Operation)
parseStart = do
  what <- parseString
  return . fromJust $ matchSuffixes what where
    matchSuffixes str = (\x -> (x, Modify)) <$> stripSuffix ".MOD" str
                    <|> (\x -> (x, Forget)) <$> stripSuffix ".FORGET" str
                    <|> (\x -> (x, Copy)) <$> stripSuffix ".COPY" str
                    <|> return (str, Add)
