{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Modifications where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Control.Applicative
import Common
import Data.Maybe
import Data.List(stripPrefix)

data Operation = Add | Copy | Modify | Forget deriving Show

data LSTLine a = LSTLine { operation :: Operation
                         , tags :: [a] }
                 deriving Show

class LSTObject a where
  parseLine :: String -> Parser [a]

  parseLSTLine :: Parser (LSTLine a)
  parseLSTLine = do
    (name, operation) <- parseStart <* tabs
    tags <- parseLine name
    _ <- many1 $ satisfy $ inClass "\r\n"
    return LSTLine { .. }

stripSuffix :: String -> String -> Maybe String
stripSuffix sfx rest = case stripPrefix (reverse sfx) (reverse rest) of
  Just ys -> Just (reverse ys)
  Nothing -> Nothing

parseStart :: Parser (String, Operation)
parseStart = do
  what <- parseString
  return . fromJust $ matchSuffixes what where
    matchSuffixes str = (\x -> (x, Modify)) <$> stripSuffix ".MOD" str
                    <|> (\x -> (x, Forget)) <$> stripSuffix ".FORGET" str
                    <|> (\x -> (x, Copy)) <$> stripSuffix ".COPY" str
                    <|> return (str, Add)
