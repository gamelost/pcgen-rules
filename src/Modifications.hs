{-# LANGUAGE RecordWildCards #-}

module Modifications where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Common

data Operation = Add | Copy | Modify | Forget deriving Show

data LSTLine a = LSTLine { operation :: Operation
                         , tags :: [a] } deriving Show

class LSTObject a where
  parseLine :: T.Text -> Parser [a]

  parseLSTLine :: Parser (LSTLine a)
  parseLSTLine = do
    (name, operation) <- parseModify <|>
                         parseForget <|>
                         parseCopy <|>
                         parseAdd <* tabs
    tags <- parseLine name
    return LSTLine { .. }

parseStartString :: Parser T.Text
parseStartString = takeWhile1 $ inClass "-A-Za-z0-9 /'().:+&" -- no punctuation

parseSuffix :: String -> Parser T.Text
parseSuffix suffix = do
  what <- parseStartString
  _ <- string $ T.pack suffix
  return what

parseModify :: Parser (T.Text, Operation)
parseModify = parseSuffix ".MOD" >>= (\name -> return (name, Modify))

parseForget :: Parser (T.Text, Operation)
parseForget = parseSuffix ".FORGET" >>= (\name -> return (name, Forget))

parseCopy :: Parser (T.Text, Operation)
parseCopy = parseSuffix ".COPY" >>= (\name -> return (name, Copy))

parseAdd :: Parser (T.Text, Operation)
parseAdd = parseStartString >>= (\name -> return (name, Add))
