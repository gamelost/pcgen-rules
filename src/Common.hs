{-# LANGUAGE OverloadedStrings #-}

module Common where

import Prelude
import qualified Data.ByteString as B
import Data.ByteString.UTF8(toString)
import Control.Monad(liftM)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Error
import Text.Parsec.Prim
import Data.List(stripPrefix)

-- conversion from attoparsec -> parsec
inClass :: String -> Char -> Bool
inClass "" = const False
inClass (a:'-':b:xs) = \c -> (c >= a && c <= b) || f c where f = inClass xs
inClass (x:xs) = \c -> c == x || f c where f = inClass xs

parseWord :: Parser String
parseWord = many1 $ satisfy $ inClass "-A-Za-z"

parseString :: Parser String
parseString = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+,./:?!%#'()[]~" -- do not put in '=' or '|'

allCaps :: Parser String
allCaps = many1 upper

manyNumbers :: Parser String
manyNumbers = many1 digit

tabs :: Parser ()
tabs = skipMany1 tab

tag :: String -> Parser String
tag t = string $ t ++ ":"

textToInt :: String -> Int
textToInt t = read t :: Int

eol :: Parser Char
eol = satisfy (inClass "\n\r") <?> "eol"

restOfLine :: Parser String
restOfLine = manyTill anyChar eol

restOfTag :: Parser String
restOfTag = manyTill anyChar $ satisfy $ inClass "\n\r\t"

parseTabs :: Parser [String]
parseTabs = restOfTag `sepBy` tabs

parseCommentLine :: Parser String
parseCommentLine = char '#' >> spaces >> restOfLine where

parseWordAndNumber :: Parser String
parseWordAndNumber = many1 $ satisfy $ inClass "-A-Za-z0-9"

yesOrNo :: Parser Bool
yesOrNo = liftM (== "YES") allCaps

stripSuffix :: String -> String -> Maybe String
stripSuffix sfx rest = case stripPrefix (reverse sfx) (reverse rest) of
  Just ys -> Just (reverse ys)
  Nothing -> Nothing

-- do not use parseOnly: it does not fail if there is any leftover
-- input. If our parser does not consume everything, we want instant
-- failure.
parseResult :: Show t => FilePath -> Either ParseError t -> t
parseResult filename result =
  case result of
    Left err -> error $ show err
    Right success -> success

-- Data.Text.IO.readFile does not do the right thing, sigh. Instead,
-- read in the contents as a bytestring and then attempt an utf8
-- decoding. We need to do this in order to parse certain LST files.
readContents :: FilePath -> IO String
readContents filename = do
  f <- B.readFile filename
  return $ toString f
