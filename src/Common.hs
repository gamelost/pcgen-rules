{-# LANGUAGE OverloadedStrings #-}

module Common where

import Prelude
import qualified Data.ByteString as B
import Data.ByteString.UTF8(toString)
import Control.Monad(liftM)
import Control.Applicative((<*), (*>))
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Prim
import Data.List(stripPrefix)

-- conversion from attoparsec -> parsec
inClass :: String -> Char -> Bool
inClass "" = const False
inClass (a:'-':b:xs) = \c -> (c >= a && c <= b) || f c where f = inClass xs
inClass (x:xs) = \c -> c == x || f c where f = inClass xs

tryStrings :: [String] -> [Parser String]
tryStrings = map $ try . string

tryOption :: Parser a -> Parser (Maybe a)
tryOption = optionMaybe . try

parseWord :: Parser String
parseWord = many1 $ satisfy $ inClass "-A-Za-z"

parseString :: Parser String
parseString = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+,./:?!%#'()[]~" -- do not put in '=' or '|'

allCaps :: Parser String
allCaps = many1 upper

manyNumbers :: Parser String
manyNumbers = many1 digit

tabs :: Parser ()
tabs = skipMany tab

tabs1 :: Parser ()
tabs1 = skipMany1 tab

labeled :: String -> Parser String
labeled = try . string

tag :: String -> Parser String
tag t = labeled $ t ++ ":"

textToInt :: String -> Int
textToInt t = read t :: Int

-- accomodate crlf line terminators.
crlf :: Parser Char
crlf = char '\r' *> char '\n' <?> "crlf new-line"

eol :: Parser Char
eol = newline <|> crlf <?> "eol"

restOfLine :: Parser String
restOfLine = manyTill anyChar eol

restOfTag :: Parser String
restOfTag = manyTill anyChar $ satisfy $ inClass "\n\r\t"

parseTabs :: Parser [String]
parseTabs = restOfTag `sepBy` tabs

parseCommentLine :: Parser String
parseCommentLine = char '#' >> option "" (try $ spaces >> restOfLine)

parseWordAndNumber :: Parser String
parseWordAndNumber = many1 $ satisfy $ inClass "-A-Za-z0-9"

yesOrNo :: Parser Bool
yesOrNo = liftM (== "YES") allCaps

stripSuffix :: String -> String -> Maybe String
stripSuffix sfx rest = case stripPrefix (reverse sfx) (reverse rest) of
  Just ys -> Just (reverse ys)
  Nothing -> Nothing

parseResult :: Show a => Parser a -> FilePath -> String -> a
parseResult parser filename contents =
  let doall = parser <* eof in
  case parse doall filename contents of
    Left err -> error $ show err
    Right success -> success

readContents :: FilePath -> IO String
readContents filename = B.readFile filename >>= spit where
  spit x = return $ toString x
