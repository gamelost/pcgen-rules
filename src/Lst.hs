{-# LANGUAGE OverloadedStrings #-}

module Lst where

import Prelude hiding (takeWhile)
import Control.Monad(liftM)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Common

-- custom lst types
import Lst.ArmorProf(ArmorProf, parseArmorProfLine)
import Lst.Language(LanguageDefinition, parseLanguageLine)
import Lst.Generic(LSTTag, parseGenericLine)

-- structure of a lst file
data LST a = Source [Header]
           | Definition a
           | Comment T.Text deriving Show

-- source headers: these are found in nearly every lst file type.
data Header = SourceLong T.Text
            | SourceShort T.Text
            | SourceWeb T.Text
            | SourceDate T.Text deriving Show

parseSource :: T.Text -> Parser T.Text
parseSource source = do
  _ <- string source
  parseString

parseSourceLong :: Parser Header
parseSourceLong = liftM SourceLong $ parseSource "SOURCELONG:"

parseSourceShort :: Parser Header
parseSourceShort = liftM SourceShort $ parseSource "SOURCESHORT:"

parseSourceWeb :: Parser Header
parseSourceWeb = liftM SourceWeb $ parseSource "SOURCEWEB:"

parseSourceDate :: Parser Header
parseSourceDate = liftM SourceDate $ parseSource "SOURCEDATE:"

parseHeaders :: Parser [Header]
parseHeaders = many1 (parseSourceLong <|>
                      parseSourceShort <|>
                      parseSourceWeb <|>
                      parseSourceDate) <* tabs

parseLSTLine :: Parser a -> Parser [LST a]
parseLSTLine parseDefinition = do
  -- next line added solely because of saspg_languages.lst, ugh
  _ <- many endOfLine
  many1 $ (liftM Source parseHeaders <|>
           liftM Comment parseCommentLine <|>
           liftM Definition parseDefinition) <* many endOfLine

parseLST :: FilePath -> Parser [LST a] -> IO [LST a]
parseLST lstName lstParser = do
  contents <- readContents lstName
  return $ parseResult lstName $ parse lstParser contents

parseLanguageLST :: FilePath -> IO [LST LanguageDefinition]
parseLanguageLST lstName = parseLST lstName $ parseLSTLine parseLanguageLine

parseGenericLST :: FilePath -> IO [LST LSTTag]
parseGenericLST lstName = parseLST lstName $ parseLSTLine parseGenericLine

parseArmorProfLST :: FilePath -> IO [LST ArmorProf]
parseArmorProfLST lstName = parseLST lstName $ parseLSTLine parseArmorProfLine
