{-# LANGUAGE OverloadedStrings #-}

module Lst.Language where

import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepBy)
import ClassyPrelude

import Modifications
import Common

-- we only define the most common language types here
data LanguageType = Read
                  | Spoken
                  | Written
                  | Other String
                    deriving Show

data LanguageDefinition = Key String
                        | LanguageTypes [LanguageType]
                          deriving Show

parseKey :: PParser LanguageDefinition
parseKey = Key <$> (tag "KEY" >> restOfTag)

parseTypes :: PParser LanguageDefinition
parseTypes = do
  types <- tag "TYPE" >> parseWordAndNumber `sepBy` char '.'
  return . LanguageTypes $ map convertLanguageType types where
    convertLanguageType :: String -> LanguageType
    convertLanguageType "Read" = Read
    convertLanguageType "Spoken" = Spoken
    convertLanguageType "Written" = Written
    convertLanguageType l = Other l

parseLanguageTag :: PParser LanguageDefinition
parseLanguageTag = parseKey
               <|> parseTypes

instance LSTObject LanguageDefinition where
  parseSpecificTags = parseLanguageTag
