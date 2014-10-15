{-# LANGUAGE OverloadedStrings #-}

module Lst.Language where

import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepBy)
import ClassyPrelude hiding (try)

import Restrictions
import Modifications
import Lst.GlobalTags
import Bonus(parseBonus, Bonus)
import Common

-- we only define the most common language types here
data LanguageType = Read
                  | Spoken
                  | Written
                  | Other String
                    deriving Show

data LanguageDefinition = Name String
                        | Key String
                        | LanguageTypes [LanguageType]
                        | LanguageBonus Bonus
                        -- shared tags
                        | Global GlobalTag
                        | Restricted Restriction
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
               <|> LanguageBonus <$> parseBonus
               <|> Restricted <$> parseRestriction
               <|> Global <$> parseGlobalTags

parseLanguageDefinition :: String -> PParser [LanguageDefinition]
parseLanguageDefinition name = do
  languageTags <- parseLanguageTag `sepBy` tabs
  return $ languageTags ++ [Name name]

instance LSTObject LanguageDefinition where
  parseLine = parseLanguageDefinition
