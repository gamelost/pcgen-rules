{-# LANGUAGE OverloadedStrings #-}

module Lst.Language where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Control.Applicative
import Restrictions
import Modifications
import Lst.GlobalTags
import Common
import Bonus(parseBonus, Bonus)

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

parseKey :: Parser LanguageDefinition
parseKey = Key <$> (tag "KEY" >> parseString)

parseTypes :: Parser LanguageDefinition
parseTypes = do
  types <- tag "TYPE" >> parseWordAndNumber `sepBy` char '.'
  return . LanguageTypes $ map convertLanguageType types where
    convertLanguageType :: String -> LanguageType
    convertLanguageType "Read" = Read
    convertLanguageType "Spoken" = Spoken
    convertLanguageType "Written" = Written
    convertLanguageType l = Other l

parseLanguageTag :: Parser LanguageDefinition
parseLanguageTag = parseKey
               <|> parseTypes
               <|> Global <$> parseGlobalTags
               <|> LanguageBonus <$> parseBonus
               <|> Restricted <$> parseRestriction

parseLanguageDefinition :: String -> Parser [LanguageDefinition]
parseLanguageDefinition name = do
  languageTags <- parseLanguageTag `sepBy` tabs
  return $ languageTags ++ [Name name]

instance LSTObject LanguageDefinition where
  parseLine = parseLanguageDefinition
