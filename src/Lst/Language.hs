{-# LANGUAGE OverloadedStrings #-}

module Lst.Language where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Restrictions
import Modifications
import Common
import Bonus

-- we only define the most common language types here
data LanguageType = Read
                  | Spoken
                  | Written
                  | Other T.Text
                    deriving Show

data LanguageDefinition = Name T.Text
                        | Key T.Text
                        | ProductIdentity Bool
                        | UseUntrained Bool
                        | LanguageTypes [LanguageType]
                        | KeyStat T.Text
                        | SourcePage T.Text
                        | LanguageBonus Bonus
                        | Restricted Restriction
                          deriving Show

parseProductIdentity :: Parser LanguageDefinition
parseProductIdentity = ProductIdentity <$> (tag "NAMEISPI" >> yesOrNo)

parseUseUntrained :: Parser LanguageDefinition
parseUseUntrained = UseUntrained <$> (tag "USEUNTRAINED" >> yesOrNo)

parseKeyStat :: Parser LanguageDefinition
parseKeyStat = KeyStat <$> (tag "KEYSTAT" >> parseString)

parseSourcePage :: Parser LanguageDefinition
parseSourcePage = SourcePage <$> (tag "SOURCEPAGE" >> parseString)

parseKey :: Parser LanguageDefinition
parseKey = Key <$> (tag "KEY" >> parseString)

parseTypes :: Parser LanguageDefinition
parseTypes = do
  types <- tag "TYPE" >> parseWordAndNumber `sepBy` char '.'
  return . LanguageTypes $ map convertLanguageType types where
    convertLanguageType :: T.Text -> LanguageType
    convertLanguageType "Read" = Read
    convertLanguageType "Spoken" = Spoken
    convertLanguageType "Written" = Written
    convertLanguageType l = Other l

parseLanguageTag :: Parser LanguageDefinition
parseLanguageTag = parseKey
               <|> parseProductIdentity
               <|> parseKeyStat
               <|> parseUseUntrained
               <|> parseTypes
               <|> parseSourcePage
               <|> LanguageBonus <$> parseBonus
               <|> Restricted <$> parseRestriction

parseLanguageDefinition :: T.Text -> Parser [LanguageDefinition]
parseLanguageDefinition name = do
  languageTags <- tabs *> parseLanguageTag `sepBy` tabs
  return $ languageTags ++ [Name name]

instance LSTObject LanguageDefinition where
  parseLine = parseLanguageDefinition
