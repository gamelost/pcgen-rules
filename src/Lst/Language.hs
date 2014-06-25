{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lst.Language where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Restrictions
import Modifications
import Common

-- we only define the most common language types here
data LanguageType = Read
                  | Spoken
                  | Written
                  | Other T.Text
                    deriving Show

data LanguageDefinition = LanguageDefinition { name :: T.Text
                                             , key :: Maybe T.Text
                                             , productIdentity :: Bool
                                             , useUntrained :: Bool
                                             , languageType :: [LanguageType]
                                             , keyStat :: Maybe T.Text
                                             , sourcePage :: Maybe T.Text
                                             , restriction :: Maybe Restriction
                                             } deriving Show

parseProductIdentity :: Parser Bool
parseProductIdentity = tag "NAMEISPI" >> yesOrNo

parseUseUntrained :: Parser Bool
parseUseUntrained = tag "USEUNTRAINED" >> yesOrNo

parseKeyStat :: Parser T.Text
parseKeyStat = tag "KEYSTAT" >> parseString

parseSourcePage :: Parser T.Text
parseSourcePage = tag "SOURCEPAGE" >> parseString

parseKey :: Parser T.Text
parseKey = tag "KEY" >> parseString

parseType :: Parser [T.Text]
parseType = tag "TYPE" >> parseWordAndNumber `sepBy` char '.'

parseLanguageDefinition :: T.Text -> Parser LanguageDefinition
parseLanguageDefinition name = do
  key <- optionMaybe parseKey <* tabs
  productIdentity <- option False parseProductIdentity <* tabs
  keyStat <- optionMaybe parseKeyStat <* tabs
  useUntrained <- option False parseUseUntrained <* tabs
  types <- parseType <* tabs -- required
  restriction <- option Nothing parseRestriction <* tabs
  sourcePage <- optionMaybe parseSourcePage <* tabs
  let languageType = map convertLanguageType types
  return LanguageDefinition {..} where
    convertLanguageType :: T.Text -> LanguageType
    convertLanguageType "Read" = Read
    convertLanguageType "Spoken" = Spoken
    convertLanguageType "Written" = Written
    convertLanguageType l = Other l

instance LSTObject LanguageDefinition where
  parseLine = parseLanguageDefinition
