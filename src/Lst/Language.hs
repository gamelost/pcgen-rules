{-# LANGUAGE OverloadedStrings #-}

module Lst.Language where

import qualified Data.Text as T
import Control.Monad(liftM)
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

type LanguageMod = Modification LanguageDefinition

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

parseLanguageDefinition :: Parser (T.Text, Operation) -> Parser LanguageMod
parseLanguageDefinition p = do
  (languageName, op) <- p <* tabs
  languageKey <- optionMaybe parseKey <* tabs
  pid <- option False parseProductIdentity <* tabs
  keystat <- optionMaybe parseKeyStat <* tabs
  untrained <- option False parseUseUntrained <* tabs
  types <- parseType <* tabs -- required
  languageRestriction <- option Nothing parseRestriction <* tabs
  page <- optionMaybe parseSourcePage <* tabs
  let languages = map convertLanguageType types
  return Modification { operation = op
                      , record = LanguageDefinition { name = languageName
                                                    , key = languageKey
                                                    , productIdentity = pid
                                                    , useUntrained = untrained
                                                    , languageType = languages
                                                    , keyStat = keystat
                                                    , sourcePage = page
                                                    , restriction = languageRestriction } } where
    convertLanguageType :: T.Text -> LanguageType
    convertLanguageType "Read" = Read
    convertLanguageType "Spoken" = Spoken
    convertLanguageType "Written" = Written
    convertLanguageType l = Other l

parseLanguageLine :: Parser LanguageMod
parseLanguageLine = parseLanguageDefinition parseForget <|>
                    parseLanguageDefinition parseMod <|>
                    parseLanguageDefinition parseAdd
