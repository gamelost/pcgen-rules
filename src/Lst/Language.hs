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
                                             , modification :: Maybe Modification
                                             } deriving Show

parseProductIdentity :: Parser Bool
parseProductIdentity = string "NAMEISPI:" >> yesOrNo

parseUseUntrained :: Parser Bool
parseUseUntrained = string "USEUNTRAINED:" >> yesOrNo

parseKeyStat :: Parser T.Text
parseKeyStat = string "KEYSTAT:" >> parseString

parseSourcePage :: Parser T.Text
parseSourcePage = string "SOURCEPAGE:" >> parseString

parseKey :: Parser T.Text
parseKey = string "KEY:" >> parseString

parseType :: Parser [T.Text]
parseType = string "TYPE:" >> parseWordAndNumber `sepBy` char '.'

parseLanguageDefinition :: Parser (T.Text, Maybe Modification) -> Parser LanguageDefinition
parseLanguageDefinition p = do
  (languageName, modifier) <- p <* tabs
  languageKey <- optionMaybe parseKey <* tabs
  pid <- option False parseProductIdentity <* tabs
  keystat <- optionMaybe parseKeyStat <* tabs
  untrained <- option False parseUseUntrained <* tabs
  types <- parseType <* tabs
  languageRestriction <- option Nothing parseRestriction <* tabs
  page <- optionMaybe parseSourcePage <* tabs
  let languages = map convertLanguageType types
  return LanguageDefinition { name = languageName
                            , key = languageKey
                            , productIdentity = pid
                            , useUntrained = untrained
                            , languageType = languages
                            , keyStat = keystat
                            , sourcePage = page
                            , modification = modifier
                            , restriction = languageRestriction } where
    convertLanguageType :: T.Text -> LanguageType
    convertLanguageType "Read" = Read
    convertLanguageType "Spoken" = Spoken
    convertLanguageType "Written" = Written
    convertLanguageType l = Other l

parseLanguageLine :: Parser LanguageDefinition
parseLanguageLine = parseLanguageDefinition parseStartMod <|>
                    -- parseLanguageDefinition parseStartForget <|>
                    parseLanguageDefinition parseStart
