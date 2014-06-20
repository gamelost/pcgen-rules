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
                                             , productIdentity :: Bool -- optional
                                             , languageType :: [LanguageType]
                                             , sourcePage :: T.Text -- optional
                                             , restriction :: Maybe Restriction
                                             , modification :: Maybe Modification
                                             } deriving Show

parseProductIdentity :: Parser Bool
parseProductIdentity = do
  _ <- string "NAMEISPI:"
  answer <- allCaps
  return $ answer == "YES"

parseSourcePage :: Parser T.Text
parseSourcePage = do
  _ <- string "SOURCEPAGE:"
  parseString

convertLanguageType :: T.Text -> LanguageType
convertLanguageType "Read" = Read
convertLanguageType "Spoken" = Spoken
convertLanguageType "Written" = Written
convertLanguageType l = Other l

parseLanguageMod :: Parser (T.Text, Maybe Modification)
parseLanguageMod = liftM (\x -> (x, Just Add)) parseModification

parseLanguage :: Parser (T.Text, Maybe Modification)
parseLanguage = liftM (\x -> (x, Nothing)) parseString

parseLanguageDefinition :: Parser (T.Text, Maybe Modification) -> Parser LanguageDefinition
parseLanguageDefinition p = do
  (languageName, modifier) <- p <* tabs
  pid <- option False parseProductIdentity <* tabs
  _ <- string "TYPE:"
  types <- parseWord `sepBy` char '.' <* tabs
  languageRestriction <- option Nothing parseRestriction <* tabs
  page <- option (T.pack "") parseSourcePage <* tabs
  let languages = map convertLanguageType types
  return LanguageDefinition { name = languageName
                            , productIdentity = pid
                            , languageType = languages
                            , sourcePage = page
                            , modification = modifier
                            , restriction = languageRestriction }

parseLanguageLine :: Parser LanguageDefinition
parseLanguageLine = parseLanguageDefinition parseLanguageMod <|>
                    parseLanguageDefinition parseLanguage
