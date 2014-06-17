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

data LanguageLine = Source Headers
                  | Definition LanguageDefinition
                  | Comment T.Text deriving Show

type Languages = [LanguageLine]

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
  (name, mod) <- p <* tabs
  pi <- option False parseProductIdentity <* tabs
  _ <- string "TYPE:"
  types <- parseWord `sepBy` char '.' <* tabs
  restriction <- option Nothing parseRestriction <* tabs
  page <- option (T.pack "") parseSourcePage <* tabs
  let languages = map convertLanguageType types
  return LanguageDefinition { name = name
                            , productIdentity = pi
                            , languageType = languages
                            , sourcePage = page
                            , modification = mod
                            , restriction = restriction }

parseLanguageLine :: Parser Languages
parseLanguageLine = do
  -- next line added solely because of saspg_languages.lst, ugh
  _ <- many endOfLine
  many1 $ (liftM Comment parseCommentLine <|>
           liftM Source parseHeaders <|>
           liftM Definition (parseLanguageDefinition parseLanguageMod) <|>
           liftM Definition (parseLanguageDefinition parseLanguage)) <* many endOfLine