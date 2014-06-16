{-# LANGUAGE OverloadedStrings #-}

module Lst.Language where

import qualified Data.Text as T
import Control.Monad(liftM)
import Data.Attoparsec.Text
import Control.Applicative
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

parseLanguage :: Parser LanguageLine
parseLanguage = do
  name <- parseString <* tabs
  pi <- option False parseProductIdentity <* tabs
  _ <- string "TYPE:"
  types <- parseWord `sepBy` char '.' <* tabs
  page <- option (T.pack "") parseSourcePage <* tabs
  let languages = map convertLanguageType types
  return $ Definition LanguageDefinition { name = name,
                                           productIdentity = pi,
                                           languageType = languages,
                                           sourcePage = page }

parseComment :: Parser LanguageLine
parseComment = liftM Comment commentedLine

parseHeader :: Parser LanguageLine
parseHeader = liftM Source parseHeaders

parseLanguageLine :: Parser Languages
parseLanguageLine = many1 $ (parseComment <|>
                             parseHeader <|>
                             parseLanguage) <* many endOfLine
