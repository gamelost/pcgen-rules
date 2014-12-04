{-# LANGUAGE OverloadedStrings #-}

module Lst.Feat where

import Text.Parsec.Char (char, satisfy)
import Text.Parsec.Combinator (sepBy, sepBy1, many1)
import ClassyPrelude

import Restrictions (RestrictionTag, parseRestriction)
import Modifications
import JEPFormula hiding (Add)
import Common

data FeatDefinition = Description String
                    | Visible Visibility
                    | Type [String]
                       deriving Show

parseDescription :: PParser String
parseDescription = tag "DESC" *> restOfTag

parseType :: PParser [String]
parseType = tag "TYPE" *> parseWordAndNumbers `sepBy1` char '.' where
  parseWordAndNumbers = many1 $ satisfy $ inClass "-A-Za-z0-9, _/"

data Visibility = IsVisible
                | NotVisible
                | Display
                | Export
                  deriving (Show, Eq)

parseVisible :: PParser Visibility
parseVisible = tag "VISIBLE" *> parseVisibility where
  parseVisibility = IsVisible <$ (labeled "YES")
                <|> NotVisible <$ (labeled "NO")
                <|> Display <$ (labeled "DISPLAY")
                <|> Export <$ (labeled "EXPORT")

parseFeatTag :: PParser FeatDefinition
parseFeatTag = Description <$> parseDescription
           <|> Type <$> parseType
           <|> Visible <$> parseVisible

instance LSTObject FeatDefinition where
  parseSpecificTags = parseFeatTag
