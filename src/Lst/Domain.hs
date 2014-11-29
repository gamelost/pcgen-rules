{-# LANGUAGE OverloadedStrings #-}

module Lst.Domain where

import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepBy)
import ClassyPrelude

import Modifications
import Common

type DomainSpell = (String, Int, String)

data DomainDefinition = DomainDescription String
                      | DomainSpellLevel [DomainSpell]
                      | DomainType String
                        deriving Show

parseDescription :: PParser String
parseDescription = tag "DESC" *> restOfTag

parseType :: PParser String
parseType = tag "TYPE" *> restOfTag

parseSpellLevel :: PParser [DomainSpell]
parseSpellLevel = do
  _ <- labeled "SPELLLEVEL:DOMAIN|"
  parseSpell `sepBy` char '|' where
    parseSpell :: PParser DomainSpell
    parseSpell = do
      word <- parseTill '='
      level <- manyNumbers <* char '|'
      description <- parseString
      return (word, textToInt level, description)

parseDomainTag :: PParser DomainDefinition
parseDomainTag = DomainDescription <$> parseDescription
             <|> DomainType <$> parseType
             <|> DomainSpellLevel <$> parseSpellLevel

instance LSTObject DomainDefinition where
  parseSpecificTags = parseDomainTag
