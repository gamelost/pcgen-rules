{-# LANGUAGE OverloadedStrings #-}

module Lst.Domain where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Control.Applicative
import Modifications
import Restrictions
import Lst.GlobalTags
import Common
import Bonus(parseBonus, Bonus)

type DomainSpell = (String, Int, String)

data DomainDefinition = Name String
                      | Description String
                      | DomainSpellLevel [DomainSpell]
                      -- shared tags
                      | Global GlobalTag
                      | DomainBonus Bonus
                      | Restricted Restriction
                        deriving Show

parseDescription :: PParser String
parseDescription = tag "DESC" *> parseString

parseSpellLevel :: PParser [DomainSpell]
parseSpellLevel = do
  _ <- labeled "SPELLLEVEL:DOMAIN|"
  parseSpell `sepBy` char '|' where
    parseSpell :: PParser DomainSpell
    parseSpell = do
      word <- parseString <* char '='
      level <- manyNumbers <* char '|'
      description <- parseString
      return (word, textToInt level, description)

parseDomainTag :: PParser DomainDefinition
parseDomainTag = Description <$> parseDescription
             <|> DomainSpellLevel <$> parseSpellLevel
             <|> DomainBonus <$> parseBonus
             <|> Restricted <$> parseRestriction
             <|> Global <$> parseGlobalTags

parseDomain :: String -> PParser [DomainDefinition]
parseDomain domainName = do
  domainTags <- tabs *> parseDomainTag `sepBy` tabs
  return $ Name domainName : domainTags

instance LSTObject DomainDefinition where
  parseLine = parseDomain
