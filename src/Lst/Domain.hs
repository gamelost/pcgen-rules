{-# LANGUAGE OverloadedStrings #-}

module Lst.Domain where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Control.Applicative
import Modifications
import Restrictions
import Lst.GlobalTags
import Common
import Clear(parseClear, ClearTag(..))
import Bonus(parseBonus, Bonus)

type DomainSpell = (String, Int, String)

data DomainDefinition = Name String
                      | DomainDescription String
                      | DomainSpellLevel [DomainSpell]
                      | DomainType String
                      | DomainKey String
                      | DomainVision String
                      -- shared tags
                      | Global GlobalTag
                      | DomainBonus Bonus
                      | DomainClear ClearTag
                      | Restricted Restriction
                        deriving Show

parseDescription :: PParser String
parseDescription = tag "DESC" *> restOfTag

parseType :: PParser String
parseType = tag "TYPE" *> restOfTag

parseKey :: PParser String
parseKey = tag "KEY" *> restOfTag

parseVision :: PParser String
parseVision = tag "VISION" *> parseString

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
parseDomainTag = DomainClear <$> parseClear
             <|> DomainDescription <$> parseDescription
             <|> DomainType <$> parseType
             <|> DomainKey <$> parseKey
             <|> DomainVision <$> parseVision
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
