{-# LANGUAGE OverloadedStrings #-}

module Lst.Domain where

import Text.Parsec.Combinator
import Control.Applicative
import Modifications
import Restrictions
import Lst.GlobalTags
import Common
import Bonus(parseBonus, Bonus)

data DomainDefinition = Name String
                      | Description String
                      -- shared tags
                      | Global GlobalTag
                      | DomainBonus Bonus
                      | Restricted Restriction
                        deriving Show

parseDescription :: PParser String
parseDescription = tag "DESC" *> parseString

parseDomainTag :: PParser DomainDefinition
parseDomainTag = Description <$> parseDescription
             <|> Global <$> parseGlobalTags
             <|> DomainBonus <$> parseBonus
             <|> Restricted <$> parseRestriction

parseDomain :: String -> PParser [DomainDefinition]
parseDomain domainName = do
  domainTags <- tabs *> parseDomainTag `sepBy` tabs
  return $ domainTags ++ [Name domainName]

instance LSTObject DomainDefinition where
  parseLine = parseDomain
