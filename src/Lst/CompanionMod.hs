{-# LANGUAGE OverloadedStrings #-}

module Lst.CompanionMod where

import Text.Parsec.Char (char, noneOf, string)
import Text.Parsec.Combinator (sepBy, many1, option, notFollowedBy)
import Text.Parsec.Prim (many, try)
import ClassyPrelude hiding (try)

import Restrictions (RestrictionTag, parseAdditionalRestrictions)
import Modifications
import JEPFormula hiding (Add)
import Common

data CompanionModDefinition = Follower CompanionType
                            | MasterBonusRace String
                              deriving Show

data CompanionType = CompanionType { companionClasses :: [String]
                                   , companionRequired :: Int }
                   deriving Show

parseFollower :: PParser CompanionType
parseFollower = do
  _ <- tag "FOLLOWER"
  companionClasses <- parseStringNoCommas `sepBy` char ','
  _ <- char '='
  companionRequired <- parseInteger
  return CompanionType { .. }

parseMasterBonusRace :: PParser String
parseMasterBonusRace = tag "MASTERBONUSRACE" *> restOfTag

parseCompanionModTag :: PParser CompanionModDefinition
parseCompanionModTag = undefined

parseCompanionModBeginning :: PParser (LSTStart CompanionModDefinition, Operation)
parseCompanionModBeginning = do
  what <- Follower <$> parseFollower
      <|> MasterBonusRace <$> parseMasterBonusRace
  return (Block what, Add)

instance LSTObject CompanionModDefinition where
  parseBeginning = parseCompanionModBeginning
  parseSpecificTags = parseCompanionModTag
