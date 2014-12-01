{-# LANGUAGE OverloadedStrings #-}

module Lst.CompanionMod where

import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepBy)
import ClassyPrelude

import Modifications
import JEPFormula hiding (Add)
import Common

data CompanionModDefinition = Follower Companion
                            | MasterBonusRace String
                            -- rest
                            | CompanionType String
                            | CompanionRaceType String
                            | CompanionHD [Roll]
                            | CompanionMasterCheck Bool
                            | CompanionMasterBAB Bool
                            | CompanionMasterSkill Bool
                            | CompanionMasterHP Formula
                            | CompanionTemplate String
                              deriving Show

data Companion = Companion { companionClasses :: [String]
                           , companionRequired :: Int }
                 deriving Show

parseFollower :: PParser Companion
parseFollower = do
  _ <- tag "FOLLOWER"
  companionClasses <- parseStringNoCommas `sepBy` char ','
  _ <- char '='
  companionRequired <- parseInteger
  return Companion { .. }

parseMasterBonusRace :: PParser String
parseMasterBonusRace = tag "MASTERBONUSRACE" *> restOfTag

parseCompanionType :: PParser String
parseCompanionType = tag "TYPE" *> parseString

parseCompanionRaceType :: PParser String
parseCompanionRaceType = tag "RACETYPE" *> parseString

parseCompanionTemplate :: PParser String
parseCompanionTemplate = tag "TEMPLATE" *> parseString

parseCompanionHD :: PParser [Roll]
parseCompanionHD = tag "HD" *> parseRolls

parseCompanionMasterCheck :: PParser Bool
parseCompanionMasterCheck = True <$ labeled "COPYMASTERCHECK:MASTER"

parseCompanionMasterBAB :: PParser Bool
parseCompanionMasterBAB = True <$ labeled "COPYMASTERBAB:MASTER"

parseUseMasterSkill :: PParser Bool
parseUseMasterSkill = True <$ labeled "USEMASTERSKILL:YES"

parseCompanionMasterHP :: PParser Formula
parseCompanionMasterHP = tag "COPYMASTERHP" *> parseFormula

parseCompanionModTag :: PParser CompanionModDefinition
parseCompanionModTag = CompanionType <$> parseCompanionType
                   <|> CompanionRaceType <$> parseCompanionRaceType
                   <|> CompanionHD <$> parseCompanionHD
                   <|> CompanionTemplate <$> parseCompanionTemplate
                   <|> CompanionMasterCheck <$> parseCompanionMasterCheck
                   <|> CompanionMasterBAB <$> parseCompanionMasterBAB
                   <|> CompanionMasterHP <$> parseCompanionMasterHP
                   <|> CompanionMasterSkill <$> parseUseMasterSkill

parseCompanionModBeginning :: PParser (LSTStart CompanionModDefinition, Operation)
parseCompanionModBeginning = do
  what <- Follower <$> parseFollower
      <|> MasterBonusRace <$> parseMasterBonusRace
  return (Block what, Add)

instance LSTObject CompanionModDefinition where
  parseBeginning = parseCompanionModBeginning
  parseSpecificTags = parseCompanionModTag
