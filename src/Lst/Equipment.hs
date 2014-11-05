{-# LANGUAGE OverloadedStrings #-}

module Lst.Equipment where

import Text.Parsec.Char (char, satisfy, noneOf, string)
import Text.Parsec.Combinator (sepBy1, many1, option, notFollowedBy)
import Text.Parsec.Prim (many)
import ClassyPrelude

import Restrictions (RestrictionTag, parseAdditionalRestrictions)
import Modifications
import JEPFormula
import Common

data EquipmentDefinition = Description String
                         | Cost Formula
                         | Weight Formula
                         | ACCheck Formula
                         | Size EquipmentSize
                         | SpecialProperty Property
                         | CriticalMultiple CriticalMultipleType
                         | Proficiency ProficiencyItem
                         | Range RangeIncrement
                         | EquipmentType [String]
                           deriving Show

parseWeight :: PParser Formula
parseWeight = tag "WT" *> parseFormula

parseCost :: PParser Formula
parseCost = tag "COST" *> parseFormula

parseACCheck :: PParser Formula
parseACCheck = tag "ACCHECK" *> parseFormula

parseEquipmentType :: PParser [String]
parseEquipmentType = tag "TYPE" *> parseWordAndNumbers `sepBy1` char '.' where
  -- arms_equip_armorshield.lst has a bug where it has a ',' instead of a '.'
  -- martialmayhem_equip_weap_melee.lst has a ':'
  -- aeg_gods_equip_magic.lst has "TYPE:<agic.Wondrous"
  -- apg_equip_magic_items.lst has "Artifact/Major"
  parseWordAndNumbers = many1 $ satisfy $ inClass "-A-Za-z0-9,: <_/"

parseDescription :: PParser String
parseDescription = tag "DESC" *> restOfTag

data Property = Property { specialPropertyText :: String
                         , specialPropertyVariables :: [Formula]
                         , specialPropertyRestrictions :: [RestrictionTag] }
              deriving (Show, Eq)

parseSpecialProperty :: PParser Property
parseSpecialProperty = do
  _ <- tag "SPROP"
  specialPropertyText <- many1 $ noneOf "\t\r\n|"
  -- don't bother trying to gauge the number of '%'s. some SPROP text
  -- is unescaped so we just have to blindly read in the variables.
  specialPropertyVariables <- many parsePossibleFormula
  specialPropertyRestrictions <- option [] parseAdditionalRestrictions
  return Property { .. } where
    parsePossibleFormula = notAllowed *> char '|' *> parseFormula
    notAllowed = notFollowedBy (string "|PRE") *> notFollowedBy (string "|!PRE")

data CriticalMultipleType = CriticalModifier Int
                          | NoCriticalModifier
                            deriving (Show, Eq)

parseCritMult :: PParser CriticalMultipleType
parseCritMult = tag "CRITMULT" *> parseCriticalMultipleType where
  parseCriticalMultipleType = CriticalModifier <$> (labeled "x" *> (textToInt <$> manyNumbers))
                          <|> NoCriticalModifier <$ (labeled "-")

data ProficiencyType = ProficiencyWithWeapon
                     | ProficiencyWithArmor
                     | ProficiencyWithShield
                       deriving (Show, Eq)

data ProficiencyItem = ProficiencyItem { proficiencyType :: ProficiencyType
                                       , proficiencyName :: String }
                     deriving (Show, Eq)

parseProficiency :: PParser ProficiencyItem
parseProficiency = do
  _ <- tag "PROFICIENCY"
  proficiencyType <- parseProficiencyWeapon <* char '|'
  proficiencyName <- parseString
  return ProficiencyItem { .. } where
    parseProficiencyWeapon = ProficiencyWithWeapon <$ (labeled "WEAPON")
                         <|> ProficiencyWithArmor <$ (labeled "ARMOR")
                         <|> ProficiencyWithShield <$ (labeled "SHIELD")

data EquipmentSize = Fine
                   | Diminutive
                   | Tiny
                   | Small
                   | Medium
                   | Large
                   | Huge
                   | Gargantuan
                   | Colossal
                   | PC
                   | OtherSize String
                     deriving (Show, Eq)

parseSize :: PParser EquipmentSize
parseSize = tag "SIZE" *> parseEquipmentSize where
  parseEquipmentSize = (Fine <$ labeled "F")
                   <|> (Diminutive <$ labeled "D")
                   <|> (Tiny <$ labeled "T")
                   <|> (Small <$ labeled "S")
                   <|> (Medium <$ labeled "M")
                   <|> (Large <$ labeled "L")
                   <|> (Huge <$ labeled "H")
                   <|> (Gargantuan <$ labeled "G")
                   <|> (Colossal <$ labeled "C")
                   <|> (PC <$ labeled "PC")
                   <|> (OtherSize <$> parseString)

-- the range string is way too inconsistent to parse consistently, but
-- we try to parse it as a formula here, anyway.
data RangeIncrement = RangeIncrement { incrementFormula :: Maybe Formula
                                     , incrementString :: Maybe String }
                    deriving (Show, Eq)

parseRange :: PParser RangeIncrement
parseRange = do
  _ <- tag "RANGE"
  incrementFormula <- tryOption parseFormula
  incrementString <- tryOption parseString
  let _ = assert (isJust incrementFormula || isJust incrementString)
  return RangeIncrement { .. }

parseEquipmentTag :: PParser EquipmentDefinition
parseEquipmentTag = Description <$> parseDescription
                <|> Weight <$> parseWeight
                <|> Cost <$> parseCost
                <|> ACCheck <$> parseACCheck
                <|> Size <$> parseSize
                <|> SpecialProperty <$> parseSpecialProperty
                <|> CriticalMultiple <$> parseCritMult
                <|> Proficiency <$> parseProficiency
                <|> Range <$> parseRange
                <|> EquipmentType <$> parseEquipmentType

instance LSTObject EquipmentDefinition where
  parseSpecificTags = parseEquipmentTag
