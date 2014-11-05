{-# LANGUAGE OverloadedStrings #-}

module Lst.Equipment where

import Text.Parsec.Char (char, satisfy, noneOf, string)
import Text.Parsec.Combinator (sepBy, sepBy1, many1, option, optional, notFollowedBy)
import Text.Parsec.Prim (many, try)
import ClassyPrelude hiding (try)

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
                         | Contains Container
                         | RateOfFire String
                         | BaseItem String
                         | Wield WieldType
                         | EquipmentModifier EquipmentMod
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

-- TODO: not fully implemented, need to correlate keys with variable
-- values or chooser responses
data EquipmentMod = EquipmentMod { equipmentModKeys :: [String]
                                 , equipmentModRest :: Maybe String }
                    deriving (Show, Eq)

parseEquipmentModifier :: PParser EquipmentMod
parseEquipmentModifier = do
  _ <- tag "EQMOD"
  equipmentModKeys <- parseStringNoPeriods `sepBy` char '.'
  _ <- optional $ char '|'
  equipmentModRest <- tryOption restOfTag
  return EquipmentMod { .. } where
    parseStringNoPeriods = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+/:?!%#'()[]~"

data ContainerLimits = ContainerLimits { containerWeight :: Maybe Formula
                                       , containerWeightChanges :: Bool
                                       , containerUnlimited :: Bool
                                       , containerReduction :: Maybe Formula }
                     deriving (Show, Eq)

data ContainerType = ItemLimit (String, Int)
                   | ItemType String
                     deriving (Show, Eq)

data Container = Container { containerLimits :: ContainerLimits
                           , containerTypes :: [ContainerType] }
               deriving (Show, Eq)

parseContains :: PParser Container
parseContains = do
  _ <- tag "CONTAINS"
  -- first parameter
  containerWeightChanges <- option False (True <$ char '*')
  containerUnlimited <- option False (True <$ labeled "UNLIM")
  containerWeight <- tryOption parseFormula
  _ <- optional $ char '%'
  containerReduction <- tryOption parseFormula
  let containerLimits = ContainerLimits { .. }
  -- subsequent parameters (if any)
  containerTypes <- option [] (many $ char '|' *> parseContainerTypes)
  return Container { .. } where
    parseContainerTypes = try (ItemLimit <$> parseContainerTypeLimit)
                          <|> (ItemType <$> parseString)
    parseContainerTypeLimit = do
      item <- parseString
      -- note that data/xcrawl/pandahead/xcrawl/xcrawl_equip.lst has "Any:100", ugh.
      -- we do NOT account for that here.
      _ <- char '='
      number <- textToInt <$> manyNumbers
      return (item, number)

data WieldType = Light
               | OneHanded
               | TwoHanded
               | Unusable
                 deriving (Show, Eq)

parseWield :: PParser WieldType
parseWield = tag "WIELD" *> parseWieldType where
  parseWieldType = (Light <$ labeled "Light")
               <|> (OneHanded <$ labeled "OneHanded")
               <|> (TwoHanded <$ labeled "TwoHanded")
               <|> (Unusable <$ labeled "Unusable")

-- only suitable for output.
parseRateOfFire :: PParser String
parseRateOfFire = tag "RATEOFFIRE" *> restOfTag

parseBaseItem :: PParser String
parseBaseItem = tag "BASEITEM" *> restOfTag

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
                <|> Contains <$> parseContains
                <|> RateOfFire <$> parseRateOfFire
                <|> BaseItem <$> parseBaseItem
                <|> Wield <$> parseWield
                <|> EquipmentModifier <$> parseEquipmentModifier
                <|> EquipmentType <$> parseEquipmentType

instance LSTObject EquipmentDefinition where
  parseSpecificTags = parseEquipmentTag
