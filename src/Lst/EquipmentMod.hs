{-# LANGUAGE OverloadedStrings #-}

module Lst.EquipmentMod where

import Text.Parsec.Char (char, noneOf, string)
import Text.Parsec.Combinator (sepBy, many1, option, notFollowedBy)
import Text.Parsec.Prim (many, try)
import ClassyPrelude hiding (try)

import Restrictions (RestrictionTag, parseAdditionalRestrictions)
import Modifications
import JEPFormula
import Common

data EquipmentModDefinition = Description String
                            | Visible VisibleTag
                            | Cost Formula
                            | SpecialProperty Property
                            | Plus Int
                            | Charges (Int, Int)
                            | ArmorType (String, String)
                            | AssignToAll Bool
                            | NameOpt NameOptType
                            | FormatCat FormatCatType
                            | Replaces [String]
                            | IType [String]
                            | Type [String]
                            | ChooseNoChoice ()
                            | ChooseEqBuilder EqBuilder
                            | ChooseString StringBuilder
                            | ChooseNumber ()
                            | ChooseEquipment [String]
                            | ChooseStatBonus ()
                            | ChooseSkillBonus ()
                            | ChooseWeaponProfBonus ()
                              deriving Show

data FormatCatType = CatFront
                   | CatMiddle
                   | CatParens
                     deriving (Show, Eq)

parseFormatCat :: PParser FormatCatType
parseFormatCat = do
  _ <- tag "FORMATCAT"
  parseFormatCatType where
    parseFormatCatType = (CatFront <$ labeled "FRONT")
                     <|> (CatMiddle <$ labeled "MIDDLE")
                     <|> (CatParens <$ labeled "PARENS")

data NameOptType = OptNoList
                 | OptNoName
                 | OptNormal
                 | OptNothing
                 | OptSpell
                 | OptText String
                   deriving (Show, Eq)

parseNameOpt :: PParser NameOptType
parseNameOpt = do
  _ <- tag "NAMEOPT"
  parseNameOptType where
    parseNameOptType = (OptNoList <$ labeled "NOLIST")
                   <|> (OptNoName <$ labeled "NONAME")
                   <|> (OptNormal <$ labeled "NORMAL")
                   <|> (OptNothing <$ labeled "NOTHING")
                   <|> (OptSpell <$ labeled "SPELL")
                   <|> (labeled "TEXT=" *> (OptText <$> parseString))

data VisibleTag = No
                | Yes
                | Qualify
                  deriving (Show, Eq)

parseVisible :: PParser VisibleTag
parseVisible = tag "VISIBLE" *> parseVisibleTag where
  parseVisibleTag = No <$ labeled "NO"
                <|> Yes <$ labeled "YES"
                <|> Qualify <$ labeled "QUALIFY"

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

parseArmorType :: PParser (String, String)
parseArmorType = do
  _ <- tag "ARMORTYPE"
  old <- parseString <* char '|'
  new <- parseString
  return (old, new)

parseCharges :: PParser (Int, Int)
parseCharges = do
  _ <- tag "CHARGES"
  fc <- parseInteger <* char '|'
  sc <- parseInteger
  return (fc, sc)

parseAssignToAll :: PParser Bool
parseAssignToAll = tag "ASSIGNTOALL" *> ((True <$ labeled "YES") <|> (False <$ labeled "NO"))

parseDescription :: PParser String
parseDescription = tag "DESC" *> restOfTag

parseReplaces :: PParser [String]
parseReplaces = tag "REPLACES" >> parseString `sepBy` char ','

parseType :: PParser [String]
parseType = tag "TYPE" >> parseString `sepBy` char '.'

parseIType :: PParser [String]
parseIType = tag "ITYPE" *> parseString `sepBy` char '.'

parseCost :: PParser Formula
parseCost = tag "COST" *> parseFormula

parsePlus :: PParser Int
parsePlus = tag "PLUS" *> parseInteger

parseChooseNoChoice :: PParser ()
parseChooseNoChoice = () <$ labeled "CHOOSE:NOCHOICE"

-- CHOOSE:EQBUILDER.SPELL|w|x|y|z
--   w is optional text
--   x is optional spell type (not used)
--   y is optional minimum level
--   z is optional maximum level
data EqBuilder = EqBuilder { eqBuilderText :: Maybe String
                           , eqBuilderMinimumLevel :: Formula
                           , eqBuilderMaximumLevel :: Formula }
               deriving (Show, Eq)

parseChooseEqBuilder :: PParser EqBuilder
parseChooseEqBuilder = do
  _ <- labeled "CHOOSE:EQBUILDER.SPELL"
  eqBuilderText <- tryOption $ char '|' *> parseString <* char '|'
  eqBuilderMinimumLevel <- option (Number 0) $ parseFormula <* char '|'
  eqBuilderMaximumLevel <- option (Variable "MAX_LEVEL") $ parseFormula
  return EqBuilder { .. }

-- CHOOSE:STRING|x|x..|y
--   x is choice to be offered
--   y is TITLE=text
data StringBuilder = StringBuilder { stringBuilderChoices :: [String]
                                   , stringBuilderTitle :: String }
                   deriving (Show, Eq)

parseChooseString :: PParser StringBuilder
parseChooseString = do
  _ <- labeled "CHOOSE:STRING|"
  stringBuilderChoices <- many1 (parseChoiceString <* char '|')
  stringBuilderTitle <- labeled "TITLE=" *> parseString
  return StringBuilder { .. } where
    parseChoiceString = try $ notFollowedBy (labeled "TITLE=") *> parseString

-- CHOOSE:NUMBER|v|w|x|y|z
--   not implemented. (NOSIGN/MULTIPLE seems to be undocumented)
parseChooseNumber :: PParser ()
parseChooseNumber = () <$ (labeled "CHOOSE:NUMBER|" >> restOfTag)

parseChooseEquipment :: PParser [String]
parseChooseEquipment = labeled "CHOOSE:EQUIPMENT|" *> parseEquipmentType `sepBy` char ',' where
  parseEquipmentType = labeled "TYPE=" *> parseString <|> parseString

-- CHOOSE:STATBONUS|w|x|y|z
--   not implemented.
parseChooseStatBonus :: PParser ()
parseChooseStatBonus = () <$ (labeled "CHOOSE:STATBONUS|" >> restOfTag)

-- CHOOSE:SKILLBONUS|w|x|y|z
--   not implemented.
parseChooseSkillBonus :: PParser ()
parseChooseSkillBonus = () <$ (labeled "CHOOSE:SKILLBONUS|" >> restOfTag)

-- CHOOSE:WEAPONPROFICIENCY|x
--   not implemented (or documented).
parseChooseWeaponProfBonus :: PParser ()
parseChooseWeaponProfBonus = () <$ (labeled "CHOOSE:WEAPONPROFICIENCY|" >> restOfTag)

parseEquipmentModTag :: PParser EquipmentModDefinition
parseEquipmentModTag = Description <$> parseDescription
                   <|> Visible <$> parseVisible
                   <|> Cost <$> parseCost
                   <|> SpecialProperty <$> parseSpecialProperty
                   <|> Plus <$> parsePlus
                   <|> Charges <$> parseCharges
                   <|> ArmorType <$> parseArmorType
                   <|> AssignToAll <$> parseAssignToAll
                   <|> NameOpt <$> parseNameOpt
                   <|> FormatCat <$> parseFormatCat
                   <|> Replaces <$> parseReplaces
                   <|> IType <$> parseIType
                   <|> Type <$> parseType
                   <|> ChooseNoChoice <$> parseChooseNoChoice
                   <|> ChooseEqBuilder <$> parseChooseEqBuilder
                   <|> ChooseString <$> parseChooseString
                   <|> ChooseNumber <$> parseChooseNumber
                   <|> ChooseEquipment <$> parseChooseEquipment
                   <|> ChooseStatBonus <$> parseChooseStatBonus
                   <|> ChooseSkillBonus <$> parseChooseSkillBonus
                   <|> ChooseWeaponProfBonus <$> parseChooseWeaponProfBonus

instance LSTObject EquipmentModDefinition where
  parseSpecificTags = parseEquipmentModTag
