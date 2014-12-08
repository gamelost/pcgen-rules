{-# LANGUAGE OverloadedStrings #-}

module Lst.EquipmentMod where

import Text.Parsec.Char (char, noneOf, string)
import Text.Parsec.Combinator (sepBy, many1, option, notFollowedBy)
import Text.Parsec.Prim (many)
import ClassyPrelude

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

instance LSTObject EquipmentModDefinition where
  parseSpecificTags = parseEquipmentModTag
