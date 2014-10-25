{-# LANGUAGE OverloadedStrings #-}

module Lst.Equipment where

import Text.Parsec.Char (char, satisfy)
import Text.Parsec.Combinator (sepBy1, many1)
import ClassyPrelude

import Modifications
import JEPFormula
import Common

data EquipmentDefinition = Description String
                         | Cost Formula
                         | Weight Formula
                         | ACCheck Formula
                         | Size EquipmentSize
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

parseEquipmentTag :: PParser EquipmentDefinition
parseEquipmentTag = Description <$> parseDescription
                <|> Weight <$> parseWeight
                <|> Cost <$> parseCost
                <|> ACCheck <$> parseACCheck
                <|> Size <$> parseSize
                <|> EquipmentType <$> parseEquipmentType

instance LSTObject EquipmentDefinition where
  parseSpecificTags = parseEquipmentTag
