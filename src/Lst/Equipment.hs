{-# LANGUAGE OverloadedStrings #-}

module Lst.Equipment where

import Text.Parsec.Char (char, satisfy)
import Text.Parsec.Combinator (sepBy1, option, many1)
import Text.Parsec.Prim (try)
import ClassyPrelude hiding (try)
import Prelude (read)

import Modifications
import JEPFormula
import Common

data EquipmentDefinition = Description String
                         | Cost Float
                         | Weight Float
                         | ACCheck Formula
                         | Size EquipmentSize
                         | EquipmentType [String]
                           deriving Show

textToFloat :: String -> Float
textToFloat t = read t :: Float

parseFloat :: PParser Float
parseFloat = do
  whole <- manyNumbers
  fract <- option "" $ try (char '.' *> manyNumbers)
  return . textToFloat $ whole ++ fract

parseWeight :: PParser Float
parseWeight = tag "WT" *> parseFloat

parseCost :: PParser Float
parseCost = tag "COST" *> parseFloat

parseACCheck :: PParser Formula
parseACCheck = tag "ACCHECK" *> parseFormula

parseEquipmentType :: PParser [String]
parseEquipmentType = tag "TYPE" *> parseWordAndNumbers `sepBy1` char '.' where
  parseWordAndNumbers = many1 $ satisfy $ inClass "-A-Za-z0-9"

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
