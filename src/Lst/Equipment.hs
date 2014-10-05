{-# LANGUAGE OverloadedStrings #-}

module Lst.Equipment where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding ((<|>))
import Control.Applicative
import Modifications
import Restrictions
import JEPFormula
import Lst.GlobalTags
import Common
import Bonus(parseBonus, Bonus)

data EquipmentDefinition = Name String
                         | Description String
                         | Cost Float
                         | Weight Float
                         | ACCheck Formula
                         | EquipmentType [String]
                         -- shared tags
                         | Global GlobalTag
                         | EquipmentBonus Bonus
                         | Restricted Restriction
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
parseDescription = tag "DESC" *> parseStringWithQuotes where
  parseStringWithQuotes = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+,./:?!%#'()[]~\";"

parseEquipmentTag :: PParser EquipmentDefinition
parseEquipmentTag = Description <$> parseDescription
                <|> Weight <$> parseWeight
                <|> Cost <$> parseCost
                <|> ACCheck <$> parseACCheck
                <|> EquipmentType <$> parseEquipmentType
                <|> EquipmentBonus <$> parseBonus
                <|> Restricted <$> parseRestriction
                <|> Global <$> parseGlobalTags

parseEquipment :: String -> PParser [EquipmentDefinition]
parseEquipment equipmentName = do
  equipmentTags <- tabs *> parseEquipmentTag `sepBy` tabs
  return $ Name equipmentName : equipmentTags

instance LSTObject EquipmentDefinition where
  parseLine = parseEquipment
