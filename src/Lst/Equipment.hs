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

parseEquipmentTag :: PParser EquipmentDefinition
parseEquipmentTag = Description <$> parseDescription
                <|> Weight <$> parseWeight
                <|> Cost <$> parseCost
                <|> ACCheck <$> parseACCheck
                <|> EquipmentType <$> parseEquipmentType

instance LSTObject EquipmentDefinition where
  parseSpecificTags = parseEquipmentTag
