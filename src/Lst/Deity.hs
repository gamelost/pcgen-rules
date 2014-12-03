{-# LANGUAGE OverloadedStrings #-}

module Lst.Deity where

import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepBy)
import ClassyPrelude

import Modifications
import JEPFormula hiding (Add)
import Common

data DeityDefinition = Deity String
                     -- rest
                     | AlignmentType Alignment
                       deriving Show

data Alignment = LG | LN | LE | NG | TN | NE | CG | CN | CE
                 deriving (Show, Eq)

parseAlignmentType :: PParser Alignment
parseAlignmentType = tag "ALIGN" *> parseAlignment where
  parseAlignment = LG <$ (labeled "LG")
               <|> LN <$ (labeled "LN")
               <|> LE <$ (labeled "LE")
               <|> NG <$ (labeled "NG")
               <|> TN <$ (labeled "TN")
               <|> NE <$ (labeled "NE")
               <|> CG <$ (labeled "CG")
               <|> CN <$ (labeled "CN")
               <|> CE <$ (labeled "CE")

parseDeityTag :: PParser DeityDefinition
parseDeityTag = AlignmentType <$> parseAlignmentType

parseDeityBeginning :: PParser (LSTStart DeityDefinition, Operation)
parseDeityBeginning = do
  what <- Deity <$> parseString
  return (Block what, Add)

instance LSTObject DeityDefinition where
  parseBeginning = parseDeityBeginning
  parseSpecificTags = parseDeityTag
