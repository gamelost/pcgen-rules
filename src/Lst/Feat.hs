{-# LANGUAGE OverloadedStrings #-}

module Lst.Feat where

import Text.Parsec.Char (char, satisfy)
import Text.Parsec.Combinator (sepBy, sepBy1, many1)
import ClassyPrelude

import Modifications
import JEPFormula hiding (Add)
import Common

data FeatDefinition = Description String
                    | Visible Visibility
                    | Type [String]
                    | Multiple Bool
                    | CanStack Bool
                    | Benefit String
                    | Cost Formula
                    | AddSpellLevel Formula
                    | TemplateChoice [String]
                    | Template [String]
                    | AppliedName String
                    | Aspect String
                       deriving Show

parseDescription :: PParser String
parseDescription = tag "DESC" *> restOfTag

parseType :: PParser [String]
parseType = tag "TYPE" *> parseWordAndNumbers `sepBy1` char '.' where
  parseWordAndNumbers = many1 $ satisfy $ inClass "-A-Za-z0-9, _/"

parseMult :: PParser Bool
parseMult = tag "MULT" *> yesOrNo

parseStack :: PParser Bool
parseStack = tag "STACK" *> yesOrNo

parseCost :: PParser Formula
parseCost = tag "COST" *> parseFormula

parseAddSpellLevel :: PParser Formula
parseAddSpellLevel = tag "ADDSPELLLEVEL" *> parseFormula

parseAppliedName :: PParser String
parseAppliedName = tag "APPLIEDNAME" *> restOfTag

parseTemplate :: PParser [String]
parseTemplate = tag "TEMPLATE" *> parseString `sepBy` char '|'

parseTemplateChoose :: PParser [String]
parseTemplateChoose = tag "TEMPLATE:CHOOSE" *> parseString `sepBy` char '|'

-- TODO:
-- parse properly (%1 especially)
parseAspect :: PParser String
parseAspect = tag "ASPECT" *> restOfTag

-- TODO:
-- parse properly
parseBenefit :: PParser String
parseBenefit = tag "BENEFIT" *> restOfTag

data Visibility = IsVisible
                | NotVisible
                | Display
                | Export
                  deriving (Show, Eq)

parseVisible :: PParser Visibility
parseVisible = tag "VISIBLE" *> parseVisibility where
  parseVisibility = IsVisible <$ (labeled "YES")
                <|> NotVisible <$ (labeled "NO")
                <|> Display <$ (labeled "DISPLAY")
                <|> Export <$ (labeled "EXPORT")

parseFeatTag :: PParser FeatDefinition
parseFeatTag = Description <$> parseDescription
           <|> Type <$> parseType
           <|> Visible <$> parseVisible
           <|> Multiple <$> parseMult
           <|> CanStack <$> parseStack
           <|> Benefit <$> parseBenefit
           <|> Cost <$> parseCost
           <|> AddSpellLevel <$> parseAddSpellLevel
           <|> TemplateChoice <$> parseTemplateChoose
           <|> Template <$> parseTemplate
           <|> AppliedName <$> parseAppliedName
           <|> Aspect <$> parseAspect

instance LSTObject FeatDefinition where
  parseSpecificTags = parseFeatTag
