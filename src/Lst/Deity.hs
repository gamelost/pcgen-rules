{-# LANGUAGE OverloadedStrings #-}

module Lst.Deity where

import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepBy, option)
import ClassyPrelude

import Restrictions (RestrictionTag, parseAdditionalRestrictions)
import Modifications
import Common

data DeityDefinition = Deity String
                     -- rest
                     | AlignmentType Alignment
                     | DomainType Domain
                     | Weapons [DeityWeapon]
                     | Pantheons [String]
                     | Description String
                     | Symbol String
                     | Title String
                     | Worshippers String
                     | Races [String]
                     | Type String
                     | Appearance String
                       deriving Show

data Alignment = LG | LN | LE | NG | TN | NE | CG | CN | CE
                 deriving (Show, Eq)

parseAlignmentType :: PParser Alignment
parseAlignmentType = tag "ALIGN" *> parseAlignment where
  parseAlignment = LG <$ labeled "LG"
               <|> LN <$ labeled "LN"
               <|> LE <$ labeled "LE"
               <|> NG <$ labeled "NG"
               <|> TN <$ labeled "TN"
               <|> NE <$ labeled "NE"
               <|> CG <$ labeled "CG"
               <|> CN <$ labeled "CN"
               <|> CE <$ labeled "CE"

data DomainType = DomainName String
                | AllDomains
                  deriving (Show, Eq)

data Domain = Domain { domains :: [DomainType]
                     , domainRestrictions :: [RestrictionTag] }
            deriving (Show, Eq)

parseDomains :: PParser Domain
parseDomains = do
  _ <- tag "DOMAINS"
  domains <- parseDomain `sepBy` char ','
  domainRestrictions <- option [] parseAdditionalRestrictions
  return Domain { .. } where
    parseDomain = (AllDomains <$ labeled "ALL")
              <|> (DomainName <$> parseStringNoCommas)

data DeityWeapon = WeaponName String
                 | AllWeapons
                   deriving (Show, Eq)

parseDeityWeapon :: PParser [DeityWeapon]
parseDeityWeapon = tag "DEITYWEAP" *> parseWeapon `sepBy` char '|' where
  parseWeapon = (AllWeapons <$ labeled "ALL")
            <|> (WeaponName <$> parseString)

parseDeityPantheon :: PParser [String]
parseDeityPantheon = tag "PANTHEON" *> parseString `sepBy` char '|'

parseRace :: PParser [String]
parseRace = tag "RACE" *> parseString `sepBy` char '|'

parseSymbol :: PParser String
parseSymbol = tag "SYMBOL" *> restOfTag

parseTitle :: PParser String
parseTitle = tag "TITLE" *> restOfTag

parseWoshippers :: PParser String
parseWoshippers = tag "WORSHIPPERS" *> restOfTag

parseDescription :: PParser String
parseDescription = tag "DESC" *> restOfTag

parseAppearance :: PParser String
parseAppearance = tag "APPEARANCE" *> restOfTag

parseType :: PParser String
parseType = tag "TYPE" *> restOfTag

parseDeityTag :: PParser DeityDefinition
parseDeityTag = AlignmentType <$> parseAlignmentType
            <|> DomainType <$> parseDomains
            <|> Weapons <$> parseDeityWeapon
            <|> Pantheons <$> parseDeityPantheon
            <|> Description <$> parseDescription
            <|> Symbol <$> parseSymbol
            <|> Title <$> parseTitle
            <|> Worshippers <$> parseWoshippers
            <|> Races <$> parseRace
            <|> Type <$> parseType
            <|> Appearance <$> parseAppearance

parseDeityBeginning :: PParser (LSTStart DeityDefinition, Operation)
parseDeityBeginning = do
  what <- Deity <$> parseString
  return (Block what, Add)

instance LSTObject DeityDefinition where
  parseBeginning = parseDeityBeginning
  parseSpecificTags = parseDeityTag
