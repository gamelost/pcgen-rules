{-# LANGUAGE OverloadedStrings #-}

module Lst.Spell where

import Text.Parsec.Char (char, satisfy)
import Text.Parsec.Combinator (sepBy, sepBy1, many1)
import ClassyPrelude

import Restrictions (RestrictionTag, parseRestriction)
import Modifications
import JEPFormula hiding (Add)
import Common

data SpellDefinition = Description String
                     | Descriptor [String]
                     | Classes Class
                     | Range String
                     | SaveInfo String
                     | CastTime String
                     | TargetArea String
                     | Duration String
                     | Schools [String]
                     | Components String
                     | SpellResistance String
                     | Type [String]
                     | SubSchool [String]
                     | Item ItemType
                     | CastingThreshold Int
                     | Cost Formula
                     | XPCost Int
                     | BonusLevelStat Stat
                     | Variants [String]
                     | Domains Domain
                     | SpellPointCosts [SpellPointCost]
                       deriving Show

data ClassType = ClassName String
               | ClassType String
               | AllClasses
                 deriving (Show, Eq)

data Class = Class { classTypes :: [([ClassType], Int)]
                   , classRestriction :: Maybe RestrictionTag }
           deriving (Show, Eq)

parseClasses :: PParser Class
parseClasses = do
  _ <- tag "CLASSES"
  classTypes <- parseClassTypes `sepBy` char '|'
  -- restrictions are denoted by [...]
  classRestriction <- tryOption (char '[' >> parseRestriction <* char ']')
  return Class { .. } where
    parseClassTypes = do
      types <- parseClassType `sepBy` char ','
      _ <- char '='
      number <- parseInteger
      return (types, number)
    parseClassType = AllClasses <$ labeled "ALL"
                 <|> (labeled "TYPE." *> (ClassType <$> parseStringNoCommas))
                 <|> (ClassName <$> parseStringNoCommas)

data Domain = Domain { domainNames :: [([String], Int)]
                     , domainRestriction :: Maybe RestrictionTag }
            deriving (Show, Eq)

parseDomains :: PParser Domain
parseDomains = do
  _ <- tag "DOMAINS"
  domainNames <- parseDomainTypes `sepBy` char '|'
  -- restrictions are denoted by [...]
  domainRestriction <- tryOption (char '[' >> parseRestriction <* char ']')
  return Domain { .. } where
    parseDomainTypes = do
      domains <- parseStringNoCommas `sepBy` char ','
      _ <- char '='
      number <- parseInteger
      return (domains, number)

data SpellPointCost = SpellPointCost { spellEffect :: String
                                     , spellCost :: Int }
                    deriving (Show, Eq)

parseSpellPointCost :: PParser [SpellPointCost]
parseSpellPointCost = tag "SPELLPOINTCOST" *> parseSPCostTypes `sepBy` char '|' where
  parseSPCostTypes = do
    spellEffect <- parseString <* char '='
    spellCost <- parseInteger
    return SpellPointCost { .. }

data Stat = Charisma
          | Constitution
          | Dexterity
          | Intelligence
          | Strength
          | Wisdom
            deriving (Show, Eq)

parseStat :: PParser Stat
parseStat = tag "STAT" *> parseStatType where
  parseStatType = (Charisma <$ labeled "CHA")
              <|> (Constitution <$ labeled "CON")
              <|> (Dexterity <$ labeled "DEX")
              <|> (Intelligence <$ labeled "INT")
              <|> (Strength <$ labeled "STR")
              <|> (Wisdom <$ labeled "WIS")

parseType :: PParser [String]
parseType = tag "TYPE" *> parseWordAndNumbers `sepBy1` char '.' where
  parseWordAndNumbers = many1 $ satisfy $ inClass "-A-Za-z0-9, _/"

data ItemType = Not String
              | Is String
                deriving (Show, Eq)

parseItem :: PParser ItemType
parseItem = tag "ITEM" *> parseItemType where
  parseItemType = Not <$> (char '[' >> parseStringNoBrackets <* char ']')
              <|> Is <$> parseString
  parseStringNoBrackets = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+,./:?!%#'()~"

-- TODO: too messy to parse properly.
parseComps :: PParser String
parseComps = tag "COMPS" *> restOfTag

-- TODO: too messy to parse properly.
parseSpellRes :: PParser String
parseSpellRes = tag "SPELLRES" *> restOfTag

-- TODO: parse formulas in parentheses
parseDuration :: PParser String
parseDuration = tag "DURATION" *> restOfTag

-- TODO: parse formulas in parentheses
parseTargetArea :: PParser String
parseTargetArea = tag "TARGETAREA" *> restOfTag

parseCT :: PParser Int
parseCT = tag "CT" *> parseInteger

parseXPCost :: PParser Int
parseXPCost = tag "XPCOST" *> parseInteger

parseCost :: PParser Formula
parseCost = tag "COST" *> parseFormula

parseSchool :: PParser [String]
parseSchool = tag "SCHOOL" *> parseString `sepBy` char '|'

parseCastTime :: PParser String
parseCastTime = tag "CASTTIME" *> restOfTag

parseSaveInfo :: PParser String
parseSaveInfo = tag "SAVEINFO" *> restOfTag

parseRange :: PParser String
parseRange = tag "RANGE" *> restOfTag

parseSubSchool :: PParser [String]
parseSubSchool = tag "SUBSCHOOL" *> parseString `sepBy1` char '|'

parseDescriptor :: PParser [String]
parseDescriptor = tag "DESCRIPTOR" *> parseStringEquals `sepBy1` char '|' where
   parseStringEquals = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+,./:?!%#'()[]~;="

parseVariants :: PParser [String]
parseVariants = tag "VARIANTS" *> parseString `sepBy1` char '|'

parseDescription :: PParser String
parseDescription = tag "DESC" *> restOfTag

parseSpellTag :: PParser SpellDefinition
parseSpellTag = Classes <$> parseClasses
            <|> Description <$> parseDescription
            <|> Descriptor <$> parseDescriptor
            <|> Range <$> parseRange
            <|> SaveInfo <$> parseSaveInfo
            <|> CastTime <$> parseCastTime
            <|> TargetArea <$> parseTargetArea
            <|> Duration <$> parseDuration
            <|> Schools <$> parseSchool
            <|> Components <$> parseComps
            <|> SpellResistance <$> parseSpellRes
            <|> Type <$> parseType
            <|> SubSchool <$> parseSubSchool
            <|> Item <$> parseItem
            <|> CastingThreshold <$> parseCT
            <|> Cost <$> parseCost
            <|> XPCost <$> parseXPCost
            <|> BonusLevelStat <$> parseStat
            <|> Variants <$> parseVariants
            <|> Domains <$> parseDomains
            <|> SpellPointCosts <$> parseSpellPointCost

instance LSTObject SpellDefinition where
  parseSpecificTags = parseSpellTag
