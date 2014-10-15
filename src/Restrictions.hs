{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Restrictions where

import Prelude hiding (takeWhile, GT, EQ, LT)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding ((<|>), many)
import Control.Applicative
import JEPFormula
import Common

data RestrictionTag = PreClassRestriction PreClass
                    | PreClassSkillRestriction PreClassSkill
                    | PreVarRestriction PreVar
                    | PreAlignRestriction PreAlign
                    | PreAbilityRestriction PreAbility
                    | PreDeityRestriction PreDeity
                    | PreDomainRestriction PreDomain
                    | PreFeatRestriction PreFeat
                    | PreItemRestriction PreItem
                    | PreRaceRestriction PreRace
                    | PreMoveRestriction PreMove
                    | PreSkillRestriction PreSkill
                    | PreSkillTotalRestriction PreSkillTot
                    | PreRuleRestriction PreRule
                    | PreMultipleRestriction PreMult
                    | Invert RestrictionTag
                      deriving (Show, Eq)

-- PREABILITY:x,CATEGORY=y,z,z,z...
--   x is the number of abilities needed
--   y is category name or ALL
--   z is ability name, ability type (TYPE.z), or ALL
data PreAbility = PreAbility { abilityNumber :: Int
                             , categoryName :: String
                             , abilities :: [String] }
                  deriving (Show, Eq)

parsePreAbility :: PParser PreAbility
parsePreAbility = do
  n <- tag "PREABILITY" >> manyNumbers
  categoryName <- labeled ",CATEGORY=" >> parseWordWithSpaces
  abilities <- char ',' >> parseString `sepBy` char ','
  return PreAbility { abilityNumber = textToInt n, .. } where
    parseWordWithSpaces = many1 $ satisfy $ inClass "-A-Za-z "

-- PARSEALIGN:x,x...
--   x is alignment abbreviation or alignment array number
data Alignment = LG | LN | LE | NG | TN | NE | CG | CN | CE | None | Deity
                 deriving (Show, Eq)

data PreAlign = PreAlign { alignments :: [Alignment] }
                deriving (Show, Eq)

parsePreAlign :: PParser PreAlign
parsePreAlign = do
  args <- tag "PREALIGN" >> parseWord `sepBy` char ','
  return PreAlign { alignments = map parseAlignment args } where
    parseAlignment :: String -> Alignment
    parseAlignment x | x == "LG", x == "0" = LG
    parseAlignment x | x == "LN", x == "1" = LN
    parseAlignment x | x == "LE", x == "2" = LE
    parseAlignment x | x == "NG", x == "3" = NG
    parseAlignment x | x == "TN", x == "4" = TN
    parseAlignment x | x == "NE", x == "5" = NE
    parseAlignment x | x == "CG", x == "6" = CG
    parseAlignment x | x == "CN", x == "7" = CN
    parseAlignment x | x == "CE", x == "8" = CE
    parseAlignment x | x == "Deity", x == "10" = Deity
    parseAlignment _ = None

-- PRECLASS:x,y=z,y=z,y=z...
--   x is number of classes to pass
--   y is class name or class type (TYPE.y) or SPELLCASTER. or SPELLCASTER.y
--   z is number, class level
data PreClass = PreClass { passNumber :: Int
                         , classRequisites :: [(String, Int)] }
                deriving (Show, Eq)

parsePreClass :: PParser PreClass
parsePreClass = do
  n <- tag "PRECLASS" >> manyNumbers
  classRequisites <- char ',' >> parseEqual `sepBy` char ','
  return PreClass { passNumber = textToInt n, .. } where
    parseEqual :: PParser (String, Int)
    parseEqual = do
      x <- parseString
      n <- char '=' >> manyNumbers
      return (x, textToInt n)

-- PRECSKILL:x,y
--   x is number of class skills
--   y is skill name or skill type (TYPE=y)
data ClassSkill = ClassSkillName String
                | ClassSkillType String
                  deriving (Show, Eq)

data PreClassSkill = PreClassSkill { classSkillNumber :: Int
                                   , classSkill :: ClassSkill }
                     deriving (Show, Eq)

parsePreClassSkill :: PParser PreClassSkill
parsePreClassSkill = do
  n <- tag "PRECSKILL" >> manyNumbers
  classSkill <- char ',' >> parseClassSkill
  return PreClassSkill { classSkillNumber = textToInt n, .. } where
    parseClassSkill = ClassSkillType <$> (labeled "TYPE=" >> parseString)
                  <|> ClassSkillName <$> parseString

-- PREDEITY:x,y
--   x is number of required deities or pantheons
--   y is Y, N, name, or PANTHEON.name
data Deity = Worship
           | NoWorship
           | DeityName String
           | PantheonName String
              deriving (Show, Eq)

data PreDeity = PreDeity { deityNumber :: Int
                         , deities :: [Deity] }
              deriving (Show, Eq)

parsePreDeity :: PParser PreDeity
parsePreDeity = do
  n <- tag "PREDEITY" >> manyNumbers
  _ <- char ','
  deities <- parseDeity `sepBy` char ','
  return PreDeity { deityNumber = textToInt n, .. } where
    parseDeity = (Worship <$ labeled "Y")
             <|> (NoWorship <$ labeled "N")
             <|> PantheonName <$> (labeled "PANTHEON." >> parseStringNoCommasBrackets)
             <|> DeityName <$> parseStringNoCommasBrackets

-- PREDOMAIN:x,y,y...
--   x is number of required deity's domains
--   y is domain names or ANY
data Domain = DomainName String
            | DomainAny
              deriving (Show, Eq)

data PreDomain = PreDomain { domainNumber :: Int
                           , domains :: [Domain] }
               deriving (Show, Eq)

parsePreDomain :: PParser PreDomain
parsePreDomain = do
  n <- tag "PREDOMAIN" >> manyNumbers
  _ <- char ','
  domains <- parseDomain `sepBy` char ','
  return PreDomain { domainNumber = textToInt n, .. } where
    parseDomain = (DomainAny <$ labeled "ANY")
              <|> DomainName <$> parseStringNoCommasBrackets

-- PREFEAT:x,y,z,z,..
--   x is number of required feats
--   y can be CHECKMULT
--   z is feat name (or TYPE=type) ([] indicates inversion)
data Feat = FeatName String
          | FeatType String
            deriving (Show, Eq)

data PreFeat = PreFeat { featNumber :: Int
                       , feats :: [Feat]
                       , countSeparately :: Bool
                       , cannotHave :: Bool}
               deriving (Show, Eq)

parsePreFeat :: PParser PreFeat
parsePreFeat = do
  n <- tag "PREFEAT" >> manyNumbers
  _ <- char ','
  countSeparately <- option False (True <$ labeled "CHECKMULT,")
  feats <- parseFeat `sepBy` char ','
  let cannotHave = False -- not implemented
  return PreFeat { featNumber = textToInt n, .. } where
    parseFeat = FeatType <$> (labeled "TYPE=" >> parseStringNoCommasBrackets)
            <|> FeatName <$> parseStringNoCommasBrackets

-- PREITEM:x,y,y,...
--   x is number of items a character must possess
--   y is text, type, or wildcard (%)
data Item = ItemName String
          | ItemType String
          | AnyItem
            deriving (Show, Eq)

data PreItem = PreItem { itemNumber :: Int
                       , items :: [Item] }
             deriving (Show, Eq)

parsePreItem :: PParser PreItem
parsePreItem = do
  n <- tag "PREITEM" >> manyNumbers
  items <- char ',' >> parseItems `sepBy` char ','
  return PreItem { itemNumber = textToInt n, .. } where
    parseItems = ItemType <$> (labeled "TYPE=" >> parseString)
             <|> (AnyItem <$ char '%')
             <|> ItemType <$> parseString

-- PREMOVE:x,y=z,y=z...
--   x is minimum number movement types to pass
--   y is name of movement type
--   z is minimum number for the given movement type
data PreMove = PreMove { moveNumber :: Int
                       , moves :: [(String, Int)] }
               deriving (Show, Eq)

parsePreMove :: PParser PreMove
parsePreMove = do
  n <- tag "PREMOVE" >> manyNumbers
  moves <- char ',' >> parseMoves `sepBy` char ','
  return PreMove { moveNumber = textToInt n, .. } where
    parseMoves = do
      moveType <- parseString
      moveMinimum <- char '=' >> manyNumbers
      return (moveType, textToInt moveMinimum)

-- PREMULT:x,y,y...
--   x is number of restrictions to pass
--   y is any restriction in square brackets
data PreMult = PreMult { restrictionNumber :: Int
                       , restrictionsToPass :: [RestrictionTag] }
             deriving (Show, Eq)

parsePreMult :: PParser PreMult
parsePreMult = do
  n <- tag "PREMULT" >> manyNumbers
  restrictionsToPass <- char ',' >> parseRestrictions `sepBy` char ','
  return PreMult { restrictionNumber = textToInt n, .. } where
    parseRestrictions = char '[' >> parseRestriction <* char ']'

-- PRERACE:x,y,y...
--   x is number of racial properties
--   y is name of race, type, racetype, racesubtype
data Race = RaceName String
          | RaceType String
          | RaceTypeType String
          | RaceSubType String
            deriving (Show, Eq)

data PreRace = PreRace { raceNumber :: Int
                       , races :: [Race ] }
             deriving (Show, Eq)

parsePreRace :: PParser PreRace
parsePreRace = do
  n <- tag "PRERACE" >> manyNumbers
  races <- char ',' >> parseRaces `sepBy` char ','
  return PreRace { raceNumber = textToInt n, .. } where
    parseRaces = RaceSubType <$> (labeled "RACESUBTYPE=" *> parseString)
             <|> RaceTypeType <$> (labeled "RACETYPE=" *> parseString)
             <|> RaceType <$> (labeled "TYPE=" *> parseString)
             <|> RaceName <$> parseString

-- PRERULE:x,y
--   x is number of rules required
--   y is rule name
data PreRule = PreRule { ruleNumber :: Int
                       , ruleName :: String }
               deriving (Show, Eq)

parsePreRule :: PParser PreRule
parsePreRule = do
  n <- tag "PRERULE" >> manyNumbers
  _ <- char ','
  ruleName <- parseString -- not correct but will do for now
  return PreRule { ruleNumber = textToInt n, .. }

-- PRESKILL:x,y=z,y=z,..
--   x is number of skills
--   y is skill name or skill type (TYPE=y)
--   z is number of skill ranks
data Skill = SkillName String
           | SkillType String
             deriving (Show, Eq)

data PreSkill = PreSkill { skillNumber :: Int
                         , skills :: [(Skill, Int)]}
                deriving (Show, Eq)

parsePreSkill :: PParser PreSkill
parsePreSkill = do
  n <- tag "PRESKILL" >> manyNumbers
  skills <- char ',' >> parseSkills `sepBy` char ','
  return PreSkill { skillNumber = textToInt n, .. } where
    parseSkills = do
      skill <- parseSkill
      val <- char '=' *> manyNumbers
      return (skill, textToInt val)
    parseSkill = SkillType <$> (labeled "TYPE=" >> parseString)
             <|> SkillName <$> parseString

-- PRESKILLTOT:x,x,...=y
--   x is skill name ($ skill type (TYPE=x)
--   y is total non-bonus skill ranks required
data PreSkillTot = PreSkillTot { skillTotals :: [Skill]
                               , skillTotalNeeded :: Int }
                   deriving (Show, Eq)

parsePreSkillTotal :: PParser PreSkillTot
parsePreSkillTotal = do
  _ <- tag "PRESKILLTOT"
  skillTotals <- parseSkills `sepBy` char ','
  n <- char '=' *> manyNumbers
  return PreSkillTot { skillTotalNeeded = textToInt n, .. } where
    parseSkills = SkillType <$> (labeled "TYPE=" >> parseString)
              <|> SkillName <$> parseString

-- PREVARx:y,z
--   x is EQ, GT, GTEQ, LT, LTEQ, NEQ
--   y is text (must be in DEFINE: or BONUS:VAR)
--   z is number to be compared to
--
-- NB: documentation does not seem quite right. each y,z parameter is
-- probably a formula. for now, we read in unknown variables as Text.
data Operator = EQ | GT | GTEQ | LT | LTEQ | NEQ
                deriving (Show, Eq)

data PreVarType = PreVarFormula Formula
                | PreVarText String
                  deriving (Show, Eq)

data PreVar = PreVar { operator :: Operator
                     , variables :: [PreVarType] } deriving (Show, Eq)

parsePreVar :: PParser PreVar
parsePreVar = do
  op <- labeled "PREVAR" >> choice prefixes
  variables <- char ':' >> parsePreVarType `sepBy` char ','
  return PreVar { operator = convertOperator op, .. } where
    prefixes = tryStrings ["EQ", "GTEQ", "GT", "LTEQ", "LT", "NEQ"]
    parsePreVarType = PreVarFormula <$> try parseFormula
                  <|> PreVarText <$> parseStringNoCommasBrackets
    convertOperator :: String -> Operator
    convertOperator "EQ" = EQ
    convertOperator "GTEQ" = GTEQ
    convertOperator "GT" = GT
    convertOperator "LTEQ" = LTEQ
    convertOperator "LT" = LT
    convertOperator "NEQ" = NEQ
    convertOperator _ = error "invalid PREVAR operator"

parsePossibleRestriction :: PParser RestrictionTag
parsePossibleRestriction = PreVarRestriction <$> parsePreVar
                       <|> PreClassSkillRestriction <$> parsePreClassSkill
                       <|> PreClassRestriction <$> parsePreClass
                       <|> PreDeityRestriction <$> parsePreDeity
                       <|> PreDomainRestriction <$> parsePreDomain
                       <|> PreAbilityRestriction <$> parsePreAbility
                       <|> PreFeatRestriction <$> parsePreFeat
                       <|> PreItemRestriction <$> parsePreItem
                       <|> PreRaceRestriction <$> parsePreRace
                       <|> PreMoveRestriction <$> parsePreMove
                       <|> PreRuleRestriction <$> parsePreRule
                       <|> PreAlignRestriction <$> parsePreAlign
                       <|> PreSkillTotalRestriction <$> parsePreSkillTotal
                       <|> PreSkillRestriction <$> parsePreSkill
                       <|> PreMultipleRestriction <$> parsePreMult

parseRestriction :: PParser RestrictionTag
parseRestriction = parseInvertedRestriction parsePossibleRestriction
                                        <|> parsePossibleRestriction where
  parseInvertedRestriction p = char '!' >> Invert <$> p

-- for chained restrictions (e.g., BONUS tags)
parseAdditionalRestrictions :: PParser [RestrictionTag]
parseAdditionalRestrictions = many $ try restrictions where
  restrictions = char '|' >> parseRestriction
