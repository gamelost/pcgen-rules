{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Restrictions where

import Prelude hiding (takeWhile, GT, EQ, LT)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Control.Applicative
import JEPFormula
import Common

data Restriction = PreClassRestriction PreClass
                 | PreClassSkillRestriction PreClassSkill
                 | PreVarRestriction PreVar
                 | PreAlignRestriction PreAlign
                 | PreAbilityRestriction PreAbility
                 | PreFeatRestriction PreFeat
                 | PreItemRestriction PreItem
                 | PreRaceRestriction PreRace
                 | PreMoveRestriction PreMove
                 | PreSkillRestriction PreSkill
                 | PreSkillTotalRestriction PreSkillTot
                 | PreRuleRestriction PreRule
                 | PreMultipleRestriction PreMult
                 | Invert Restriction
                   deriving (Show, Eq)

-- PREABILITY:x,CATEGORY=y,z,z,z...
--   x is the number of abilities needed
--   y is category name or ALL
--   z is ability name, ability type (TYPE.z), or ALL
data PreAbility = PreAbility { abilityNumber :: Int
                             , categoryName :: String
                             , abilities :: [String] }
                  deriving (Show, Eq)

parsePreAbility :: Parser PreAbility
parsePreAbility = do
  n <- tag "PREABILITY" >> manyNumbers
  categoryName <- string ",CATEGORY=" >> parseWordWithSpaces
  abilities <- char ',' >> parseString `sepBy` char ','
  return PreAbility { abilityNumber = textToInt n, .. } where
    parseWordWithSpaces = many1 $ satisfy $ inClass "-A-Za-z "

-- PARSEALIGN:x,x...
--   x is alignment abbreviation or alignment array number
data Alignment = LG | LN | LE | NG | TN | NE | CG | CN | CE | None | Deity
                 deriving (Show, Eq)

data PreAlign = PreAlign { alignments :: [Alignment] }
                deriving (Show, Eq)

parsePreAlign :: Parser PreAlign
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

parsePreClass :: Parser PreClass
parsePreClass = do
  n <- tag "PRECLASS" >> manyNumbers
  classRequisites <- char ',' >> parseEqual `sepBy` char ','
  return PreClass { passNumber = textToInt n, .. } where
    parseEqual :: Parser (String, Int)
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

parsePreClassSkill :: Parser PreClassSkill
parsePreClassSkill = do
  n <- tag "PRECSKILL" >> manyNumbers
  classSkill <- char ',' >> parseClassSkill
  return PreClassSkill { classSkillNumber = textToInt n, .. } where
    parseClassSkill = ClassSkillType <$> (string "TYPE=" >> parseString)
                  <|> ClassSkillName <$> parseString

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

parsePreFeat :: Parser PreFeat
parsePreFeat = do
  n <- tag "PREFEAT" >> manyNumbers
  _ <- char ','
  countSeparately <- option False (string "CHECKMULT," >> return True)
  feats <- parseFeat `sepBy` char ','
  let cannotHave = False -- not implemented
  return PreFeat { featNumber = textToInt n, .. } where
    parseFeat = FeatType <$> (string "TYPE=" >> parseStringNoCommas)
            <|> FeatName <$> parseStringNoCommas
    parseStringNoCommas = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+./:?!%#'()~"

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

parsePreItem :: Parser PreItem
parsePreItem = do
  n <- tag "PREITEM" >> manyNumbers
  items <- char ',' >> parseItems `sepBy` char ','
  return PreItem { itemNumber = textToInt n, .. } where
    parseItems = ItemType <$> (string "TYPE=" >> parseString)
             <|> (char '%' >> return AnyItem)
             <|> ItemType <$> parseString

-- PREMOVE:x,y=z,y=z...
--   x is minimum number movement types to pass
--   y is name of movement type
--   z is minimum number for the given movement type
data PreMove = PreMove { moveNumber :: Int
                       , moves :: [(String, Int)] }
               deriving (Show, Eq)

parsePreMove :: Parser PreMove
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
                       , restrictionsToPass :: [Restriction] }
             deriving (Show, Eq)

parsePreMult :: Parser PreMult
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

parsePreRace :: Parser PreRace
parsePreRace = do
  n <- tag "PRERACE" >> manyNumbers
  races <- char ',' >> parseRaces `sepBy` char ','
  return PreRace { raceNumber = textToInt n, .. } where
    parseRaces = RaceSubType <$> (string "RACESUBTYPE=" *> parseString)
             <|> RaceTypeType <$> (string "RACETYPE=" *> parseString)
             <|> RaceType <$> (string "TYPE=" *> parseString)
             <|> RaceName <$> parseString

-- PRERULE:x,y
--   x is number of rules required
--   y is rule name
data PreRule = PreRule { ruleNumber :: Int
                       , ruleName :: String }
               deriving (Show, Eq)

parsePreRule :: Parser PreRule
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

parsePreSkill :: Parser PreSkill
parsePreSkill = do
  n <- tag "PRESKILL" >> manyNumbers
  skills <- char ',' >> parseSkills `sepBy` char ','
  return PreSkill { skillNumber = textToInt n, .. } where
    parseSkills = do
      skill <- parseSkill
      val <- char '=' *> manyNumbers
      return (skill, textToInt val)
    parseSkill = SkillType <$> (string "TYPE=" >> parseString)
             <|> SkillName <$> parseString

-- PRESKILLTOT:x,x,...=y
--   x is skill name ($ skill type (TYPE=x)
--   y is total non-bonus skill ranks required
data PreSkillTot = PreSkillTot { skillTotals :: [Skill]
                               , skillTotalNeeded :: Int }
                   deriving (Show, Eq)

parsePreSkillTotal :: Parser PreSkillTot
parsePreSkillTotal = do
  _ <- tag "PRESKILLTOT"
  skillTotals <- parseSkills `sepBy` char ','
  n <- char '=' *> manyNumbers
  return PreSkillTot { skillTotalNeeded = textToInt n, .. } where
    parseSkills = SkillType <$> (string "TYPE=" >> parseString)
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

parsePreVar :: Parser PreVar
parsePreVar = do
  op <- string "PREVAR" >> (choice $ map string ["EQ", "GTEQ", "GT", "LTEQ", "LT", "NEQ"])
  variables <- char ':' >> parsePreVarType `sepBy` char ','
  return PreVar { operator = convertOperator op, .. } where
    parsePreVarType = PreVarFormula <$> parseFormula
                  <|> PreVarText <$> parseStringNoCommas
    convertOperator :: String -> Operator
    convertOperator "EQ" = EQ
    convertOperator "GTEQ" = GTEQ
    convertOperator "GT" = GT
    convertOperator "LTEQ" = LTEQ
    convertOperator "LT" = LT
    convertOperator "NEQ" = NEQ
    convertOperator _ = error "invalid PREVAR operator"
    parseStringNoCommas = many1 $ satisfy $ inClass "-A-Za-z0-9_ &+./:?!%#'()~"

parsePossibleRestriction :: Parser Restriction
parsePossibleRestriction = PreVarRestriction <$> parsePreVar
                       <|> PreClassSkillRestriction <$> parsePreClassSkill
                       <|> PreClassRestriction <$> parsePreClass
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

parseRestriction :: Parser Restriction
parseRestriction = parseInvertedRestriction parsePossibleRestriction
                                        <|> parsePossibleRestriction where
  parseInvertedRestriction p = char '!' >> Invert <$> p

-- for chained restrictions (e.g., BONUS tags)
parseAdditionalRestrictions :: Parser [Restriction]
parseAdditionalRestrictions = char '|' *> (parseRestriction `sepBy` char '|')
