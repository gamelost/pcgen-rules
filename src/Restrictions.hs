{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Restrictions where

import Prelude hiding (takeWhile, GT, EQ, LT)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Common

data Operator = EQ | GT | GTEQ | LT | LTEQ | NEQ deriving Show

data Alignment = LG | LN | LE | NG | TN | NE | CG | CN | CE | None | Deity deriving Show

data PreVar = PreVar { operator :: Operator
                     , definition :: T.Text
                     , comparator :: Int } deriving Show

data PreAlign = PreAlign { alignments :: [Alignment] } deriving Show

data PreClass = PreClass { passNumber :: Int
                         , classRequisites :: [(T.Text, Int)] } deriving Show

data PreAbility = PreAbility { abilityNumber :: Int
                             , categoryName :: T.Text
                             , abilities :: [T.Text] } deriving Show

data PFeat = FeatName T.Text | FeatType T.Text deriving Show

data PreFeat = PreFeat { featNumber :: Int
                       , feats :: [PFeat]
                       , countSeparately :: Bool
                       , cannotHave :: Bool} deriving Show

data PSkill = PSkillName T.Text
            | PSkillType T.Text
              deriving Show

data PreSkill = PreSkill { skillNumber :: Int
                         , skills :: [(PSkill, Int)]} deriving Show

data Restriction = PreClassRestriction PreClass
                 | PreVarRestriction PreVar
                 | PreAlignRestriction PreAlign
                 | PreAbilityRestriction PreAbility
                 | PreFeatRestriction PreFeat
                 | PreSkillRestriction PreSkill
                 | Invert Restriction deriving Show

parseInvertedRestriction :: Parser Restriction -> Parser Restriction
parseInvertedRestriction p = char '!' >> Invert <$> p

-- PARSEALIGN:x,x...
--   x is alignment abbreviation or alignment array number
parsePreAlign :: Parser PreAlign
parsePreAlign = do
  args <- tag "PREALIGN" >> parseWord `sepBy` char ','
  return PreAlign { alignments = map parseAlignment args } where
    parseAlignment :: T.Text -> Alignment
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
parsePreClass :: Parser PreClass
parsePreClass = do
  n <- tag "PRECLASS" >> manyNumbers
  classRequisites <- char ',' >> parseEqual `sepBy` char ','
  return PreClass { passNumber = textToInt n, .. } where
    parseEqual :: Parser (T.Text, Int)
    parseEqual = do
      x <- parseString
      n <- char '=' >> manyNumbers
      return (x, textToInt n)

-- PREVARx:y,z
--   x is EQ, GT, GTEQ, LT, LTEQ, NEQ
--   y is text (must be in DEFINE: or BONUS:VAR)
--   z is number to be compared to
parsePreVar :: Parser PreVar
parsePreVar = do
  op <- string "PREVAR" >> choice ["EQ", "GTEQ", "GT", "LTEQ", "LT", "NEQ"]
  def <- char ':' >> parseWord
  n <- char ',' >> manyNumbers
  return PreVar { operator = convertOperator op
                , definition = def
                , comparator = textToInt n } where
    convertOperator :: T.Text -> Operator
    convertOperator "EQ" = EQ
    convertOperator "GT" = GT
    convertOperator "GTEQ" = GTEQ
    convertOperator "LT" = LT
    convertOperator "LTEQ" = LTEQ
    convertOperator "NEQ" = NEQ
    convertOperator _ = error "invalid PREVAR operator"

-- PREABILITY:x,CATEGORY=y,z,z,z...
--   x is the number of abilities needed
--   y is category name or ALL
--   z is ability name, ability type (TYPE.z), or ALL
parsePreAbility :: Parser PreAbility
parsePreAbility = do
  n <- tag "PREABILITY" >> manyNumbers
  categoryName <- string ",CATEGORY=" >> parseWord
  abilities <- char ',' >> parseString `sepBy` char ','
  return PreAbility { abilityNumber = textToInt n, .. }

-- PREFEAT:x,y,z,z,..
--   x is number of required feats
--   y can be CHECKMULT
--   z is feat name (or TYPE=type) ([] indicates inversion)
parsePreFeat :: Parser PreFeat
parsePreFeat = do
  n <- tag "PREFEAT" >> manyNumbers
  _ <- char ','
  countSeparately <- option False (string "CHECKMULT," >> return True)
  feats <- parseFeat `sepBy` char ','
  let cannotHave = False -- not implemented
  return PreFeat { featNumber = textToInt n, .. } where
    parseFeat = FeatType <$> (string "TYPE=" >> parseString) <|>
                FeatName <$> parseString

-- PRESKILL:x,y=z,y=z,..
--   x is number of skills
--   y is skill name or skill type (TYPE=y)
--   z is number of skill ranks
parsePreSkill :: Parser PreSkill
parsePreSkill = do
  n <- tag "PRESKILL" >> manyNumbers
  skills <- parseSkills `sepBy` char ','
  return PreSkill { skillNumber = textToInt n, .. } where
    parseSkills = do
      skill <- parseSkill
      val <- char '=' *> manyNumbers
      return (skill, textToInt val)
    parseSkill = PSkillType <$> (string "TYPE=" >> parseString) <|>
                 PSkillName <$> parseString

parsePossibleRestriction :: Parser Restriction
parsePossibleRestriction =
  PreVarRestriction <$> parsePreVar <|>
  PreClassRestriction <$> parsePreClass <|>
  PreAbilityRestriction <$> parsePreAbility <|>
  PreFeatRestriction <$> parsePreFeat <|>
  PreAlignRestriction <$> parsePreAlign <|>
  PreSkillRestriction <$> parsePreSkill

parseRestriction :: Parser Restriction
parseRestriction = parseInvertedRestriction parsePossibleRestriction <|>
                                            parsePossibleRestriction

-- for chained restrictions (e.g., BONUS tags)
parseAdditionalRestrictions :: Parser [Restriction]
parseAdditionalRestrictions = char '|' *> (parseRestriction `sepBy` char '|')
