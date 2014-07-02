{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Bonus where

import qualified Data.Text as T
import Control.Monad(liftM)
import Data.Attoparsec.Text
import Control.Applicative
import JEPFormula
import Restrictions
import Common

data BonusToSkill = List
                  | All
                  | SkillName T.Text
                  | SkillType T.Text
                  | StatName T.Text
                    deriving Show

-- BONUS:SKILL:x,x,...|y
--   x is LIST, ALL, skill name, stat name (STAT.x), skill type (TYPE=x)
--   y is number, variable, formula
data BonusSkill = BonusSkill { toWhat :: [BonusToSkill]
                             , increaseBy :: Formula } deriving Show

parseBonusSkill :: Parser BonusSkill
parseBonusSkill = do
  _ <- string "BONUS:SKILL|"
  toWhat <- parseBonusSkills `sepBy` char ','
  increaseBy <- char '|' *> parseFormula
  return BonusSkill { .. } where
    parseBonusSkills = parseList <|>
                       parseAll <|>
                       parseSkillType <|>
                       parseStatName <|>
                       parseSkillName
    parseList = string "LIST" >> return List
    parseAll = string "ALL" >> return All
    parseSkillType = string "TYPE=" >> (SkillType <$> parseString)
    parseStatName = string "STAT." >> (StatName <$> parseString)
    parseSkillName = SkillName <$> parseString
