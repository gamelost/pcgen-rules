{-# LANGUAGE OverloadedStrings #-}

module BonusTests where

import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Test.HUnit
import Restrictions
import JEPFormula
import Bonus
import Common

parseBonusString :: T.Text -> Bonus
parseBonusString contents = parseResult "parseBonus" $ parse parseBonus contents

testSkillBonus = do
  parseBonusString skillBonus1 @?= skillResult1
  parseBonusString skillBonus2 @?= skillResult2
  parseBonusString skillBonus3 @?= skillResult3 where
    skillBonus1 = "BONUS:SKILL|Climb|8|PREMOVE:1,Climb=1|TYPE=Racial"
    skillResult1 =
      BonusSkill Skill
        { bonusToSkills = [ BonusSkillName "Climb" ]
        , skillFormula = SkillFormula ( Number 8 )
        , skillType = Just "Racial"
        , skillStack = False
        , skillRestrictions =
            [ PreMoveRestriction
                PreMove { moveNumber = 1 , moves = [ ( "Climb" , 1 ) ] }
            ]
        }
    skillBonus2 = "BONUS:SKILL|Craft (Gunsmith)|SynergyBonus|\
                  \PRESKILL:1,Craft (Blacksmithing)=5,\
                  \Craft (Weaponsmithing)=5,\
                  \Profession (Artillery)=5|TYPE=Synergy.STACK"
    skillResult2 =
      BonusSkill Skill
        { bonusToSkills = [ BonusSkillName "Craft (Gunsmith)" ]
        , skillFormula = SkillFormula ( Variable "SynergyBonus" )
        , skillType = Just "Synergy"
        , skillStack = True
        , skillRestrictions =
            [ PreSkillRestriction
                PreSkill { skillNumber = 1
                         , skills = [ ( SkillName "Craft (Blacksmithing)", 5 )
                                    , ( SkillName "Craft (Weaponsmithing)", 5 )
                                    , ( SkillName "Profession (Artillery)", 5 ) ] }
            ]
        }
    skillBonus3 = "BONUS:SKILL|Deception (Act in character)|SKILL.Deception.MISC"
    skillResult3 =
      BonusSkill Skill
        { bonusToSkills = [ BonusSkillName "Deception (Act in character)" ]
        , skillFormula = SkillText "SKILL.Deception.MISC"
        , skillType = Nothing
        , skillStack = False
        , skillRestrictions = [ ]
        }

testSkillRankBonus =
  parseBonusString skillRankBonus1 @?= skillRankResult1 where
    skillRankBonus1 = "BONUS:SKILLRANK|Acrobatics (On ship)|\
                       \skillinfo(\"TOTALRANK\", \"Acrobatics\")|TYPE=SkillGranted|\
                       \PREVARNEQ:var(\"SKILL.Acrobatics (On ship).MISC\"),SKILL.Acrobatics.MISC"
    skillRankResult1 =
      BonusSkillRank SkillRank
        { skillRanks = [ SkillRankName "Acrobatics (On ship)" ]
        , skillRankFormula = LookupSkill ( TOTALRANK, "Acrobatics" )
        , skillRankType = Just "SkillGranted"
        , skillRankRestrictions =
            [ PreVarNeqRestriction
                PreVarNeq { variables = [ PreVarFormula ( LookupVariable "SKILL.Acrobatics (On ship).MISC" )
                                        , PreVarText "SKILL.Acrobatics.MISC" ] }
            ]
        }

testTempBonus = do
  parseBonusString tempBonus1 @?= tempResult1
  parseBonusString tempBonus2 @?= tempResult2 where
    tempBonus1 = "TEMPBONUS:PC|SKILL|Craft (Fletcher)|-2|TYPE=Circumstance|\
                  \!PREITEM:1,TYPE=WeaponsmithingTools"
    tempResult1 =
      TemporaryBonus TempBonus
        { target = PC
        , equipmentType = Nothing
        , additionalBonuses =
            [ BonusSkill
                Skill
                  { bonusToSkills = [ BonusSkillName "Craft (Fletcher)" ]
                  , skillFormula = SkillFormula (Number (-2))
                  , skillType = Just "Circumstance"
                  , skillStack = False
                  , skillRestrictions = []
                  }
            ]
        , additionalRestrictions =
            [ Invert $ PreItemRestriction
                PreItem
                { itemNumber = 1
                , items = [ ItemType "WeaponsmithingTools" ]
                }
            ]
        }
    tempBonus2 = "TEMPBONUS:PC|SKILL|Disguise|SynergyBonus|PRESKILL:1,Bluff=5|TYPE=TempSynergy"
    tempResult2 =
      TemporaryBonus TempBonus
        { target = PC
        , equipmentType = Nothing
        , additionalBonuses =
            [ BonusSkill
                Skill
                  { bonusToSkills = [ BonusSkillName "Disguise" ]
                  , skillFormula = SkillFormula ( Variable "SynergyBonus" )
                  , skillType = Just "Circumstance"
                  , skillStack = False
                  , skillRestrictions = []
                  }
            ]
        , additionalRestrictions = []
        }

bonusTests :: Test
bonusTests = TestList [ "parse skill bonuses" ~: testSkillBonus
                      , "parse skill rank bonuses" ~: testSkillRankBonus
                      , "parse temporary bonuses" ~: testTempBonus
                      ]
