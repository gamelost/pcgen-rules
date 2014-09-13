module BonusTests where

import Text.Parsec.Prim
import Control.Applicative
import Test.HUnit
import Restrictions
import JEPFormula
import Bonus
import Common

parseBonusString :: String -> Bonus
parseBonusString contents = parseResult $ parse parseBonus "parseBonus" contents

testSkillBonus = do
  parseBonusString skillBonus1 @?= skillResult1
  parseBonusString skillBonus2 @?= skillResult2
  parseBonusString skillBonus3 @?= skillResult3
  parseBonusString skillBonus4 @?= skillResult4
  parseBonusString skillBonus5 @?= skillResult5
  parseBonusString skillBonus6 @?= skillResult6 where
    skillBonus1 = "BONUS:SKILL|Climb|8|PREMOVE:1,Climb=1|TYPE=Racial"
    skillResult1 =
      BonusSkill Skill
        { bonusToSkills = [ BonusSkillName "Climb" ]
        , skillFormula = SkillFormula ( Number 8 )
        , skillType = Just ("Racial", False)
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
        , skillType = Just ("Synergy", True)
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
        , skillRestrictions = [ ]
        }
    skillBonus4 = "BONUS:SKILL|Sleight of Hand,Diplomacy,Intimidate|SynergyBonus|TYPE=Synergy.STACK|PRESKILL:1,Bluff=5"
    skillResult4 =
      BonusSkill Skill
        { bonusToSkills = [ BonusSkillName "Sleight of Hand"
                          , BonusSkillName "Diplomacy"
                          , BonusSkillName "Intimidate" ]
        , skillFormula = SkillFormula (Variable "SynergyBonus")
        , skillType = Just ("Synergy", True)
        , skillRestrictions =
            [ PreSkillRestriction
                PreSkill { skillNumber = 1
                         , skills = [ ( SkillName "Bluff", 5 ) ] }
            ]
        }
    skillBonus5 = "BONUS:SKILL|Knowledge (Reverie)|2|PRERACE:1,Elf%"
    skillResult5 =
      BonusSkill Skill
        { bonusToSkills = [ BonusSkillName "Knowledge (Reverie)" ]
        , skillFormula = SkillFormula (Number 2)
        , skillType = Nothing
        , skillRestrictions =
            [ PreRaceRestriction
                PreRace { raceNumber = 1 , races = [ RaceName "Elf%" ] }
            ]
        }
    -- not sure about this one, since SKILL.Intimidate.MISC does not seem to be a variable.
    skillBonus6 = "BONUS:SKILL|Intimidate (Charisma),Intimidate (Strength)|SKILL.Intimidate.MISC"
    skillResult6 =
      BonusSkill Skill
        { bonusToSkills = [ BonusSkillName "Intimidate (Charisma)", BonusSkillName "Intimidate (Strength)"]
        , skillFormula = SkillText "SKILL.Intimidate.MISC"
        , skillType = Nothing
        , skillRestrictions = []}

testSkillRankBonus = do
  parseBonusString skillRankBonus1 @?= skillRankResult1
  parseBonusString skillRankBonus2 @?= skillRankResult2 where
    skillRankBonus1 = "BONUS:SKILLRANK|Acrobatics (On ship)|\
                       \skillinfo(\"TOTALRANK\", \"Acrobatics\")|TYPE=SkillGranted|\
                       \PREVARNEQ:var(\"SKILL.Acrobatics (On ship).MISC\"),SKILL.Acrobatics.MISC"
    skillRankResult1 =
      BonusSkillRank SkillRank
        { skillRanks = [ SkillRankName "Acrobatics (On ship)" ]
        , skillRankFormula = SkillFormula (LookupSkill ( TOTALRANK, "Acrobatics" ))
        , skillRankType = Just ("SkillGranted", False)
        , skillRankRestrictions =
            [ PreVarRestriction
                PreVar { operator = NEQ
                       , variables = [ PreVarFormula ( LookupVariable "SKILL.Acrobatics (On ship).MISC" )
                                     , PreVarText "SKILL.Acrobatics.MISC" ] }
            ]
        }
    -- not sure about this one either, since SKILLRANK=Intimidate does not seem to be a variable.
    skillRankBonus2 = "BONUS:SKILLRANK|Intimidate (Charisma), Intimidate (Strength)|SKILLRANK=Intimidate"
    skillRankResult2 =
      BonusSkillRank SkillRank
        { skillRanks = [ SkillRankName "Intimidate (Charisma)", SkillRankName "Intimidate (Strength)" ]
        , skillRankFormula = SkillText "Intimidate"
        , skillRankType = Nothing
        , skillRankRestrictions = []
        }

testVariableBonus = do
  parseBonusString bonusVar1 @?= bonusVarResult1
  parseBonusString bonusVar2 @?= bonusVarResult2 where
    bonusVar1 = "BONUS:VAR|MartialArtsSkillTotal|skillinfo(\"TOTAL\",\"Martial Arts\")|TYPE=MartialArts"
    bonusVarResult1 =
      BonusVariable BonusVar
        { bonusVariables = [ "MartialArtsSkillTotal" ]
        , adjustBy = LookupSkill ( TOTAL , "Martial Arts" )
        , bonusVarType = Just ("MartialArts", False)
        , bonusVarRestrictions = [ ]
        }
    bonusVar2 = "BONUS:VAR|AlchemyFeat|3|PRESKILLTOT:Alchemy=9|TYPE=NoStack"
    bonusVarResult2 =
      BonusVariable BonusVar
        { bonusVariables = [ "AlchemyFeat" ]
        , adjustBy = Number 3
        , bonusVarType = Just ("NoStack", False)
        , bonusVarRestrictions =
            [ PreSkillTotalRestriction
                PreSkillTot { skillTotals = [ SkillName "Alchemy" ] , skillTotalNeeded = 9 }
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
                  , skillType = Just ("Circumstance", False)
                  , skillRestrictions =
                      [ Invert $ PreItemRestriction
                        PreItem
                        { itemNumber = 1
                        , items = [ ItemType "WeaponsmithingTools" ]
                        }
                      ]
                  }
            ]
        , additionalRestrictions = []
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
                  , skillType = Just ("TempSynergy", False)
                  , skillRestrictions =
                      [ PreSkillRestriction
                        PreSkill
                         { skillNumber = 1
                         , skills = [(SkillName "Bluff",5)]
                         }
                      ]
                  }
            ]
        , additionalRestrictions = []
        }

bonusTests :: Test
bonusTests = TestList [ "parse skill bonuses" ~: testSkillBonus
                      , "parse skill rank bonuses" ~: testSkillRankBonus
                      , "parse temporary bonuses" ~: testTempBonus
                      , "parse variable bonuses" ~: testVariableBonus
                      ]
