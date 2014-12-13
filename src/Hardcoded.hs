module Hardcoded ( varBuiltins ) where

import ClassyPrelude

-- since we don't fully parse all variables yet, hard-code some to get
-- past the parser verification process.
--
-- this list needs to be cleaned up.
varBuiltins :: [String]
varBuiltins = [ "ACCHECK"
              , "AdvancedArcherPathBonus"
	      , "AdvancedAsceticPathBonus"
	      , "AdvancedBrawlingPathBonus"
	      , "AerialistFeatEvasion"
              , "AlchemistBombAdditionalDice"
              , "AlignmentAuraBase"
              , "AllowHolyAvenger"
              , "ALTPLUSTOTAL"
              , "AmbushHide"
              , "AmbushLvl"
              , "AntipaladinLVL"
	      , "ArcaneShieldBonus"
              , "ArcaneStrikeLVL"
              , "ArcherPathBonus"
              , "ArmorDPValue"
              , "ARMOR.SHIELD.EQUIPPED.ACBONUS"
              , "ArrowEnhancement"
              , "AsceticPathBonus"
              , "AstralSuitLVL"
              , "ATWILL"
              , "BAB"
              , "BASECOST"
              , "BaseCriticalDC"
              , "BaseInventionLevel"
              , "BASESPELLSTAT"
              , "BBB"
              , "BlindingSprayDC"
              , "BlindingSprayDurationDice"
              , "BlindingSprayDurationDieSize"
              , "BlindingSprayRange"
              , "BloodConsumptionBonus"
              ,	"BloodrankAdd"
              , "%CASTERLEVEL"
              , "CASTERLEVEL"
              , "CatNap"
              , "CHA"
              , "%CHARGES"
              , "CHASCORE"
              , "%CHOICE"
              , "CHOICE"
              , "CL"
	      , "ClericChannelEnergyLVL"
              , "CompanionExtraHD"
              , "CONSCORE"
              , "CON"
              , "COUNT[EQTYPE.ARMOR.EQUIPPED]" -- ??
              , "COUNT[EQTYPE.ARMOR.EQUIPPED.IS.HEAVY]"
              , "COUNT[EQTYPE.ARMOR.EQUIPPED.IS.MEDIUM]"
              , "COUNT[EQTYPE.ARMOR.IS.HEAVY.EQUIPPED]"
              , "COUNT[FEATNAME=Called Shot]"
              , "COUNT[FEATNAME=Combo Strike]"
              , "COUNT[FEATNAME=Evasion]"
              , "COUNT[FEATNAME=Extra Evolution]"
              , "COUNT[FEATNAME=Extra Summons]"
              , "COUNT[FEATNAME=Improved Power Resistance]"
              , "COUNT[FEATNAME=Licensed and Authorized]"
              , "COUNT[FEATNAME=Phobia]"
              , "COUNT[FEATNAME=Trace Enchantment II]"
              , "COUNT[FEATNAME=Trace Enchantment III]"
              , "COUNT[FEATNAME=Trace Enchantment IV]"
              , "COUNT[FEATTYPE=FavoredTech]"
              , "COUNT[FEATTYPE=MasterElectronic]"
              , "COUNT[FEATTYPE=MasterMechanical]"
              , "COUNT[FEATTYPE=Metapsionic]"
              , "COUNT[FEATTYPE=Psionic]"
              , "COUNT[FEATTYPE=VarisianTattoo]"
              , "CoverFire"
              , "CRITMULT"
              , "DancingRobesArmorBonus"
              , "DervishTranceBonus"
              , "DEX"
              , "DEXSCORE"
              , "DissonanceEnhancementBonusAlt"
              , "DissonanceEnhancementBonusMain"
              , "DMGDIE"
              , "DomainAbilityTriggerLVL"
              , "DomainLVL"
              , "DomainPowerTimes"
              , "DreadPrimeStat"
              , "EnchantArrow"
              , "EnduranceSwimmer"
              , "Enraged"
              , "EOMCasterLevel"
              , "EQHANDS"
              , "FamiliarBonusHP"
              , "FastSlip"
              , "FastPick"
              , "FavoredClassLevels"
              , "FavoredTech1"
              , "FeedbackDamage"
              , "FellBlowBonus"
              , "FlurryAttacks"
              , "FlurryBABBonus"
              , "FlurryExtraAttacks"
              , "FlurryOfFistsExtraAttacks"
              , "FlurryPenalty"
              , "Fortitude"
              , "FuryConBonus"
              , "FuryMorale"
              , "FuryStrBonus"
              , "Furied"
              , "GunGlyphMarksman"
              , "GunneryPenalty"
              , "HD"
              , "HEADPLUSTOTAL"
              , "InsanityGazeDC"
              , "Insanity"
              , "INT"
              , "INTSCORE"
              , "InventionLevel"
              , "InterceptorPathBonus"
	      , "IPRCount"
              , "ItemEgo"
              , "LeadershipFollowerLVL1"
              , "LeadershipFollowerLVL2"
              , "LeadershipFollowerLVL3"
              , "LeadershipFollowerLVL4"
              , "LeadershipFollowerLVL5"
              , "LeadershipFollowerLVL6"
              , "%LIST"
              , "MASTER"
              , "MasterLVL"
              , "MasterOfDisguise"
              , "MAX_LEVEL"
              , "MAXVEHICLEMODS"
              , "MeditantFlurryBABBonus"
              , "MeditantFlurryExtraAttacks"
              , "m_hp" -- ??
              , "MindBladeDAMAGESIZEADJ"
              , "MindBladeEnchantment"
              , "MindBladeRANGE"
              , "MinstrelMusicLevel"
              , "MODEQUIPMAXDEX"
              , "MOVEBASE"
              , "MXDXEN"
	      , "NauseatingBlowDC"
              , "NOB"
              , "PaladinLvl"
              , "PCrystalLevel"
              , "PLUSTOTAL"
              , "PowerDrainTime"
              , "PrecisionStrikeBonus"
              , "PsiBladeDamage"
              , "PsiBladeEnhancement"
              , "PsiCrystalLVL"
              , "PsionicFocusActive"
              , "PSIONLEVEL"
              , "PsiShieldDefense"
              , "PsychoMetabolism"
              , "RageACPenalty"
              , "RageConBonus"
              , "RageMorale"
              , "RageStrBonus"
              , "Reputation"
              , "RobotPurchaseDC"
              , "RobotSpeedIncreaseCost"
              , "SamuraiArmorBonus"
              , "sCargo"
              , "sCrewNUM"
              , "sCrewTrain"
              , "sDefense"
              , "sDefenseAuto"
              , "sDefenseFlat"
              , "Severis"
              , "SeverisEnhancementBonus"
              , "sGrapple"
              , "sGunnerAB"
              , "sHard"
              , "sHDpost"
              , "sHDpre"
              , "SHIELDACCHECK"
              , "ShieldBonus"
              , "ShimmerMailACBonus"
              , "ShipTypeMod"
              , "sInit"
              , "SIZE"
              , "SKILLRANK=Bluff"
              , "sLength"
              , "SneakAttackDice"
              , "SneakAttackDie"
              , "SneakAttack"
              , "sPass"
              , "SpecOpsLVL"
              , "%SPELLCOST"
              , "SPELLFAILURE"
              , "%SPELLLEVEL"
              , "SPELLLEVEL"
              , "SPELLSTAT"
              , "%SPELLXPCOST"
              , "sPilotCB"
              , "sPilotDEX"
              , "sSize"
              , "sTacSpeed"
              , "StaggerUndeadDC"
              , "sTargetingSB"
              , "STAT.0.BASE" -- ?
              , "STAT.3.BASE" -- ?
              , "STR"
              , "STRSCORE"
              , "sWeight"
              , "SynergyBonus"
              , "TL"
              , "TrapCR"
              , "TurnCheckBase"
              , "TurnCheckSnakes"
              , "TurnDamagePlusBase"
              , "TurnDamagePlusSnakes"
              , "TurnDiceBase"
              , "TurnDiceSnakes"
              , "TurnDieSizeBase"
              , "TurnDieSizeSnakes"
	      , "TurnLevelAnimals"
              , "TurnLevelBase"
              , "TurnLevelSnakes"
              , "TurnTimesAnimals"
              , "TurnTimesBase"
              , "TurnTimesSnakes"
              , "TwoWeaponShield"
              , "unit" -- ??
              , "ValBloodrank"
              , "vcargo" -- ??
              , "vcrew" -- ??
              , "vdefense" -- ??
              , "VEHICLEWOUNDPOINTS"
              , "vhard" -- ??
              , "vhpee" -- ??
              , "vinit" -- ??
              , "vmaneuver" -- ??
              , "v_max_speed" -- ??
              , "vpass" -- ??
              , "vt_speed" -- ??
              , "WarriorPathLVL"
              , "WEIGHT.CARRIED"
              , "WIS"
              , "WISSCORE"
              , "WT"

              ]
