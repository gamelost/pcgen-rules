module RestrictionTests where

import Text.Parsec.Prim
import Control.Applicative
import Test.HUnit
import Restrictions
import JEPFormula
import Bonus
import Common

parseRestrictionString :: String -> Restriction
parseRestrictionString = parseResult parseRestriction "parseRestriction"

testPreFeat = do
  parseRestrictionString preFeat1 @?= preFeatResult1
  parseRestrictionString preFeat2 @?= preFeatResult2 where
    preFeat1 = "PREFEAT:1,Psychoinventive Basics"
    preFeatResult1 =
      PreFeatRestriction PreFeat
        { featNumber = 1
        , feats = [ FeatName "Psychoinventive Basics" ]
        , countSeparately = False
        , cannotHave = False }
    preFeat2 = "PREFEAT:1,Psychometabolism,Wild Talent Psychic (Psychic (Adaptation))"
    preFeatResult2 =
      PreFeatRestriction PreFeat
        { featNumber = 1
        , feats = [ FeatName "Psychometabolism", FeatName "Wild Talent Psychic (Psychic (Adaptation))" ]
        , countSeparately = False
        , cannotHave = False }

testPreVar = do
  parseRestrictionString preVar1 @?= preVarResult1
  parseRestrictionString preVar2 @?= preVarResult2 where
    preVar1 = "PREVARLTEQ:skillinfo(\"TOTALRANK\",\"Acrobatics\"),MaxRankAcrobatics"
    preVarResult1 =
      PreVarRestriction PreVar
        { operator = LTEQ
        , variables = [ PreVarFormula (LookupSkill (TOTALRANK,"Acrobatics"))
                      , PreVarText "MaxRankAcrobatics" ] }
    preVar2 = "PREVARGTEQ:PsychometabolismBypass,1"
    preVarResult2 =
      PreVarRestriction PreVar
        { operator = GTEQ
        , variables = [ PreVarText "PsychometabolismBypass"
                      , PreVarFormula (Number 1) ] }

testPreMult =
  parseRestrictionString preMult1 @?= preMultResult1 where
    preMult1 = "PREMULT:1,[PREVARGTEQ:PsychometabolismBypass,1],\
                \[PREFEAT:1,Psychometabolism,Wild Talent Psychic (Psychic (Adaptation))]"
    preMultResult1 =
      PreMultipleRestriction PreMult
        { restrictionNumber = 1
        , restrictionsToPass = [ PreVarRestriction PreVar { operator = GTEQ
                                                           , variables = [ PreVarText "PsychometabolismBypass"
                                                                         , PreVarFormula (Number 1) ]}
                               , PreFeatRestriction PreFeat { featNumber = 1
                                                             , feats = [ FeatName "Psychometabolism"
                                                                       , FeatName "Wild Talent Psychic (Psychic (Adaptation))"]
                                                             , countSeparately = False
                                                             , cannotHave = False } ]}

restrictionTests :: Test
restrictionTests = TestList [ "parse prefeat" ~: testPreFeat
                            , "parse prevar" ~: testPreVar
                            , "parse premult" ~: testPreMult
                            ]
