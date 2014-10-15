module Lst.ShieldProf where

import Text.Parsec.Combinator (sepBy)
import ClassyPrelude

import Modifications
import Restrictions
import Common

data ShieldProficency = Name String
                      | Restricted Restriction
                        deriving Show

parseShieldProficencyTag :: PParser ShieldProficency
parseShieldProficencyTag = Restricted <$> parseRestriction

parseShieldProficency :: String -> PParser [ShieldProficency]
parseShieldProficency shieldName = do
  shieldTags <- tabs *> parseShieldProficencyTag `sepBy` tabs
  return $ shieldTags ++ [Name shieldName]

instance LSTObject ShieldProficency where
  parseLine = parseShieldProficency
