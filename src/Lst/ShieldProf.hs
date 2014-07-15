module Lst.ShieldProf where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Control.Applicative
import Modifications
import Restrictions
import Common

data ShieldProficency = Name String
                      | Restricted Restriction
                        deriving Show

parseShieldProficencyTag :: Parser ShieldProficency
parseShieldProficencyTag = Restricted <$> parseRestriction

parseShieldProficency :: String -> Parser [ShieldProficency]
parseShieldProficency shieldName = do
  shieldTags <- tabs *> parseShieldProficencyTag `sepBy` tabs
  return $ shieldTags ++ [Name shieldName]

instance LSTObject ShieldProficency where
  parseLine = parseShieldProficency
