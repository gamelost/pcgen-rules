module Lst.ArmorProf where

import Text.Parsec.Combinator
import Control.Applicative
import Modifications
import Restrictions
import Common

data ArmorProficency = Name String
                     | Type String
                     | Restricted Restriction
                       deriving Show

parseArmorType :: PParser ArmorProficency
parseArmorType = Type <$> (tag "TYPE" >> parseWord)

parseArmorProficencyTag :: PParser ArmorProficency
parseArmorProficencyTag = parseArmorType <|>
                          Restricted <$> parseRestriction

parseArmorProficency :: String -> PParser [ArmorProficency]
parseArmorProficency armorName = do
  armorTags <- parseArmorProficencyTag `sepBy` tabs
  return $ armorTags ++ [Name armorName]

instance LSTObject ArmorProficency where
  parseLine = parseArmorProficency
