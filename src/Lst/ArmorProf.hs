module Lst.ArmorProf where

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Control.Applicative
import Modifications
import Restrictions
import Common

data ArmorProficency = Name String
                     | Type String
                     | Restricted Restriction
                       deriving Show

parseArmorType :: Parser ArmorProficency
parseArmorType = Type <$> (tag "TYPE" >> parseWord)

parseArmorProficencyTag :: Parser ArmorProficency
parseArmorProficencyTag = parseArmorType <|>
                          Restricted <$> parseRestriction

parseArmorProficency :: String -> Parser [ArmorProficency]
parseArmorProficency armorName = do
  armorTags <- parseArmorProficencyTag `sepBy` tabs
  return $ armorTags ++ [Name armorName]

instance LSTObject ArmorProficency where
  parseLine = parseArmorProficency
