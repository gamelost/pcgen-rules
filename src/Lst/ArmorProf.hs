module Lst.ArmorProf where

import qualified Data.Text as T
import Control.Applicative
import Data.Attoparsec.Text
import Modifications
import Restrictions
import Common

data ArmorProficency = Name T.Text
                     | Type T.Text
                     | Restricted Restriction
                       deriving Show

parseArmorType :: Parser ArmorProficency
parseArmorType = Type <$> (tag "TYPE" >> parseWord)

parseArmorProficencyTag :: Parser ArmorProficency
parseArmorProficencyTag = parseArmorType <|>
                          Restricted <$> parseRestriction

parseArmorProficency :: T.Text -> Parser [ArmorProficency]
parseArmorProficency armorName = do
  armorTags <- parseArmorProficencyTag `sepBy` tabs
  return $ armorTags ++ [Name armorName]

instance LSTObject ArmorProficency where
  parseLine = parseArmorProficency
