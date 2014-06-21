module Lst.ArmorProf where

import qualified Data.Text as T
import Control.Applicative
import Data.Attoparsec.Text
import Modifications
import Common

data ArmorProf = ArmorProf { armorName :: T.Text
                           , armorType :: Maybe T.Text } deriving Show

type ArmorMod = Modification ArmorProf

parseArmorType :: Parser T.Text
parseArmorType = tag "TYPE" >> parseWord

parseArmorProficency :: Parser (T.Text, Operation) -> Parser ArmorMod
parseArmorProficency p = do
  (name, op) <- p <* tabs
  at <- optionMaybe parseArmorType
  return Modification { operation = op
                      , record = ArmorProf { armorName = name
                                           , armorType = at } }

parseArmorLine :: Parser ArmorMod
parseArmorLine = parseArmorProficency parseMod <|>
                 parseArmorProficency parseAdd
