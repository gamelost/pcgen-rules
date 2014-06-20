{-# LANGUAGE OverloadedStrings #-}

module Lst.ArmorProf where

import qualified Data.Text as T
import Control.Applicative
import Data.Attoparsec.Text
import Modifications
import Common

data ArmorProf = ArmorProf { armorName :: T.Text
                           , armorType :: Maybe T.Text
                           , modification :: Maybe Modification } deriving Show

parseArmorType :: Parser T.Text
parseArmorType = string "TYPE:" >> parseWord

parseArmorProficency :: Parser (T.Text, Maybe Modification) -> Parser ArmorProf
parseArmorProficency p = do
  (name, modifier) <- p <* tabs
  at <- optionMaybe parseArmorType
  return ArmorProf { armorName = name
                   , armorType = at
                   , modification = modifier }

parseArmorProfLine :: Parser ArmorProf
parseArmorProfLine = parseArmorProficency parseStartMod <|>
                     parseArmorProficency parseStart
