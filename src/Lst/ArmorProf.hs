{-# LANGUAGE RecordWildCards #-}

module Lst.ArmorProf where

import qualified Data.Text as T
import Control.Applicative
import Data.Attoparsec.Text
import Modifications
import Common

data ArmorProficency = ArmorProficency { armorName :: T.Text
                                       , armorType :: Maybe T.Text } deriving Show

parseArmorType :: Parser T.Text
parseArmorType = tag "TYPE" >> parseWord

parseArmorProficency :: T.Text -> Parser ArmorProficency
parseArmorProficency armorName = do
  armorType <- optionMaybe parseArmorType
  return ArmorProficency {..}

instance LSTObject ArmorProficency where
  parseLine = parseArmorProficency
