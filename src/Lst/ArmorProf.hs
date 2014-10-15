module Lst.ArmorProf where

import ClassyPrelude

import Modifications
import Common

data ArmorProficency = Type String
                       deriving Show

parseArmorType :: PParser ArmorProficency
parseArmorType = Type <$> (tag "TYPE" >> parseWord)

instance LSTObject ArmorProficency where
  parseSpecificTags = parseArmorType
