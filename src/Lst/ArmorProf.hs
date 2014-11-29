module Lst.ArmorProf where

import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepBy)
import ClassyPrelude

import Modifications
import Common

data ArmorProficency = Type [String]
                       deriving Show

parseArmorType :: PParser ArmorProficency
parseArmorType = Type <$> (tag "TYPE" >> parseWord `sepBy` char '.')

instance LSTObject ArmorProficency where
  parseSpecificTags = parseArmorType
