module Lst.ShieldProf where

import Text.Parsec.Prim (parserZero)
import ClassyPrelude

import Modifications

-- no shield specific tags!
data ShieldProficency = Void deriving Show

instance LSTObject ShieldProficency where
  parseSpecificTags = parserZero
