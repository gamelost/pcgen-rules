{-# LANGUAGE OverloadedStrings #-}

module Lst.ArmorProf where

import qualified Data.Text as T
import Control.Monad(liftM)
import Control.Applicative
import Data.Attoparsec.Text
import Modifications
import Common

data ArmorProf = ArmorProf { armorName :: T.Text
                           , armorType :: Maybe T.Text
                           , modification :: Maybe Modification } deriving Show

parseArmorProfMod :: Parser (T.Text, Maybe Modification)
parseArmorProfMod = liftM (\x -> (x, Just Add)) parseModification

parseArmorProf :: Parser (T.Text, Maybe Modification)
parseArmorProf = liftM (\x -> (x, Nothing)) parseString

parseArmorType :: Parser T.Text
parseArmorType = do
  _ <- string "TYPE:"
  parseWord

parseArmorProficency :: Parser (T.Text, Maybe Modification) -> Parser ArmorProf
parseArmorProficency p = do
  (name, modifier) <- p <* tabs
  at <- option Nothing $ liftM Just parseArmorType
  return ArmorProf { armorName = name
                   , armorType = at
                   , modification = modifier }

parseArmorProfLine :: Parser ArmorProf
parseArmorProfLine = parseArmorProficency parseArmorProfMod <|>
                     parseArmorProficency parseArmorProf
