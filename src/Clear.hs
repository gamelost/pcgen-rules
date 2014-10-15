module Clear ( ClearTag(..)
             , parseClear
             ) where

import Text.Parsec.Combinator (choice)
import Data.Maybe (fromJust)
import ClassyPrelude

import Common

data ClearTag = Clear String
              deriving (Show, Eq)

parseClear :: PParser ClearTag
parseClear = do
  -- intentionally very limited (for now).
  what <- choice $ tryPrefixes [ "RANGE"
                               , "TYPE"
                               , "PRE"
                               , "DESC"
                               , "CSKILL"
                               , "AUTO"
                               , "LANG"
                               , "SAB"]
  return $ Clear $ stripClear what where
    tryPrefixes = tryStrings . map (++ ":.CLEAR")
    stripClear = fromJust . stripSuffix ":.CLEAR"
