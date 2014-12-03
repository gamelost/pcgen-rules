module Clear ( ClearTag(..)
             , parseClear
             ) where

import Text.Parsec.Combinator (choice)
import Data.Maybe (fromJust)
import ClassyPrelude

import Common

data ClearTag = Clear String
              | ClearAll String
              deriving (Show, Eq)

parseClear :: PParser ClearTag
parseClear = parseClearOnly <|> parseClearAll

parseClearOnly :: PParser ClearTag
parseClearOnly = do
  -- intentionally very limited (for now).
  what <- choice $ tryPrefixes [ "RANGE"
                               , "TYPE"
                               , "PRE"
                               , "DESC"
                               , "CSKILL"
                               , "AUTO"
                               , "LANG"
                               , "TARGETAREA"
                               , "SAB"]
  return $ Clear $ stripClear what where
    tryPrefixes = tryStrings . map (++ ":.CLEAR")
    stripClear = fromJust . stripSuffix ":.CLEAR"

parseClearAll :: PParser ClearTag
parseClearAll = do
  -- intentionally very limited (for now).
  what <- choice $ tryPrefixes [ "CLASSES"
                               , "DESC" ]
  return $ Clear $ stripClear what where
    tryPrefixes = tryStrings . map (++ ":.CLEARALL")
    stripClear = fromJust . stripSuffix ":.CLEARALL"
