{-# LANGUAGE OverloadedStrings #-}

module Lst.Generic where

import ClassyPrelude

import Modifications
import Common

-- generic lst placeholder while we implement specific lst types
data LSTDefinition = Generic String
                     deriving Show

parseLSTTag :: PParser LSTDefinition
parseLSTTag = Generic <$> restOfTag

instance LSTObject LSTDefinition where
  parseSpecificTags = parseLSTTag
