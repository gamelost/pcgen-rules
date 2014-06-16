module Lst.Language where

import qualified Data.Text as T
import Control.Monad(liftM)
import Data.Attoparsec.Text
import Control.Applicative
import Common

-- we only define the most common language types here
data LanguageType = Read
                  | Spoken
                  | Written
                  | Other T.Text
                    deriving Show

data LanguageDefinition = LanguageDefinition { name :: T.Text
                                             , productIdentity :: Bool -- optional
                                             , languageType :: [LanguageType]
                                             , sourcePage :: T.Text -- optional
                                             } deriving Show

data LanguageLine = Source Headers
                  | Definition LanguageDefinition
                  | Comment T.Text deriving Show

type Languages = [LanguageLine]

parseLanguage :: Parser LanguageLine
parseLanguage = do
  -- not finished
  name <- takeWhile1 $ notInClass "\t"
  return $ Definition LanguageDefinition { name = name }

parseComment :: Parser LanguageLine
parseComment = liftM Comment commentedLine

parseLanguageLine :: Parser Languages
parseLanguageLine = many $ (parseComment <|>
                            --parseHeaders <|>
                            parseLanguage) <* many endOfLine
