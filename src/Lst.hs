module Lst where

import Prelude hiding (takeWhile)
import Control.Monad(liftM)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Common

-- custom lst types
import Lst.Skill(SkillMod, parseSkillLine)
import Lst.WeaponProf(WeaponMod, parseWeaponLine)
import Lst.ArmorProf(ArmorMod, parseArmorLine)
import Lst.ShieldProf(ShieldMod, parseShieldLine)
import Lst.Language(LanguageMod, parseLanguageLine)
import Lst.Generic(LSTTag, parseGenericLine)

-- structure of a lst file
data LST a = Source [Header]
           | Definition a
           | Comment T.Text deriving Show

-- source headers: these are found in nearly every lst file type.
data Header = SourceLong T.Text
            | SourceShort T.Text
            | SourceWeb T.Text
            | SourceDate T.Text deriving Show

parseSource :: String -> Parser T.Text
parseSource source = tag source >> parseString

parseSourceUrl :: String -> Parser T.Text
parseSourceUrl source = tag source >> parseUrl

parseSourceWeb :: Parser Header
parseSourceWeb = liftM SourceWeb $ parseSourceUrl "SOURCEWEB"

parseSourceLong :: Parser Header
parseSourceLong = liftM SourceLong $ parseSource "SOURCELONG"

parseSourceShort :: Parser Header
parseSourceShort = liftM SourceShort $ parseSource "SOURCESHORT"

parseSourceDate :: Parser Header
parseSourceDate = liftM SourceDate $ parseSource "SOURCEDATE"

parseHeaders :: Parser [Header]
parseHeaders = many1 (parseSourceLong <|>
                      parseSourceShort <|>
                      parseSourceWeb <|>
                      parseSourceDate) <* tabs

parseLSTLine :: Parser a -> Parser [LST a]
parseLSTLine parseDefinition = do
  -- next line added solely because of saspg_languages.lst, ugh
  _ <- many endOfLine
  many1 $ (liftM Source parseHeaders <|>
           liftM Comment parseCommentLine <|>
           liftM Definition parseDefinition) <* many endOfLine

parseLST :: FilePath -> Parser [LST a] -> IO [LST a]
parseLST lstName lstParser = do
  contents <- readContents lstName
  return $ parseResult lstName $ parse lstParser contents

parseLanguageLST :: FilePath -> IO [LST LanguageMod]
parseLanguageLST lstName = parseLST lstName $ parseLSTLine parseLanguageLine

parseGenericLST :: FilePath -> IO [LST LSTTag]
parseGenericLST lstName = parseLST lstName $ parseLSTLine parseGenericLine

parseArmorLST :: FilePath -> IO [LST ArmorMod]
parseArmorLST lstName = parseLST lstName $ parseLSTLine parseArmorLine

parseShieldLST :: FilePath -> IO [LST ShieldMod]
parseShieldLST lstName = parseLST lstName $ parseLSTLine parseShieldLine

parseWeaponLST :: FilePath -> IO [LST WeaponMod]
parseWeaponLST lstName = parseLST lstName $ parseLSTLine parseWeaponLine

parseSkillLST  :: FilePath -> IO [LST SkillMod]
parseSkillLST lstName = parseLST lstName $ parseLSTLine parseSkillLine
