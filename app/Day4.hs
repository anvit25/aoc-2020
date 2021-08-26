module Day4 (day4a, day4b) where

import Data.Char (isDigit, isHexDigit)
import Data.Either (fromRight)
import Helper.Misc (countIf, isBetween, (<&&>))
import Helper.Parse
import Text.Parsec (sepBy)
import Text.Parsec.Char (noneOf)

data Field
  = Ecl
  | Pid
  | Eyr
  | Hcl
  | Byr
  | Iyr
  | Cid
  | Hgt
  deriving (Show, Eq, Ord, Enum, Bounded)

day4a :: String -> Int
day4a xs = fromRight 0 $ do
  book <- parse bookP xs
  return . countIf allFields $ book

day4b :: String -> Int
day4b xs = fromRight 0 $ do
  book <- parse bookP xs
  return
    . countIf (all fieldValid <&&> allFields)
    $ book

fieldP :: Parser (Field, String)
fieldP = do
  field <- enumP <* charP ':'
  str <- multiple (noneOf " \n") <* anycharP " \n"
  return (field, str)

passportP :: Parser [(Field, String)]
passportP = multiple fieldP

bookP :: Parser [[(Field, String)]]
bookP = sepBy passportP $ charP '\n'

allFields :: [(Field, String)] -> Bool
allFields = (== 7) . length . filter ((/= Cid) . fst)

fieldValid :: (Field, String) -> Bool
fieldValid (Byr, xs) = isBetween 1920 2002 $ read xs
fieldValid (Iyr, xs) = isBetween 2010 2020 $ read xs
fieldValid (Eyr, xs) = isBetween 2020 2030 $ read xs
fieldValid (Hgt, xs) = case reverse xs of
  ('m' : 'c' : ys) -> isBetween 150 193 $ read (reverse ys)
  ('n' : 'i' : ys) -> isBetween 59 76 $ read (reverse ys)
  _ -> False
fieldValid (Hcl, '#' : xs) = (length xs == 6) && all isHexDigit xs
fieldValid (Hcl, _) = False
fieldValid (Ecl, xs) = xs `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
fieldValid (Pid, xs) = (length xs == 9) && all isDigit xs
fieldValid (Cid, _) = True
