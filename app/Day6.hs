module Day6 (day6a, day6b) where

import qualified Data.Set as Set
import Helper.Parse
import Text.Parsec (noneOf, sepBy)

day6a :: String -> Either PError Int
day6a xs = sum . map (length . makeset) <$> parse planeP xs

day6b :: String -> Either PError Int
day6b xs =
  sum . map groupfault
    <$> parse planeP xs
  where
    groupfault = length . foldr1 Set.intersection . map Set.fromList

singleP :: Parser String
singleP = multiple $ noneOf "\n"

groupP :: Parser [String]
groupP = multiple $ singleP <* charP '\n'

planeP :: Parser [[String]]
planeP = sepBy groupP (charP '\n') <* eof

makeset :: [String] -> Set.Set Char
makeset = Set.fromList . concat
