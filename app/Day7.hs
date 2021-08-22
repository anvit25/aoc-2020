module Day7 (day7a, day7b) where

import Control.Applicative ((<|>))
import Data.Function (fix)
import Data.Function.Memoize (memoize)
import Data.Map.Strict (Map, empty, fromList, keys, mapWithKey, (!), (!?))
import Helper.Misc (countIf)
import Helper.Parse
import Text.Parsec (many, option, sepBy, space)

type Bag = String

type Bags = Map Bag (Map Bag Int)

day7a :: String -> Either PError Int
day7a xs = do
  bags <- parse parseFile xs
  return . countIf (containShinyGold' bags) $ keys bags

day7b :: String -> Either PError Int
day7b xs = countBag "shiny gold" <$> parse parseFile xs

countBag :: Bag -> Bags -> Int
countBag x bags = sum . mapWithKey (\k a -> a * (1 + countBag k bags)) $ bags ! x

containShinyGold :: Bags -> (Bag -> Bool) -> Bag -> Bool
containShinyGold bags f bag = case contains !? "shiny gold" of
  Nothing -> any f $ keys contains
  Just _ -> True
  where
    contains = bags ! bag

containShinyGold' :: Bags -> Bag -> Bool
containShinyGold' bags = fix (memoize . containShinyGold bags)

----- PARSING -------
parseFile :: Parser Bags
parseFile = fromList <$> multiple parseLine

parseLine :: Parser (Bag, Map Bag Int)
parseLine = do
  adj <- multiple letter <* space
  col <- multiple letter <* stringP " bags contain "
  ma <- (emptybag <|> nonemptybag) <* space
  return (unwords [adj, col], ma)

emptybag :: Parser (Map Bag Int)
emptybag = empty <$ stringP "no other bags."

nonemptybag :: Parser (Map Bag Int)
nonemptybag = fromList <$> sepBy containsP (stringP ", ") <* charP '.'

containsP :: Parser (Bag, Int)
containsP = do
  n <- integer <* space
  adj <- multiple letter <* space
  col <- multiple letter
  stringP " bag" *> many (charP 's')
  return (unwords [adj, col], n)
