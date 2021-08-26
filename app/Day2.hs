module Day2
  ( day2a,
    day2b,
  )
where

import Data.Either (fromRight)
import Data.Function (on)
import Helper.Misc (countIf)
import Helper.Parse

type Rule = (Int, Int, Char, String)

parsefile :: String -> Either PError [Rule]
parsefile = mapM (parse p) . lines

day2a :: String -> Int
day2a = fromRight 0 . (countIf isValid <$>) . parsefile

day2b :: String -> Int
day2b = fromRight 0 . (countIf isValid2 <$>) . parsefile

p :: Parser Rule
p = do
  l <- integer <* charP '-'
  h <- integer <* charP ' '
  c <- letter <* stringP ": "
  str <- multiple letter
  return (l, h, c, str)

isValid :: Rule -> Bool
isValid (l, h, c, str) = all ($ countIf (== c) str) [(l <=), (<= h)]

isValid2 :: Rule -> Bool
isValid2 (l, h, c, str) = on (/=) f (l - 1) (h - 1)
  where
    f = (== c) . (str !!)
