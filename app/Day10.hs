module Day10 (day10a, day10b) where

import Control.Applicative (Applicative (liftA2))
import Data.Function (fix)
import Data.Function.Memoize (memoize)
import Data.List (sort, tails)
import Helper.Misc

day10a :: String -> Int
day10a xs = liftA2 (*) nThrees nOnes differences
  where
    adapters = (0 :) . sort $ parseFile xs :: [Int]
    differences = zipWith (-) (tail adapters) adapters
    nOnes = countIf (== 1)
    nThrees = (+ 1) . countIf (== 3)

day10b :: String -> Int
day10b = count' . reverse . (0 :) . sort . parseFile

count :: ([Int] -> Int) -> [Int] -> Int
count _ [] = 0
count _ [_] = 1
count _ [_, _] = 1
count f (x : xs) =
  sum . map f
    . takeWhile ((< 4) . (x -) . head)
    . takeWhile (not . null)
    $ tails xs

count' :: [Int] -> Int
count' = fix (memoize . count)
