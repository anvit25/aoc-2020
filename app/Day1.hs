module Day1
  ( day1a,
    day1b,
  )
where

import Data.List (tails)
import Data.Maybe (mapMaybe)
import Helper.Misc

n :: Int
n = 2020

day1a :: String -> Maybe Int
day1a = (product <$>) . finder n . parseFile

day1b :: String -> Int
day1b = firstJust' (f n) . tails . parseFile
  where
    f :: Int -> [Int] -> Maybe Int
    f m xs@(x : _) = (* x) . product <$> finder (m - x) xs
    f _ [] = Nothing

finder :: Int -> [Int] -> Maybe [Int]
finder _ [] = Nothing
finder m (x : xs) = if m - x `elem` xs then Just [x, m - x] else finder m xs
