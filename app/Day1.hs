module Day1
  ( day1a,
    day1b,
  )
where

import Data.List (tails)
import Data.Maybe (mapMaybe)

n :: Int
n = 2020

day1a :: String -> Maybe Int
day1a = (product <$>) . finder n . map read . lines

day1b :: String -> Int
day1b = head . mapMaybe (f n) . tails . map read . lines
  where
    f :: Int -> [Int] -> Maybe Int
    f m xs@(x : _) = (* x) . product <$> finder (m - x) xs
    f _ [] = Nothing

-- day1b :: String -> Int
-- day1b = f . map read . lines
--  where
--   f (x : xs) = case finder (n - x) xs of
--     Just a  -> a * x
--     Nothing -> f xs
--   f [] = undefined

finder :: Int -> [Int] -> Maybe [Int]
finder _ [] = Nothing
finder m (x : xs) = if m - x `elem` xs then Just [x, m - x] else finder m xs
