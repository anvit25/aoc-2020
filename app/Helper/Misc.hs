module Helper.Misc where

import Control.Applicative (Alternative ((<|>)))
import Data.List (find)
import Data.Maybe (fromJust)

instance (Num a, Num b) => Num (a, b) where
  (a, b) + (c, d) = (a + c, b + d)
  (a, b) * (c, d) = (a * c, b * d)
  abs (c, d) = (abs c, abs d)
  signum (c, d) = (signum c, signum d)
  fromInteger a = (fromInteger a, fromInteger a) -- allows (*) to be used for both scalar and vector mult
  negate (c, d) = (- c, - d)

countIf :: (a -> Bool) -> [a] -> Int
countIf x = length . filter x

-- Safe Lookup
(!!?) :: [a] -> Int -> Maybe a
xs !!? n = if length xs > n then Just $ xs !! n else Nothing

-- Combine Boolean functions via AND
(<&&>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
x <&&> y = (&&) <$> x <*> y

-- Combine Boolean functions via OR
(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
x <||> y = (||) <$> x <*> y

isBetween :: (Ord a) => a -> a -> a -> Bool
isBetween x y z = (x <= z) && (z <= y)

-- Least Significant Digit is first
bintodec :: String -> Int
bintodec [] = 0
bintodec (x : xs) = read [x] + 2 * bintodec xs

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = foldr ((<|>) . f) Nothing

firstJust' :: (a -> Maybe b) -> [a] -> b
firstJust' f = fromJust . foldr ((<|>) . f) Nothing

find' :: (a -> Bool) -> [a] -> a
find' = (fromJust .) . find

parseFile :: (Read a) => String -> [a]
parseFile = map read . lines

(<$*>) :: (a -> c, b -> d) -> (a, b) -> (c, d)
(f, g) <$*> (a, b) = (f a, g b)
