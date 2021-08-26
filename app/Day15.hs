module Day15 (day15a, day15b) where

import Data.IntMap ((!?))
import qualified Data.IntMap.Strict as IM
import Data.List.Split (splitOn)
import Helper.Misc (find')

type Book = IM.IntMap Int

day15a xs = (\(_, b, _) -> b) . find' (\(a, _, _) -> a == 2020) $ iterate next (startturn, last starting, startbook)
  where
    starting = map read . splitOn "," $ init xs
    startbook = IM.fromList $ zip starting [1 ..]
    startturn = length starting

day15b xs = (\(_, b, _) -> b) . find' (\(a, _, _) -> a == 30000000) $ iterate next (startturn, last starting, startbook)
  where
    starting = map read . splitOn "," $ init xs
    startbook = IM.fromList $ zip starting [1 ..]
    startturn = length starting

next ::
  (Int, Int, Book) -> -- (Turn number, Last spoken, book)
  (Int, Int, Book) -- (Turn number, Number spoken, Book)
next (n, s, book) = case prev of
  Just y -> (n + 1, n - y, IM.insert s n book)
  Nothing -> (n + 1, 0, IM.insert s n book)
  where
    prev = book !? s
