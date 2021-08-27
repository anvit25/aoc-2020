module Day15 where

import Control.Monad (zipWithM_)
import Control.Monad.ST (ST, runST)
import Data.List.Split (splitOn)
import qualified Data.Vector.Unboxed.Mutable as MV

day15a :: String -> Int
day15a xs = snd $
  runST $ do
    mybook <- MV.new 3000 :: ST s (MV.MVector s Int)
    let starting = map read . splitOn "," $ init xs
    let turn = length starting
    zipWithM_ (MV.write mybook) starting [1 ..]

    next (2020 - turn) mybook (turn, last starting)

day15b :: String -> Int
day15b xs = snd $
  runST $ do
    mybook <- MV.new 30000000 :: ST s (MV.MVector s Int)
    let starting = map read . splitOn "," $ init xs
    let turn = length starting
    zipWithM_ (MV.write mybook) starting [1 ..]

    next (30000000 - turn) mybook (turn, last starting)

next ::
  Int ->
  MV.MVector s Int ->
  (Int, Int) -> -- (Turn number, Last spoken)
  ST s (Int, Int) -- (Turn number, Number spoken)
next 0 _ r = return r
next i book (n, s) = do
  prev <- MV.read book s
  let y = if prev == 0 then 0 else n - prev
  MV.write book s n
  next (i - 1) book (n + 1, y)
