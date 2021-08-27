module Main where

import Day1 (day1a, day1b)
import Day10 (day10a, day10b)
import Day11 (day11a, day11b)
import Day12 (day12a, day12b)
import Day13 (day13a, day13b)
import Day14 (day14a, day14b)
import Day15 (day15a, day15b)
import Day2 (day2a, day2b)
import Day3 (day3a, day3b)
import Day4 (day4a, day4b)
import Day5 (day5a, day5b)
import Day6 (day6a, day6b)
import Day7 (day7a, day7b)
import Day8 (day8a, day8b)
import Day9 (day9a, day9b)
import Test.Hspec (describe, hspec, it, shouldReturn)

main :: IO ()
main = hspec $ do
  describe "Day 1-5" $ do
    it "Day 1" $ do
      (day1a <$> readFile "input/1.input") `shouldReturn` 800139
      (day1b <$> readFile "input/1.input") `shouldReturn` 59885340

    it "Day 2" $ do
      (day2a <$> readFile "input/2.input") `shouldReturn` 607
      (day2b <$> readFile "input/2.input") `shouldReturn` 321

    it "Day 3" $ do
      (day3a <$> readFile "input/3.input") `shouldReturn` 200
      (day3b <$> readFile "input/3.input") `shouldReturn` 3737923200

    it "Day 4" $ do
      (day4a <$> readFile "input/4.input") `shouldReturn` 254
      (day4b <$> readFile "input/4.input") `shouldReturn` 184

    it "Day 5" $ do
      (day5a <$> readFile "input/5.input") `shouldReturn` 883
      (day5b <$> readFile "input/5.input") `shouldReturn` 532

  describe "Day 6-10" $ do
    it "Day 6" $ do
      (day6a <$> readFile "input/6.input") `shouldReturn` 6549
      (day6b <$> readFile "input/6.input") `shouldReturn` 3466

    it "Day 7" $ do
      (day7a <$> readFile "input/7.input") `shouldReturn` 172
      (day7b <$> readFile "input/7.input") `shouldReturn` 39645

    it "Day 8" $ do
      (day8a <$> readFile "input/8.input") `shouldReturn` 1610
      (day8b <$> readFile "input/8.input") `shouldReturn` 1703

    it "Day 9" $ do
      (day9a <$> readFile "input/9.input") `shouldReturn` 1504371145
      (day9b <$> readFile "input/9.input") `shouldReturn` 183278487

    it "Day 10" $ do
      (day10a <$> readFile "input/10.input") `shouldReturn` 2432
      (day10b <$> readFile "input/10.input") `shouldReturn` 453551299002368

  describe "Day 11-15" $ do
    it "Day 11" $ do
      (day11a <$> readFile "input/11.input") `shouldReturn` 2427
      (day11b <$> readFile "input/11.input") `shouldReturn` 2199

    it "Day 12" $ do
      (day12a <$> readFile "input/12.input") `shouldReturn` 1032
      (day12b <$> readFile "input/12.input") `shouldReturn` 156735

    it "Day 13" $ do
      (day13a <$> readFile "input/13.input") `shouldReturn` 115
      (day13b <$> readFile "input/13.input") `shouldReturn` 756261495958122

    it "Day 14" $ do
      (day14a <$> readFile "input/14.input") `shouldReturn` 7477696999511
      (day14b <$> readFile "input/14.input") `shouldReturn` 3687727854171

    it "Day 15" $ do
      (day15a <$> readFile "input/15.input") `shouldReturn` 1009
      (day15b <$> readFile "input/15.input") `shouldReturn` 62714
