module Main where

import Day1 (day1a, day1b)
import Day10
import Day14
import Day2 (day2a, day2b)
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
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

    it "Day 14" $ do
      (day14a <$> readFile "input/14.input") `shouldReturn` 7477696999511
      (day14b <$> readFile "input/14.input") `shouldReturn` 3687727854171
