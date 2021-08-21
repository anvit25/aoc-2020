module Main where

import Day1 (day1a, day1b)
import Day10 (day10a, day10b)
import Day2 (day2a, day2b)
import Day3 (day3a, day3b)
import Day4 (day4a, day4b)
import Day5 (day5a, day5b)
import Day6 (day6a, day6b)
import Day7 (day7a, day7b)
import Day8 (day8a, day8b)
import Day9 (day9a, day9b)

main :: IO ()
main = readFile "input/7.input" >>= (print . day7b)
