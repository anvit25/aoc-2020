@echo off
mkdir input
set /p AOC_COOKIE=<.cookie
cd input
FOR /L %%A IN (1,1,25) DO (
    curl -o "%%A.input" -b %AOC_COOKIE% https://adventofcode.com/2020/day/%%A/input
)