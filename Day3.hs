{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Day3 (solve) where

import Grid
import Utils
import Data.Char

solvePart1 :: [String] -> Int
solvePart1 input = solveK input 2

solvePart2 :: [String] -> Int
solvePart2 input = solveK input 12

solveK :: [String] -> Int -> Int
solveK input k =
    let banks = map (map digitToInt) input
    in sum $ map (maxJolts k) banks

dropEnd :: Int -> [a] -> [a]
dropEnd n xs = take (length xs - n) xs

maxJolts :: Int -> [Int] -> Int
maxJolts 0 _ = 0
maxJolts k nums =
    let max = maximum $ dropEnd (k - 1) nums
        rest = tail $ dropWhile (/= max) nums
    in max * 10 ^ (k - 1) + maxJolts (k - 1) rest

solve :: String -> String
solve input = 
    let linesOfFile = lines input
        res1 = solvePart1 linesOfFile
        res2 = solvePart2 linesOfFile
    in "Part 1: " ++ show res1 ++ "\nPart 2: " ++ show res2
