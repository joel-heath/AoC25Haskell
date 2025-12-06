{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Day6 (solve) where

import Grid
import Utils
import Data.List
import Data.List.Split
import Data.Char

tokens :: String -> [String]
tokens line = filter (not . null) (splitWhen isSpace line)

evaluate :: Char -> [Int] -> Int
evaluate '+' counts = sum counts
evaluate '*' counts = product counts

solvePart1 :: [String] -> Int
solvePart1 input =
    let lines = transpose $ map tokens input
        problems = map (\line -> (head $ last line, map read $ init line)) lines :: [(Char, [Int])]
    in sum $ map (uncurry evaluate) problems

solvePart2 :: [String] -> Int
solvePart2 input =
    let rotated = map trim $ rotateLeft input
        parts = splitWhen (== "") rotated
        problems = map (\lines -> (last $ last lines, map (read . trimBy "+*") lines)) parts :: [(Char, [Int])]
    in sum $ map (uncurry evaluate) problems

solve :: String -> String
solve input =
    let linesOfFile = lines input
        res1 = solvePart1 linesOfFile
        res2 = solvePart2 linesOfFile
    in "Part 1: " ++ show res1 ++ "\nPart 2: " ++ show res2