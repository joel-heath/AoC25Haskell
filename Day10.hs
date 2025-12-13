module Day10 (solve) where

import Utils

data Machine = Machine { lightDiagram :: [Bool],
                         buttons :: [[Int]],
                         requirements :: [Int],
                         lights :: [Bool] } deriving (Show, Eq)

solvePart1 :: [String] -> Int
solvePart1 input =
    let 
    in undefined

solvePart2 :: [String] -> Int
solvePart2 input =
    let 
    in undefined

solve :: String -> String
solve input = 
    let linesOfFile = lines input
        res1 = solvePart1 linesOfFile
        res2 = solvePart2 linesOfFile
    -- in "Part 1: " ++ show res1 ++ "\nPart 2: " ++ show res2
    in "Unsolved, it was hard enough in C#.\n(https://github.com/joel-heath/AdventOfCode2025/blob/master/AdventOfCode2025/Day10.cs)"