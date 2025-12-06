module Day1 (solve) where

import Grid
import Utils

solvePart1 :: [String] -> Int
solvePart1 = countZeroes . map parse

solvePart2 :: [String] -> Int
solvePart2 = countClicks . map parse

parse :: String -> Int
parse ('L':tail) = -read tail
parse ('R':tail) = read tail

countZeroes :: [Int] -> Int
countZeroes rotations = snd $ foldl step (50, 0) rotations
    where
        step :: (Int, Int) -> Int -> (Int, Int)
        step (dial, count) rotation =
            let newDial = (dial + rotation) `mod` 100
                newCount = if newDial == 0 then count + 1 else count
            in (newDial, newCount)

countClicks :: [Int] -> Int
countClicks rotations = snd $ foldl step (50, 0) rotations
    where
        step :: (Int, Int) -> Int -> (Int, Int)
        step (dial, count) rotation =
            let newDial = (dial + rotation)
                increment
                    | rotation > 0 && newDial >= 100 = newDial `div` 100
                    | rotation < 0 && newDial <= 0 = (if dial == 0 then 0 else 1) + ((-newDial) `div` 100)
                    | otherwise = 0

            in (newDial `mod` 100, count + increment)

solve :: String -> String
solve input = 
    let linesOfFile = lines input
        res1 = solvePart1 linesOfFile
        res2 = solvePart2 linesOfFile
    in "Part 1: " ++ show res1 ++ "\nPart 2: " ++ show res2
