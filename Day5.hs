{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-} -- haskell sucks why do i have to put this in the source just to get my LOCAL HLS to not complain

module Day5 (solve) where

import Grid
import Utils
import Data.List
import Data.List.Split

extendRange :: Int -> [(Int, Int)] -> (Int, [(Int, Int)])
extendRange end [] = (end, [])
extendRange end (x:xs)
    | nxtStart > end = (end, x:xs)
    | otherwise      = extendRange (max end nxtEnd) xs
    where (nxtStart, nxtEnd) = x

simplifyRanges :: [(Int, Int)] -> [(Int, Int)]
simplifyRanges [] = []
simplifyRanges (x:xs) =
    let (start, end) = x
        (merged, rest) = extendRange end xs
    in (start, merged) : simplifyRanges rest

parseRanges :: [String] -> [(Int, Int)]
parseRanges input =
    let parseRange :: String -> (Int, Int)
        parseRange range =
            let [start, end] = map read $ splitOn "-" range
            in (start, end)
    in map parseRange input

solvePart1 :: String -> Int
solvePart1 input =
    let [freshIds, availableIds] = map lines $ splitOn "\n\n" input
        ranges = parseRanges freshIds
    in count (\num -> any (\range -> fst range <= num && num <= snd range) ranges) $ map read availableIds

solvePart2 :: String -> Int
solvePart2 input =
    let ranges = parseRanges $ lines $ head $ splitOn "\n\n" input
        reduced = simplifyRanges $ sortOn fst ranges
    in sumBy (\(start, end) -> end - start + 1) reduced

solve :: String -> String
solve input = 
    let res1 = solvePart1 input
        res2 = solvePart2 input
    in "Part 1: " ++ show res1 ++ "\nPart 2: " ++ show res2