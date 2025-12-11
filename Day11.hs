{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Day11 (solve) where

import Utils
import StringGraph
import Data.List.Split (splitOn)

countAllPaths :: StringGraph -> Int -> Int -> Int
countAllPaths graph start end = fst (go start [])
  where
    go :: Int -> [(Int, Int)] -> (Int, [(Int, Int)])
    go current memo
      | current == end = (1, memo)
      | Just v <- lookup current memo = (v, memo)
      | otherwise =
          let neighs = neighbours graph current
              (total, memo') =
                foldl' (\(acc, m) n ->
                        let (r, m') = go n m
                        in (acc + r, m'))
                    (0, memo) neighs
              memo'' = (current, total) : memo'
          in (total, memo'')

parse :: [String] -> StringGraph
parse input =
    let edges :: [(String, String)]
        edges = concatMap((
            --          \neigh -> (fst parts, neigh)
            (\parts -> map (fst parts,) (snd parts)) .
            (\parts -> (head parts, splitOn " " (parts !! 1)))) .
            splitOn ": ") input
    in buildStringGraph edges

solvePart1 :: [String] -> Int
solvePart1 input =
    let graph = parse input
        start = lookupIndex graph "you"
        end = lookupIndex graph "out"
    in countAllPaths graph start end

solvePart2 :: [String] -> Int
solvePart2 input =
    let graph = parse input
        svr = lookupIndex graph "svr"
        fft = lookupIndex graph "fft"
        dac = lookupIndex graph "dac"
        out = lookupIndex graph "out"
    in countAllPaths graph svr fft *
       countAllPaths graph fft dac *
       countAllPaths graph dac out
       +
       countAllPaths graph svr dac *
       countAllPaths graph dac fft *
       countAllPaths graph fft out

solve :: String -> String
solve input =
    let linesOfFile = lines input
        res1 = solvePart1 linesOfFile
        res2 = solvePart2 linesOfFile
    in "Part 1: " ++ show res1 ++ "\nPart 2: " ++ show res2