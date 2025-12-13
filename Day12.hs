{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}
{- HLINT ignore "Use map once" -} -- (it looks way more readable this way in my opinion)

module Day12 (solve) where

import Grid
import Utils
import Data.List.Split

solvePart1 :: String -> Int
solvePart1 input =
    let regions = lines $ last $ splitOn "\n\n" input
        stuff =
            map (\r -> ((head (fst r) `div` 3) * (last (fst r) `div` 3),
                                sum $ snd r)) .
            map (\r -> (map read $ splitOn "x" $ head r,
                        map read $ splitOn " " $ last r)) .
            map (splitOn ": ") $ regions
    in count (\r -> snd r <= fst r) stuff

solve :: String -> String
solve input =
    let res1 = solvePart1 input
    in "Part 1: " ++ show res1