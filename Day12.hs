{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}
{- HLINT ignore "Use map once" -} -- (it looks way more readable this way in my opinion)

module Day12 (solve) where

import Grid
import Utils
import Data.List.Split
import Data.Function ((&))

solvePart1 :: String -> Int
solvePart1 input = 
    input
      & splitOn "\n\n"
      & last
      & lines
      & map (splitOn ": ")
      & map (\r -> (map read $ splitOn "x" $ head r,
                    map read $ splitOn " " $ last r))
      & map (\(d, p) -> ((head d `div` 3) * (last d `div` 3),
                         sum p))
      & count (\(space, presents) -> presents <= space)

solve :: String -> String
solve input =
    let res1 = solvePart1 input
    in "Part 1: " ++ show res1