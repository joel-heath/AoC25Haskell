module Day9 (solve) where

import Grid
import Utils
import Data.List.Split
import Polygons

solvePart1 :: [String] -> Int
solvePart1 input =
    let redTiles = [ Point x y | [x,y] <- map (map read . splitOn ",") input]
    in maximum [ area (rectangle p1 p2) | (p1, p2) <- pairs redTiles ]

solvePart2 :: [String] -> Int
solvePart2 input =
    let redTiles = [ Point x y | [x,y] <- map (map read . splitOn ",") input]
        rectangles = [ rectangle p1 p2 | (p1, p2) <- pairs redTiles ]
        es = edges redTiles
        isContained rectangle =
            not $ any (lineIntersectsRectangle rectangle) es
    in maximum $ map area $ filter isContained rectangles

solve :: String -> String
solve input =
    let linesOfFile = lines input
        res1 = solvePart1 linesOfFile
        res2 = solvePart2 linesOfFile
    in "Part 1: " ++ show res1 ++ "\nPart 2: " ++ show res2