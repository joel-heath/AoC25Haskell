module Day4 (solve) where

import Data.Char (digitToInt)
import Data.List (mapAccumL)
import Grid
import Utils

solvePart1 :: [String] -> Int
solvePart1 input =
    let grid = rowsToGrid input
        posVals = allPosVals grid
        rolls = filter ((=='@') . snd) posVals
        neighbours = map (\(p, _) -> adjacents p grid) rolls
    in count (\ps -> count (\p -> grid @@ p == '@') ps < 4) neighbours

solvePart2 :: [String] -> Int
solvePart2 input =
    let ogGrid = rowsToGrid input
        forever = [0..]
        (_, _, total) = 
            foldWhile step (\(_, changed, _) -> changed > 0) (ogGrid, 0, 0) forever
        step (grid, changed, total) _ =
            let (rollsMoved :: Int, newGrid) = 
                    mapAccumL
                        (\changed (p, v) ->
                            let moveable = v == '@' && count (\adj -> grid @@ adj == '@') (adjacents p grid) < 4
                            in if moveable then (changed + 1, '.') else (changed, v)
                        ) 0 $ allPosVals grid
            in (elemsToGrid (width grid) (height grid) newGrid, rollsMoved, total + rollsMoved)
    in total

solve :: String -> String
solve input = 
    let linesOfFile = lines input
        res1 = solvePart1 linesOfFile
        res2 = solvePart2 linesOfFile
    in "Part 1: " ++ show res1 ++ "\nPart 2: " ++ show res2
