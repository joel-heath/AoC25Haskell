{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Day7 (solve) where

import Grid
import Utils

solvePart1 :: [String] -> Int
solvePart1 input =
    let grid = rowsToGrid input
        start = head $ filter ((=='S') . snd) $ allPosVals grid
        (count, _) = foldl step (0, [getX $ fst start]) [1..(height grid - 2)]
        step (splits1, beams1) y =
            foldl step' (splits1, beams1) beams1
            where
                step' (splits, beams) x =
                    let pos = Point x y
                        val = grid @@ pos
                        children = [Point (x - 1) (y + 1), Point (x + 1) (y + 1)]
                        newBeams = filter (\p -> contains grid p && (getX p `notElem` beams)) children
                        nextBeams = filter (/= x) beams ++ map getX newBeams
                    in if val == '^' then
                        (splits + 1, nextBeams)
                    else
                        (splits, beams)
    in count

solvePart2 :: [String] -> Int
solvePart2 input =
    let grid = rowsToGrid input
        start = head $ filter ((=='S') . snd) $ allPosVals grid
        finalBeams = foldl step [(getX $ fst start, 1)] [1..(height grid - 2)]
        --         x  paths       y       
        step :: [(Int, Int)] -> Int -> [(Int, Int)]
        step beams1 y =
            foldl step' beams1 beams1
            where
                step' beams (x, paths) =
                    let pos = Point x y
                        val = grid @@ pos
                        children = [Point (x - 1) (y + 1), Point (x + 1) (y + 1)]
                        childBeams = map getX . filter (contains grid) $ children
                        beamsNotX = filter (/= (x, paths)) beams
                        nextBeams = foldr step'' beamsNotX childBeams
                        step'' cb bs =
                            case lookup cb bs of
                                Just pth -> (cb, pth + paths) : filter ((/= cb) . fst) bs
                                Nothing  -> (cb, paths) : bs
                    in if val == '^' then nextBeams else beams
    in sum $ map snd finalBeams

solve :: String -> String
solve input =
    let linesOfFile = lines input
        res1 = solvePart1 linesOfFile
        res2 = solvePart2 linesOfFile
    in "Part 1: " ++ show res1 ++ "\nPart 2: " ++ show res2