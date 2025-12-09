{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Day8 (solve) where

import Utils
import Graph
import Data.List.Split
import Data.List (sortOn)
import Data.Ord  (Down(..))
import Data.Maybe ( fromMaybe )

useTestInput = False

orderConnections :: [Coord] -> [(Coord, Coord)]
orderConnections coords = sortOn (\(a, b) ->
        let dx = x a - x b
            dy = y a - y b
            dz = z a - z b
        in (dx ^ 2 + dy ^ 2 + dz ^ 2)
    ) $ pairs coords

exploreComponent :: Graph -> Coord -> [Coord] -> (Int, [Coord])
exploreComponent graph start visited =
    let go [] vis = (0, vis)
        go (n:ns) vis
            | n `elem` vis = go ns vis
            | otherwise =
                let adj = fromMaybe [] (lookup n (edges graph))
                    (sizeRest, visRest) = go (adj ++ ns) (n : vis)
                in (1 + sizeRest, visRest)
    in go [start] visited

isConnected :: Graph -> [Coord] -> Bool
isConnected graph coords =
    length (nodes graph) == length coords
    && (let (queue, visited) = go [head coords] []
            go [] vis = ([], vis)
            go (n:ns) vis
                | n `elem` vis = go ns vis
                | otherwise =
                    let adj = fromMaybe [] (lookup n (edges graph))
                    in go (adj ++ ns) (n : vis)
        in length visited == length coords)

solvePart1 :: [String] -> Int
solvePart1 input =
    let connectionsToMake = if useTestInput then 10 else 1000
        boxes = map (toCoord . map read . splitOn ",") input
        conns = take connectionsToMake $ orderConnections boxes
        graph = buildGraph conns
        (circuitSizes, _) =
            foldl (\(sizes, vis) node ->
                if node `elem` vis then (sizes, vis)
                else
                    let (compSize, newVis) = exploreComponent graph node vis
                    in (compSize : sizes, newVis)
            ) ([], []) (nodes graph)
    in product $ take 3 $ sortOn Down circuitSizes

solvePart2 :: [String] -> Int
solvePart2 input =
    let boxes = map (toCoord . map read . splitOn ",") input
        conns = orderConnections boxes
        graph = buildGraph conns
        (Just (a, b), _, _) =
            foldl (\(lastConn, ret, g) (a, b) ->
                if ret || isConnected g boxes then (lastConn, True, g)
                else (Just (a, b), False, connectUndirected g a b)
            ) (Nothing, False, Graph { nodes = [], edges = [] }) conns
    in x a * x b

solve :: String -> String
solve input =
    let linesOfFile = lines input
        res1 = solvePart1 linesOfFile
        res2 = solvePart2 linesOfFile
    in "Part 1: " ++ show res1 ++ "\nPart 2: " ++ show res2