module Grid (
    Grid(..),
    Point(..),
    (@@),
    rowsToGrid,
    elemsToGrid,
    adjacents,
    allPositions,
    allPosVals
) where

data Grid = Grid { width :: Int, height :: Int, values :: [[Char]] }
data Point = Point { x :: Int, y :: Int }

(@@) :: Grid -> Point -> Char
(@@) grid (Point x y) = (values grid !! y) !! x

rowsToGrid :: [[Char]] -> Grid
rowsToGrid [] = Grid { width = 0, height = 0, values = [] }
rowsToGrid rows@(r:_) = Grid { width = length r, height = length rows, values = rows }

elemsToGrid :: Int -> Int -> [Char] -> Grid
elemsToGrid w h vals =
    let rows = [ take w (drop (i * w) vals) | i <- [0 .. h - 1] ]
    in Grid { width = w, height = h, values = rows }

adjacents :: Point -> Grid -> [Point]
adjacents (Point x y) grid =
    let deltas = [(-1,0),(1,0),(0,-1),(0,1),(-1,-1),(-1,1),(1,-1),(1,1)]
        valid (nx, ny) = nx >= 0 && ny >= 0 && nx < width grid && ny < height grid
    in [ Point nx ny | (dx, dy) <- deltas, let nx = x + dx, let ny = y + dy, valid (nx, ny) ]

allPositions :: Grid -> [Point]
allPositions input =
    [ Point x y | y <- [0 .. height input - 1], x <- [0 .. width input - 1] ]

allPosVals :: Grid -> [(Point, Char)]
allPosVals input =
    [(Point x y, input @@ Point x y) | y <- [0 .. height input - 1], x <- [0 .. width input - 1]]