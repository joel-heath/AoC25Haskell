import Data.Char (digitToInt)
import Data.List (mapAccumL)

data Grid = Grid { width :: Int, height :: Int, values :: [[Char]] }
data Point = Point { x :: Int, y :: Int }

(@@) :: Grid -> Point -> Char
(@@) grid (Point x y) = (values grid !! y) !! x

rowsToGrid :: [[Char]] -> Grid
rowsToGrid vals = Grid { width = length (head vals), height = length vals, values = vals }

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


-- HELPERS BELOW --

count :: (a -> Bool) -> [a] -> Int
count predicate xs = length $ filter predicate xs

-- `tail` because we want to skip testing the predicate on the seed
foldWhile :: (acc -> x -> acc) -> (acc -> Bool) -> acc -> [x] -> acc
foldWhile step ok acc xs = last (takeWhile ok (tail (scanl step acc xs)))

-- SOLUTION BELOW --

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

main :: IO ()
main = do
    input <- readFile "Inputs/Day4.txt"
    let linesOfFile = lines input
        res1 = solvePart1 linesOfFile
        res2 = solvePart2 linesOfFile
    putStrLn $ "Part 1: " ++ show res1
    putStrLn $ "Part 2: " ++ show res2