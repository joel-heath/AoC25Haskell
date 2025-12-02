import Data.List.Split
import Data.List (nub)

solvePart1 :: String -> Int
solvePart1 = sum . filter halvesSame . parse

solvePart2 :: String -> Int
solvePart2 = sum . filter repeatedSubstring . parse

parse :: String -> [Int]
parse input =
    let ranges = splitOn "," input
        parseRange :: String -> [Int]
        parseRange range =
            let [start, end] = map read $ splitOn "-" range
            in [start..end]
    in concatMap parseRange ranges

halvesSame :: Int -> Bool
halvesSame n =
    let str = show n
        len = length str
        (firstHalf, secondHalf) = splitAt (len `div` 2) str
    in firstHalf == secondHalf

repeatedSubstring :: Int -> Bool
repeatedSubstring n =
    let str = show n
        len = length str
        allRepeats size =
            let chunks = chunksOf size str
            in length (nub chunks) == 1
    in any allRepeats [1..(len `div` 2)]

main :: IO ()
main = do
    input <- readFile "Inputs/Day2.txt"
    let res1 = solvePart1 input
        res2 = solvePart2 input
    putStrLn $ "Part 1: " ++ show res1
    putStrLn $ "Part 2: " ++ show res2