import Data.Char (digitToInt)

solvePart1 :: [String] -> Int
solvePart1 input = solve input 2

solvePart2 :: [String] -> Int
solvePart2 input = solve input 12

solve :: [String] -> Int -> Int
solve input k =
    let banks = map (map digitToInt) input
    in sum $ map (maxJolts k) banks

dropEnd :: Int -> [a] -> [a]
dropEnd n xs = take (length xs - n) xs

maxJolts :: Int -> [Int] -> Int
maxJolts 0 _ = 0
maxJolts k nums =
    let max = maximum $ dropEnd (k - 1) nums
        rest = tail $ dropWhile (/= max) nums
    in max * 10 ^ (k - 1) + maxJolts (k - 1) rest

main :: IO ()
main = do
    input <- readFile "Inputs/Day3.txt"
    let linesOfFile = lines input
        res1 = solvePart1 linesOfFile
        res2 = solvePart2 linesOfFile
    putStrLn $ "Part 1: " ++ show res1
    putStrLn $ "Part 2: " ++ show res2