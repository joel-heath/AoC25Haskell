module Utils (
    count,
    sumBy,
    foldWhile,
    rotateLeft,
    rotateRight,
    rotate180,
    trim,
    trimBy
) where

import Data.List (transpose, dropWhileEnd)
import Data.Char (isSpace)

sumBy :: Num a => (b -> a) -> [b] -> a
sumBy selector xs = sum $ map selector xs

count :: (a -> Bool) -> [a] -> Int
count predicate xs = length $ filter predicate xs

-- `drop 1` because we dont want to test the predicate on the seed nor return it
foldWhile :: (acc -> x -> acc) -> (acc -> Bool) -> acc -> [x] -> acc
foldWhile step ok acc xs = last (takeWhile ok (drop 1 (scanl step acc xs)))

rotateLeft :: [[a]] -> [[a]]
rotateLeft = reverse . transpose

rotateRight :: [[a]] -> [[a]]
rotateRight = transpose . reverse

rotate180 :: [[a]] -> [[a]]
rotate180 = reverse . map reverse

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

trimBy :: [Char] -> String -> String
trimBy chars = dropWhileEnd (`elem` chars) . dropWhile (`elem` chars)