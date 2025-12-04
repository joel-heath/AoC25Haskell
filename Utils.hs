module Utils (
    count,
    foldWhile
) where

count :: (a -> Bool) -> [a] -> Int
count predicate xs = length $ filter predicate xs

-- `drop 1` because we dont want to test the predicate on the seed nor return it
foldWhile :: (acc -> x -> acc) -> (acc -> Bool) -> acc -> [x] -> acc
foldWhile step ok acc xs = last (takeWhile ok (drop 1 (scanl step acc xs)))
