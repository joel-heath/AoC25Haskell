module Utils (
    count,
    foldWhile
) where

count :: (a -> Bool) -> [a] -> Int
count predicate xs = length $ filter predicate xs

-- `tail` because we want to skip testing the predicate on the seed
foldWhile :: (acc -> x -> acc) -> (acc -> Bool) -> acc -> [x] -> acc
foldWhile step ok acc xs = last (takeWhile ok (tail (scanl step acc xs)))
