module Utils (
    count,
    sum',
    foldWhile
) where

sum' :: Num a => (b -> a) -> [b] -> a
sum' selector xs = sum $ map selector xs

count :: (a -> Bool) -> [a] -> Int
count predicate xs = length $ filter predicate xs

-- `drop 1` because we dont want to test the predicate on the seed nor return it
foldWhile :: (acc -> x -> acc) -> (acc -> Bool) -> acc -> [x] -> acc
foldWhile step ok acc xs = last (takeWhile ok (drop 1 (scanl step acc xs)))
