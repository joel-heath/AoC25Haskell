{-# LANGUAGE MultilineStrings #-}

module Initialise where

import System.IO
import Text.Printf (printf)
import Control.Monad (forM_)


-- The template for the file content
dayTemplate :: Int -> String
dayTemplate dayNum = printf """module Day%d (solve) where

import Grid
import Utils

solvePart1 :: [String] -> Int
solvePart1 input =
    let 
    in undefined

solvePart2 :: [String] -> Int
solvePart2 input =
    let 
    in undefined

solve :: String -> String
solve input = 
    let linesOfFile = lines input
        res1 = solvePart1 linesOfFile
        res2 = solvePart2 linesOfFile
    in \"Part 1: \" ++ show res1 ++ \"\\nPart 2: \" ++ show res2
""" dayNum

main :: IO ()
main = do
    putStrLn "Initialising Advent of Code Repository..."
    
    forM_ [1..12] $ \i -> do
        let fileName = printf "Day%d.hs" i
        let content = dayTemplate i

        writeFile fileName content
        putStrLn $ "  - Created " ++ fileName

    putStrLn "Done! 12 files generated."