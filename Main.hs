module Main where

import System.Environment (getArgs)
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12

dispatch :: String -> String -> String
dispatch "1" = Day1.solve
dispatch "2" = Day2.solve
dispatch "3" = Day3.solve
dispatch "4" = Day4.solve
dispatch "5" = Day5.solve
dispatch "6" = Day6.solve
dispatch "7" = Day7.solve
dispatch "8" = Day8.solve
dispatch "9" = Day9.solve
dispatch "10" = Day10.solve
dispatch "11" = Day11.solve
dispatch "12" = Day12.solve
dispatch _   = const "Invalid day"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dayNum] -> do
            let fileName = "Inputs/Day" ++ dayNum ++ ".txt"
            input <- readFile fileName
            putStrLn $ "Running Day " ++ dayNum
            putStrLn $ dispatch dayNum input
        _ -> putStrLn "Usage: stack run <DayNumber>"