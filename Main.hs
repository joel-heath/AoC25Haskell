module Main where

import System.Environment (getArgs)
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5

dispatch :: String -> String -> String
dispatch "1" = Day1.solve
dispatch "2" = Day2.solve
dispatch "3" = Day3.solve
dispatch "4" = Day4.solve
dispatch "5" = Day5.solve
dispatch _   = const "Day not implemented yet!"

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