module Main (main) where

import qualified Day1

main :: IO ()
main = do
    contents <- readFile "input/day1"
    -- contents <- readFile "input/day1-2.test"
    Day1.solve1 contents
    Day1.solve2 contents
