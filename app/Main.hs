module Main (main) where

import Prelude (IO)
import Day1 qualified
import RIO (readFileUtf8)

main :: IO ()
main = do
  contents <- readFileUtf8 "input/day1"
  -- contents <- readFile "input/day1-2.test"
  Day1.solve1 contents
  Day1.solve2 contents
