module Day2 (solve1, solve2) where

import Prelude (print)
import RIO
import RIO.Text (Text)
import RIO.Text qualified as T

processLine :: Text -> Int
processLine line = do
    1

-- The Elf would first like to know which games would have been
-- possible if the bag contained only 
-- 12 red cubes, 13 green cubes, and 14 blue cubes?
solve1 :: Text -> IO ()
solve1 input = do
  let lines = T.lines input
      results = map processLine lines
    in print "test"

solve2 :: Text -> IO ()
solve2 input = do
  print "Day 2, solve 2"
