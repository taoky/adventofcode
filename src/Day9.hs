module Day9 (solve1, solve2) where

import Data.Text qualified as DT
import RIO
import RIO.List.Partial (head, last, tail)
import RIO.Text qualified as T
import Utils
import Prelude (print)

-- Get a list, return its next number
-- recursive
go :: [Int] -> Bool -> Int
go input isLast =
  let newList = zipWith (-) (tail input) input
      isAllZero = all (== 0) newList
      offset = if isAllZero then 0 else go newList isLast
   in (if isLast then last input + offset else head input - offset)

parser :: Text -> [[Int]]
parser input =
  let linesOfInput = T.lines input
   in map (map stringToSigned . DT.splitOn " ") linesOfInput

solve1 :: Text -> IO ()
solve1 input =
  let inputLines = parser input
      result = map (`go` True) inputLines
   in print $ sum result

solve2 :: Text -> IO ()
solve2 input =
  let inputLines = parser input
      result = map (`go` False) inputLines
   in print $ sum result
