module Day11 (solve1, solve2) where

import RIO
import RIO.List qualified as L
import RIO.List.Partial (head, (!!))
import RIO.Text qualified as T
import Prelude (print)

data Item = Star | Empty deriving (Show, Eq)

parse :: Text -> [[Item]]
parse input =
  let chars = (map T.unpack $ T.lines input)
   in map
        ( map
            ( \case
                '#' -> Star
                '.' -> Empty
                _ -> error "unexpected char"
            )
        )
        chars

calculatePairs :: ((Int, Int) -> (Int, Int) -> Int) -> [(Int, Int)] -> [Int]
calculatePairs f xs = [f x y | (x, i) <- zip xs [0 :: Int ..], (y, j) <- zip xs [0 ..], i < j]

solve :: Text -> Int -> IO ()
solve input expansion =
  let items = parse input
      emptyRowNumbers = map fst $ filter (\(_, row) -> all (== Empty) row) $ zip [0 ..] items
      emptyColNumbers = map fst $ filter (\(_, col) -> all (== Empty) col) $ zip [0 ..] $ L.transpose items
      starRawCoords = filter (\(row, col) -> items !! row !! col == Star) [(row, col) | row <- [0 .. length items - 1], col <- [0 .. length (head items) - 1]]
      getDistance (x1, y1) (x2, y2) =
        abs (x1 - x2)
          + abs (y1 - y2)
          + (expansion - 1)
          * length (filter (\x -> x >= min x1 x2 && x <= max x1 x2) emptyRowNumbers)
          + (expansion - 1)
          * length (filter (\x -> x >= min y1 y2 && x <= max y1 y2) emptyColNumbers)
   in print $ sum $ calculatePairs getDistance starRawCoords

solve1 :: Text -> IO ()
solve1 input = solve input 2

solve2 :: Text -> IO ()
solve2 input = solve input 1000000
