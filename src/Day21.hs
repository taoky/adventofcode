module Day21 (solve1, solve2) where

import Data.Vector qualified as V
import Data.MemoUgly qualified as M
import RIO
import RIO.HashSet qualified as HS
import RIO.List.Partial (head, (!!))
import RIO.Text qualified as T
import Utils
import Prelude (print)

data Item = Plot | Rock deriving (Show, Eq)

data Direction = Up | Dow | Lef | Righ deriving (Show, Eq)

type Matrix = V.Vector (V.Vector Item)

walk :: (Int, Int) -> Direction -> (Int, Int)
walk (x, y) Up = (x - 1, y)
walk (x, y) Dow = (x + 1, y)
walk (x, y) Lef = (x, y - 1)
walk (x, y) Righ = (x, y + 1)

isFeasible :: Matrix -> (Int, Int) -> Bool
isFeasible map' (x, y) = x >= 0 && x < V.length map' && y >= 0 && y < V.length (map' V.! x) && map' V.! x V.! y /= Rock

getElement2 :: Matrix -> (Int, Int) -> Item
getElement2 map' (x, y) =
  let mappedX = x `mod` V.length map'
      mappedY = y `mod` V.length (map' V.! mappedX)
   in map' V.! mappedX V.! mappedY

isFeasible2 :: Matrix -> (Int, Int) -> Bool
isFeasible2 map' (x, y) = getElement2 map' (x, y) /= Rock

parse :: Text -> (Matrix, (Int, Int))
parse input =
  let chars = (map T.unpack $ T.lines input)
      map' =
        V.fromList
          ( map
              ( V.fromList
                  . map
                    ( \case
                        '#' -> Rock
                        '.' -> Plot
                        'S' -> Plot
                        _ -> error "unexpected char"
                    )
              )
              chars
          )
      start = filter (\(x, y) -> chars !! x !! y == 'S') [(x, y) | x <- [0 .. length chars - 1], y <- [0 .. length (head chars) - 1]]
      start' = assert' (length start == 1) $ head start
   in (map', start')

step1 :: Matrix -> [(Int, Int)] -> [(Int, Int)]
step1 map' positions =
  let positions' = map (`walk` Up) positions <> map (`walk` Dow) positions <> map (`walk` Lef) positions <> map (`walk` Righ) positions
      positions'' = filter (isFeasible map') (HS.toList $ HS.fromList positions')
   in positions''

applyStep1 :: Int -> Matrix -> [(Int, Int)] -> [(Int, Int)]
applyStep1 0 _ positions = positions
applyStep1 n map' positions = applyStep1 (n - 1) map' (step1 map' positions)

step2 :: Matrix -> [(Int, Int)] -> [(Int, Int)]
step2 map' positions =
  let positions' = map (`walk` Up) positions <> map (`walk` Dow) positions <> map (`walk` Lef) positions <> map (`walk` Righ) positions
      positions'' = filter (isFeasible2 map') (HS.toList $ HS.fromList positions')
   in positions''

-- applyStep2 :: Int -> Matrix -> [(Int, Int)] -> [(Int, Int)]
-- applyStep2 0 _ positions = positions
-- applyStep2 n map' positions = applyStep2 (n - 1) map' (step2 map' positions)

solve1 :: Text -> IO ()
solve1 input =
  let (map', start) = parse input
   in print $ length $ applyStep1 64 map' [start]

-- solving ax^2 + bx + c = 0
cramer :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Double, Double, Double)
cramer (x1, y1) (x2, y2) (x3, y3) =
  let matrix = V.fromList [V.fromList [x1 * x1, x1, 1, y1], V.fromList [x2 * x2, x2, 1, y2], V.fromList [x3 * x3, x3, 1, y3]]
      d =
        fromIntegral
          $ (matrix V.! 0 V.! 0 * (matrix V.! 1 V.! 1 * matrix V.! 2 V.! 2 - matrix V.! 1 V.! 2 * matrix V.! 2 V.! 1))
          - (matrix V.! 0 V.! 1 * (matrix V.! 1 V.! 0 * matrix V.! 2 V.! 2 - matrix V.! 1 V.! 2 * matrix V.! 2 V.! 0))
          + (matrix V.! 0 V.! 2 * (matrix V.! 1 V.! 0 * matrix V.! 2 V.! 1 - matrix V.! 1 V.! 1 * matrix V.! 2 V.! 0))
      da =
        fromIntegral
          $ (matrix V.! 0 V.! 3 * (matrix V.! 1 V.! 1 * matrix V.! 2 V.! 2 - matrix V.! 1 V.! 2 * matrix V.! 2 V.! 1))
          - (matrix V.! 0 V.! 1 * (matrix V.! 1 V.! 3 * matrix V.! 2 V.! 2 - matrix V.! 1 V.! 2 * matrix V.! 2 V.! 3))
          + (matrix V.! 0 V.! 2 * (matrix V.! 1 V.! 3 * matrix V.! 2 V.! 1 - matrix V.! 1 V.! 1 * matrix V.! 2 V.! 3))
      db =
        fromIntegral
          $ (matrix V.! 0 V.! 0 * (matrix V.! 1 V.! 3 * matrix V.! 2 V.! 2 - matrix V.! 1 V.! 2 * matrix V.! 2 V.! 3))
          - (matrix V.! 0 V.! 3 * (matrix V.! 1 V.! 0 * matrix V.! 2 V.! 2 - matrix V.! 1 V.! 2 * matrix V.! 2 V.! 0))
          + (matrix V.! 0 V.! 2 * (matrix V.! 1 V.! 0 * matrix V.! 2 V.! 3 - matrix V.! 1 V.! 3 * matrix V.! 2 V.! 0))
      dc =
        fromIntegral
          $ (matrix V.! 0 V.! 0 * (matrix V.! 1 V.! 1 * matrix V.! 2 V.! 3 - matrix V.! 1 V.! 3 * matrix V.! 2 V.! 1))
          - (matrix V.! 0 V.! 1 * (matrix V.! 1 V.! 0 * matrix V.! 2 V.! 3 - matrix V.! 1 V.! 3 * matrix V.! 2 V.! 0))
          + (matrix V.! 0 V.! 3 * (matrix V.! 1 V.! 0 * matrix V.! 2 V.! 1 - matrix V.! 1 V.! 1 * matrix V.! 2 V.! 0))
   in (da / d, db / d, dc / d)

-- Observations:
-- Grid is 131x131
-- No Rock in starting row and column
-- Starting position is at (65, 65), aka, center
-- 26501365 = 131 * 202300 + 65
-- for every x which x % 65 == 131, it forms a quadratic function
solve2 :: Text -> IO ()
solve2 input =
  let (map', start) = parse input
      applyStep2 :: Int -> [(Int, Int)] -> [(Int, Int)]
      applyStep2 0 positions = positions
      applyStep2 n positions = applyStep2Memo (n - 1) (step2 map' positions)
      applyStep2Memo = M.memo applyStep2
      points = map (\i -> let v = 65 + 131 * i in (v, length $ applyStep2Memo v [start])) [0 .. 2]
      x = 26501365
   in print
        $ (round :: Double -> Int64)
        $ (\(a, b, c) -> a * x * x + b * x + c)
        $ cramer (points !! 0) (points !! 1) (points !! 2)
