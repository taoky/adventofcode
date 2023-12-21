module Day21 (solve1, solve2) where

import Data.Vector qualified as V
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

applyStep2 :: Int -> Matrix -> [(Int, Int)] -> [(Int, Int)]
applyStep2 0 _ positions = positions
applyStep2 n map' positions = applyStep2 (n - 1) map' (step2 map' positions)

solve1 :: Text -> IO ()
solve1 input =
  let (map', start) = parse input
   in print $ length $ applyStep1 64 map' [start]

solve2 :: Text -> IO ()
solve2 input =
  let (map', start) = parse input
   in print "too slow" -- $ length $ applyStep2 1000 map' [start]
