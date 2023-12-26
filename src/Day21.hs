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

dedupWithFilter :: [(Int, Int)] -> ((Int, Int) -> Bool) -> [(Int, Int)]
dedupWithFilter positions ffunc = toList $ HS.filter ffunc (HS.fromList positions)

step1 :: Matrix -> [(Int, Int)] -> [(Int, Int)]
step1 map' positions =
  let positions' = map (`walk` Up) positions <> map (`walk` Dow) positions <> map (`walk` Lef) positions <> map (`walk` Righ) positions
      positions'' = dedupWithFilter positions' (isFeasible map')
   in positions''

applyStep1 :: Int -> Matrix -> [(Int, Int)] -> [(Int, Int)]
applyStep1 0 _ positions = positions
applyStep1 n map' positions = applyStep1 (n - 1) map' (step1 map' positions)

-- correct but slow
step1' :: Matrix -> [(Int, Int)] -> [(Int, Int)]
step1' map' positions =
  let positions' = map (`walk` Up) positions <> map (`walk` Dow) positions <> map (`walk` Lef) positions <> map (`walk` Righ) positions
      positions'' = dedupWithFilter positions' (isFeasible2 map')
   in positions''

-- applyStep2 :: Int -> Matrix -> [(Int, Int)] -> [(Int, Int)]
-- applyStep2 0 _ positions = positions
-- applyStep2 n map' positions = applyStep2 (n - 1) map' (step2 map' positions)

-- memo does not help
-- applyStep2Memo :: Int -> Matrix -> [(Int, Int)] -> HashMap Int [(Int, Int)] -> ([(Int, Int)], HashMap Int [(Int, Int)])
-- applyStep2Memo 0 _ positions memo = (positions, memo)
-- applyStep2Memo n map' positions memo =
--   case HM.lookup n memo of
--     Just positions' -> (positions', memo)
--     Nothing ->
--       let (positions', memo') = applyStep2Memo (n - 1) map' (step2 map' positions) memo
--        in (positions', HM.insert n positions' memo')

-- wrong algorithm
-- applyStep2 :: Int -> Matrix -> (Int, Int) -> [(Int, Int)]
-- applyStep2 n map' (xstart, ystart) =
--   let isMarkOdd = if even (xstart + ystart) then odd n else even n
--       go n' expand =
--         let points = [(xstart - n', ystart), (xstart + n', ystart)]
--             oddFilter x y = let z = x + y in if isMarkOdd then odd z else even z
--             checkPoints x y = filter (uncurry oddFilter) [(x, y + offset) | offset <- [-expand .. expand]]
--           in concatMap (uncurry checkPoints) points
--     in dedupWithFilter (concatMap (\i -> go i (n - i)) [0 .. n]) (isFeasible2 map')

-- BFS, even slower
-- applyStep2 :: Int -> Matrix -> (Int, Int) -> HS.HashSet (Int, Int)
-- applyStep2 n map' start =
--   let bfs :: Seq.Seq ((Int, Int), Int) -> HS.HashSet ((Int, Int), Int) -> HS.HashSet ((Int, Int), Int)
--       bfs q h = case Seq.viewl q of
--         Seq.EmptyL -> h
--         ((x, y), steps) Seq.:< q' ->
--           let points = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
--               points' = filter (isFeasible2 map') points
--               q'' = foldl' (\q_ p -> if steps > 0 then q_ Seq.|> (p, steps - 1) else q_) q' points'
--            in if HS.member ((x, y), steps) h then bfs q' h else bfs q'' (HS.insert ((x, y), steps) h)
--    in HS.map (\((a, b), _) -> (a, b))
--         $ HS.filter (\((_, _), c) -> c == 0)
--         $ bfs (Seq.singleton (start, n)) HS.empty

-- hashset (all reached points, new points)
-- go 2 steps at a time
-- from https://www.reddit.com/r/haskell/comments/18nf2do/comment/kebjlfd/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
step2 :: Matrix -> (HS.HashSet (Int, Int), HS.HashSet (Int, Int)) -> (HS.HashSet (Int, Int), HS.HashSet (Int, Int))
step2 matrix (allReached, newReached) =
  let extended = HS.toList newReached >>= (\x -> step1' matrix [x]) >>= (\x -> step1' matrix [x])
      newReached' = HS.fromList $ filter (not . (`HS.member` allReached)) extended
   in (HS.union allReached newReached', newReached')

applyStep2 :: Int -> Matrix -> (Int, Int) -> HS.HashSet (Int, Int)
applyStep2 n matrix start =
  let go :: Int -> (HS.HashSet (Int, Int), HS.HashSet (Int, Int)) -> (HS.HashSet (Int, Int), HS.HashSet (Int, Int))
      go 0 reached = reached
      go 1 _ = error "go only accepts even number"
      go n_ reached = go (n_ - 2) (step2 matrix reached)
      start' = if even n then [start] else step1' matrix [start]
      n' = if even n then n else n - 1
      (res, _) = go n' (HS.fromList start', HS.fromList start')
   in res

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
-- Thanks for relevant hint from reddit
solve2 :: Text -> IO ()
solve2 input =
  let (map', start) = parse input
      -- in print $ length $ applyStep2 100 map' start
      points = map (\i -> let v = 65 + 131 * i in (v, length $ applyStep2 v map' start)) [0 .. 2]
      x = 26501365
   in print
        $ (round :: Double -> Int64)
        $ (\(a, b, c) -> a * x * x + b * x + c)
        $ cramer (points !! 0) (points !! 1) (points !! 2)
