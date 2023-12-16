module Day16 (solve1, solve2) where

import Data.Hashable (hashWithSalt)
import RIO
import RIO.HashSet qualified as HS
import RIO.List.Partial (head, maximum, (!!))
import RIO.Text qualified as T
import Prelude (print)

data Direction = Up | Dow | Lef | Righ deriving (Show, Eq)

instance Hashable Direction where
  hashWithSalt salt Up = hashWithSalt salt (0 :: Int)
  hashWithSalt salt Dow = hashWithSalt salt (1 :: Int)
  hashWithSalt salt Lef = hashWithSalt salt (2 :: Int)
  hashWithSalt salt Righ = hashWithSalt salt (3 :: Int)

parse :: Text -> [[Char]]
parse input = map T.unpack $ T.lines input

step :: (Int, Int) -> Direction -> (Int, Int)
step (x, y) Up = (x - 1, y)
step (x, y) Dow = (x + 1, y)
step (x, y) Lef = (x, y - 1)
step (x, y) Righ = (x, y + 1)

isInBound :: (Int, Int) -> [[Char]] -> Bool
isInBound (x, y) map' =
  let maxX = length map'
      maxY = length $ head map'
   in x >= 0 && x < maxX && y >= 0 && y < maxY

go :: (Int, Int) -> Direction -> [[Char]] -> HashSet ((Int, Int), Direction) -> HashSet ((Int, Int), Direction)
go (x, y) dir map' hs =
  let newHs = HS.insert ((x, y), dir) hs
      current = map' !! x !! y
      nextDirs = case current of
        '.' -> [dir]
        '/' ->
          [ case dir of
              Up -> Righ
              Dow -> Lef
              Lef -> Dow
              Righ -> Up
          ]
        '\\' ->
          [ case dir of
              Up -> Lef
              Dow -> Righ
              Lef -> Up
              Righ -> Dow
          ]
        '-' -> case dir of
          Up -> [Lef, Righ]
          Dow -> [Lef, Righ]
          Lef -> [Lef]
          Righ -> [Righ]
        '|' -> case dir of
          Up -> [Up]
          Dow -> [Dow]
          Lef -> [Up, Dow]
          Righ -> [Up, Dow]
        _ -> error "unexpected char"
      nextDirs' = filter (\dir' -> isInBound (step (x, y) dir') map') nextDirs
   in if HS.member ((x, y), dir) hs
        then hs -- do not duplicate computing
        else foldl' (\acc dir' -> go (step (x, y) dir') dir' map' acc) newHs nextDirs'

getResult :: [[Char]] -> (Int, Int) -> Direction -> Int
getResult map' (x, y) dir =
  length $ toList $ HS.fromList $ map fst $ toList $ go (x, y) dir map' HS.empty

solve1 :: Text -> IO ()
solve1 input =
  let map' = parse input
   in print $ getResult map' (0, 0) Righ

solve2 :: Text -> IO ()
solve2 input =
  let map' = parse input
      choices =
        map (\x -> ((0, x), Dow)) [0 .. length (head map') - 1]
          ++ map (\x -> ((length map' - 1, x), Up)) [0 .. length (head map') - 1]
          ++ map (\x -> ((x, 0), Righ)) [0 .. length map' - 1]
          ++ map (\x -> ((x, length (head map') - 1), Lef)) [0 .. length map' - 1]
   in print $ maximum $ map (uncurry (getResult map')) choices
