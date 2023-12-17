module Day17 (solve1, solve2) where

import Data.Char (digitToInt)
import Data.Hashable (hashWithSalt)
import Data.PQueue.Prio.Min qualified as PQ
import Data.Vector qualified as V
import RIO
import RIO.HashMap qualified as HM
import RIO.Text qualified as T
import Prelude (print)

data Direction = Up | Dow | Lef | Righ deriving (Show, Eq)

data Vertex = Vertex {x :: Int, y :: Int, dir :: Direction} deriving (Show, Eq)

type Matrix = V.Vector (V.Vector Int)

instance Hashable Direction where
  hashWithSalt salt Up = hashWithSalt salt (0 :: Int)
  hashWithSalt salt Dow = hashWithSalt salt (1 :: Int)
  hashWithSalt salt Lef = hashWithSalt salt (2 :: Int)
  hashWithSalt salt Righ = hashWithSalt salt (3 :: Int)

instance Hashable Vertex where
  hashWithSalt salt (Vertex x y dir) = hashWithSalt salt (x, y, dir)

inf :: Int
inf = 1e9

step :: (Int, Int) -> Int -> Direction -> (Int, Int)
step (x, y) steps Up = (x - steps, y)
step (x, y) steps Dow = (x + steps, y)
step (x, y) steps Lef = (x, y - steps)
step (x, y) steps Righ = (x, y + steps)

turn :: Direction -> [Direction]
turn Up = [Lef, Righ]
turn Dow = [Lef, Righ]
turn Lef = [Up, Dow]
turn Righ = [Up, Dow]

isInBound :: (Int, Int) -> Matrix -> Bool
isInBound (x, y) map' =
  let maxX = length map'
      maxY = length $ V.head map'
   in x >= 0 && x < maxX && y >= 0 && y < maxY

next :: Vertex -> [Int] -> Matrix -> [Vertex]
next (Vertex x y dir) stepRange map' =
  let newPos = [step (x, y) step' dir | step' <- stepRange]
      newPos' = filter (\(x', y') -> isInBound (x', y') map') newPos
      dirs = turn dir
   in [Vertex x' y' dir' | (x', y') <- newPos', dir' <- dirs]

-- This function shall only be used for adjacent vertices
-- from -> to -> map -> cost (adj)
cost :: Vertex -> Vertex -> Matrix -> Int
cost (Vertex x y _) (Vertex x' y' _) map'
  | x == x' && y == y' = 0
  | x == x' = sum $ map (map' V.! x V.!) $ filter (/= y) [min y y' .. max y y']
  | y == y' = sum $ map (\x'' -> map' V.! x'' V.! y) $ filter (/= x) [min x x' .. max x x']
  | otherwise = inf

isFinal :: Vertex -> Matrix -> Bool
isFinal (Vertex x y _) map' = x == length map' - 1 && y == length (V.head map') - 1

parse :: Text -> Matrix
parse input = V.fromList $ map (V.fromList . map digitToInt . T.unpack) $ T.lines input

dijkstra :: Matrix -> [Vertex] -> [Int] -> Int
dijkstra map' starts stepRange =
  let hm = HM.empty
      pq = PQ.fromList $ map (0,) starts
      dijkstra' :: HashMap Vertex Int -> PQ.MinPQueue Int Vertex -> Int
      dijkstra' visited pq' =
        let ((minCost, minVertex), pq'') = PQ.deleteFindMin pq'
            visited' = HM.insert minVertex minCost visited
            nextVertices = next minVertex stepRange map'
         in if isFinal minVertex map'
              then minCost
              else
                if HM.member minVertex visited
                  then dijkstra' visited pq''
                  else
                    dijkstra'
                      visited'
                      ( foldl'
                          ( \acc v ->
                            -- check if v has been visited, to speed up
                              if HM.member v visited then acc else PQ.insert (minCost + cost minVertex v map') v acc
                          )
                          pq''
                          nextVertices
                      )
   in dijkstra' hm pq

solve1 :: Text -> IO ()
solve1 input =
  let map' = parse input
   in print $ dijkstra map' [Vertex 0 0 Dow, Vertex 0 0 Righ] [1 .. 3]

solve2 :: Text -> IO ()
solve2 input =
  let map' = parse input
   in print $ dijkstra map' [Vertex 0 0 Dow, Vertex 0 0 Righ] [4 .. 10]
