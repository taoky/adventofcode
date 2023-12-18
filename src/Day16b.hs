module Day16b (solve1, solve2) where

import Data.Hashable (hashWithSalt)
import Data.Vector qualified as V
import Data.Vector.Hashtables qualified as H
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as VU
import RIO hiding (HashSet)
import RIO.List.Partial (head, maximum)
import RIO.Text qualified as T
import Prelude (print)

type HashTable k v = H.Dictionary (H.PrimState IO) VM.MVector k VM.MVector v

type PosDir = ((Int, Int), Direction)

type HashSet a = HashTable a ()

data Direction = Up | Dow | Lef | Righ deriving (Show, Eq)

data Matrix = Matrix
  { _matrix :: !(VU.Vector Char),
    _width :: !Int,
    _height :: !Int
  }
  deriving (Show, Eq)

makeMatrix :: [[Char]] -> Matrix
makeMatrix xs =
  let width = length $ head xs
      height = length xs
      matrix = VU.fromList $ concat xs
   in Matrix matrix width height

getMatrixElement :: Matrix -> (Int, Int) -> Char
getMatrixElement (Matrix matrix width _) (x, y) = matrix VU.! (x * width + y)

instance Hashable Direction where
  hashWithSalt salt Up = hashWithSalt salt (0 :: Int)
  hashWithSalt salt Dow = hashWithSalt salt (1 :: Int)
  hashWithSalt salt Lef = hashWithSalt salt (2 :: Int)
  hashWithSalt salt Righ = hashWithSalt salt (3 :: Int)

parse :: Text -> Matrix
parse input = makeMatrix $ map T.unpack (T.lines input)

step :: (Int, Int) -> Direction -> (Int, Int)
step (x, y) Up = (x - 1, y)
step (x, y) Dow = (x + 1, y)
step (x, y) Lef = (x, y - 1)
step (x, y) Righ = (x, y + 1)

isInBound :: (Int, Int) -> Matrix -> Bool
isInBound (x, y) map' =
  let maxX = _height map'
      maxY = _width map'
   in x >= 0 && x < maxX && y >= 0 && y < maxY

go :: (Int, Int) -> Direction -> Matrix -> IORef (HashSet PosDir) -> IO ()
go (x, y) dir map' hsRef = do
  let current = getMatrixElement map' (x, y)
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
  hs <- readIORef hsRef
  alreadyVisited <- H.lookup hs ((x, y), dir)
  case alreadyVisited of
    Just _ -> return ()
    Nothing -> do
      H.insert hs ((x, y), dir) ()
      writeIORef hsRef hs
      mapM_ (\dir' -> go (step (x, y) dir') dir' map' hsRef) nextDirs'

getResult :: Matrix -> (Int, Int) -> Direction -> IO Int
getResult map' (x, y) dir = do
  hs <- newIORef =<< H.initialize 0
  go (x, y) dir map' hs
  hs' <- readIORef hs
  hsList <- H.keys hs'
  let pointList = V.map fst hsList
      lp = length pointList
  hs'' :: HashSet (Int, Int) <- H.initialize lp
  V.mapM_ (\a -> H.insert hs'' a ()) pointList
  hs'List <- H.toList hs''
  return $ length hs'List

solve1 :: Text -> IO ()
solve1 input = do
  let map' = parse input
  res <- getResult map' (0, 0) Righ
  print res

solve2 :: Text -> IO ()
solve2 input = do
  let map' = parse input
      choices =
        map (\x -> ((0, x), Dow)) [0 .. _width map' - 1]
          ++ map (\x -> ((_height map' - 1, x), Up)) [0 .. _width map' - 1]
          ++ map (\x -> ((x, 0), Righ)) [0 .. _height map' - 1]
          ++ map (\x -> ((x, _width map' - 1), Lef)) [0 .. _height map' - 1]
  --  in print $ maximum $ map (uncurry (getResult map')) choices
  res <- mapM (uncurry (getResult map')) choices
  print $ maximum res
