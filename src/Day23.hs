module Day23 (solve1, solve2) where

import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.Hashable (hashWithSalt)
import Data.Sequence qualified as Seq
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import RIO
import RIO.HashMap qualified as HM
import RIO.HashMap.Partial qualified as HM'
import RIO.HashSet qualified as HS
import RIO.List qualified as L
import RIO.List.Partial (head)
import RIO.Text qualified as T
import Safe (maximumDef)
import Utils
import Prelude (print)

data Item = Path | Forest | UpSlope | DownSlope | LeftSlope | RightSlope deriving (Show, Eq)

itemToInt :: Item -> Int
itemToInt Path = 0
itemToInt Forest = 1
itemToInt UpSlope = 2
itemToInt DownSlope = 3
itemToInt LeftSlope = 4
itemToInt RightSlope = 5

intToItem :: Int -> Item
intToItem 0 = Path
intToItem 1 = Forest
intToItem 2 = UpSlope
intToItem 3 = DownSlope
intToItem 4 = LeftSlope
intToItem _ = RightSlope

derivingUnbox
  "Item"
  [t|Item -> Int|]
  [|itemToInt|]
  [|intToItem|]

data Direction = Up | Dow | Lef | Righ deriving (Show, Eq, Ord)

data Matrix = Matrix
  { _matrix :: !(VU.Vector Item),
    _width :: !Int,
    _height :: !Int
  }
  deriving (Show, Eq)

makeMatrix :: [[Item]] -> Matrix
makeMatrix xs =
  let width = length $ head xs
      height = length xs
      matrix = VU.fromList $ concat xs
   in Matrix matrix width height

getMatrixElement :: Matrix -> (Int, Int) -> Item
getMatrixElement (Matrix matrix width _) (x, y) = matrix VU.! (x * width + y)

instance Hashable Item where
  hashWithSalt salt Path = hashWithSalt salt (0 :: Int)
  hashWithSalt salt Forest = hashWithSalt salt (1 :: Int)
  hashWithSalt salt UpSlope = hashWithSalt salt (2 :: Int)
  hashWithSalt salt DownSlope = hashWithSalt salt (3 :: Int)
  hashWithSalt salt LeftSlope = hashWithSalt salt (4 :: Int)
  hashWithSalt salt RightSlope = hashWithSalt salt (5 :: Int)

towards :: Direction -> (Int, Int) -> (Int, Int)
towards Up (x, y) = (x - 1, y)
towards Dow (x, y) = (x + 1, y)
towards Lef (x, y) = (x, y - 1)
towards Righ (x, y) = (x, y + 1)

isFeasible :: Matrix -> (Int, Int) -> Bool
isFeasible matrix (x, y) = x >= 0 && y >= 0 && x < _height matrix && y < _width matrix && getMatrixElement matrix (x, y) /= Forest

getNearby :: (Int, Int) -> Matrix -> [(Int, Int)]
getNearby (x, y) matrix =
  let dirs = [Up, Dow, Lef, Righ]
      goto = map (\i -> towards i (x, y)) dirs
   in filter (isFeasible matrix) goto

isJunction :: (Int, Int) -> Matrix -> Bool
isJunction (x, y) matrix = length (getNearby (x, y) matrix) > 2 || x == 0 || x == _height matrix - 1

-- Returns: (x, y), direction, steps (weight)
towardsUntilJunction :: Direction -> (Int, Int) -> Matrix -> Int -> Maybe ((Int, Int), Direction, Int)
towardsUntilJunction dir (x, y) matrix acc =
  let dirs' =
        [dir] <> case dir of
          Up -> [Lef, Righ]
          Dow -> [Lef, Righ]
          Lef -> [Up, Dow]
          Righ -> [Up, Dow]
      goto_ = filter (\(_, j) -> isFeasible matrix j) $ map (\i -> (i, towards i (x, y))) dirs'
      (dir_, (x_, y_)) = head goto_
   in case length goto_ of
        0 -> Nothing
        _ -> if isJunction (x_, y_) matrix then Just ((x_, y_), dir_, acc + 1) else towardsUntilJunction dir_ (x_, y_) matrix (acc + 1)

parse :: Text -> Matrix
parse input =
  let chars = (map T.unpack $ T.lines input)
   in makeMatrix
        ( map
            ( map
                ( \case
                    '#' -> Forest
                    '.' -> Path
                    '^' -> UpSlope
                    'v' -> DownSlope
                    '<' -> LeftSlope
                    '>' -> RightSlope
                    _ -> error "unexpected char"
                )
            )
            chars
        )

-- old naive dfs
dfs :: Matrix -> (Int, Int) -> Bool -> Int
dfs matrix (startx, starty) ignoreSlope =
  go (startx, starty) HS.empty 0
  where
    go :: (Int, Int) -> HS.HashSet (Int, Int) -> Int -> Int
    go (x, y) hs acc =
      let dirs =
            -- ignoreSlope is always false though...
            if not ignoreSlope
              then case getMatrixElement matrix (x, y) of
                UpSlope -> [Up]
                DownSlope -> [Dow]
                LeftSlope -> [Lef]
                RightSlope -> [Righ]
                _ -> [Up, Dow, Lef, Righ]
              else [Up, Dow, Lef, Righ]
          goto = map (`towards` (x, y)) dirs
          goto' = filter (\i -> not $ HS.member i hs) $ filter (isFeasible matrix) goto
          hs' = HS.insert (x, y) hs
       in if x == _height matrix - 1
            then acc
            else case goto' of
              -- dead end, this shall not be counted in
              [] -> -1
              _ ->
                foldl'
                  ( \acc' i ->
                      let res = go i hs' (acc + 1)
                          maxAcc = if res == -1 then acc' else max acc' res
                       in maxAcc
                  )
                  0
                  goto'

type Graph = HashMap (Int, Int) [((Int, Int), Int)]

makeJunctionGraph :: (Int, Int) -> Matrix -> Graph
makeJunctionGraph start matrix =
  let bfs :: Seq.Seq ((Int, Int), Direction) -> HS.HashSet (Int, Int) -> Graph -> (HS.HashSet (Int, Int), Graph)
      bfs q h g =
        case Seq.viewl q of
          Seq.EmptyL -> (h, g)
          ((x, y), dir) Seq.:< xs ->
            let res = towardsUntilJunction dir (x, y) matrix 0
                h' = HS.insert (x, y) h
                nextDirs d = case d of
                  Up -> [Lef, Righ, Up]
                  Dow -> [Lef, Righ, Dow]
                  Lef -> [Up, Dow, Lef]
                  Righ -> [Up, Dow, Righ]
             in case res of
                  Nothing -> bfs xs h' g
                  Just ((x', y'), dir', weight) ->
                    if HS.member (x', y') h'
                      then bfs xs h' g
                      else
                        let g' = HM.insertWith (++) (x', y') [((x, y), weight)] $ HM.insertWith (++) (x, y) [((x', y'), weight)] g
                            g'' = HM.insert (x', y') (L.nub $ g' HM'.! (x', y')) $ HM.insert (x, y) (L.nub $ g' HM'.! (x, y)) g'
                            q' = xs <> Seq.fromList (map ((x', y'),) (nextDirs dir'))
                         in bfs q' h' g''
   in snd $ bfs (Seq.singleton (start, Dow)) HS.empty HM.empty

parallelArg :: Int
parallelArg = 4

-- new dfs with junction graph
dfs' :: Graph -> (Int, Int) -> (Int -> Bool) -> Int
dfs' graph start isEnd =
  go start HS.empty 0 0
  where
    go :: (Int, Int) -> HS.HashSet (Int, Int) -> Int -> Int -> Int
    go (x, y) hs acc depth =
      let goto = HM.lookupDefault [] (x, y) graph
          goto' = filter (\i -> not $ HS.member (fst i) hs) goto
          hs' = HS.insert (x, y) hs
          l =
            if depth < parallelArg
              then parMap rdeepseq (\i -> go (fst i) hs' (acc + snd i) (depth + 1)) goto'
              else map (\i -> go (fst i) hs' (acc + snd i) (depth + 1)) goto'
       in if isEnd x
            then acc
            else case goto' of
              -- dead end, this shall not be counted in
              [] -> -1
              _ -> l & filter (/= -1) & maximumDef 0

solve1 :: Text -> IO ()
solve1 input =
  let matrix = parse input
      start_ = [(0, y) | y <- [0 .. _width matrix - 1], getMatrixElement matrix (0, y) /= Forest]
      start = head $ assert' (length start_ == 1) start_
   in print $ dfs matrix start False

solve2 :: Text -> IO ()
solve2 input =
  let matrix = parse input
      start_ = [(0, y) | y <- [0 .. _width matrix - 1], getMatrixElement matrix (0, y) /= Forest]
      start = head $ assert' (length start_ == 1) start_
      junctionGraph = makeJunctionGraph start matrix
   in print $ dfs' junctionGraph start (\x -> x == _height matrix - 1)
