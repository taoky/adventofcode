module Day23 (solve1, solve2) where

import Data.Hashable (hashWithSalt)
import Data.Vector qualified as V
import RIO
import RIO.HashSet qualified as HS
import RIO.List.Partial (head)
import RIO.Text qualified as T
import Utils
import Prelude (print)

data Item = Path | Forest | UpSlope | DownSlope | LeftSlope | RightSlope deriving (Show, Eq)

data Direction = Up | Dow | Lef | Righ deriving (Show, Eq)

type Matrix = V.Vector (V.Vector Item)

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

parse :: Text -> Matrix
parse input =
  let chars = (map T.unpack $ T.lines input)
   in V.fromList
        ( map
            ( V.fromList
                . map
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

dfs :: Matrix -> (Int, Int) -> Bool -> Int
dfs matrix (startx, starty) ignoreSlope =
  go (startx, starty) HS.empty 0
  where
    go :: (Int, Int) -> HS.HashSet (Int, Int) -> Int -> Int
    go (x, y) hs acc =
      let dirs =
            if not ignoreSlope
              then case matrix V.! x V.! y of
                UpSlope -> [Up]
                DownSlope -> [Dow]
                LeftSlope -> [Lef]
                RightSlope -> [Righ]
                _ -> [Up, Dow, Lef, Righ]
              else [Up, Dow, Lef, Righ]
          goto = map (`towards` (x, y)) dirs
          isFeasible (x', y') = x' >= 0 && y' >= 0 && x' < V.length matrix && y' < V.length (matrix V.! x') && matrix V.! x' V.! y' /= Forest
          goto' = filter (\i -> not $ HS.member i hs) $ filter isFeasible goto
          hs' = HS.insert (x, y) hs
       in if x == V.length matrix - 1
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

solve1 :: Text -> IO ()
solve1 input =
  let matrix = parse input
      start_ = [(0, y) | y <- [0 .. V.length (matrix V.! 0) - 1], matrix V.! 0 V.! y /= Forest]
      start = head $ assert' (length start_ == 1) start_
   in print $ dfs matrix start False

solve2 :: Text -> IO ()
solve2 input =
  let matrix = parse input
      start_ = [(0, y) | y <- [0 .. V.length (matrix V.! 0) - 1], matrix V.! 0 V.! y /= Forest]
      start = head $ assert' (length start_ == 1) start_
   in print "TOO SLOW" -- $ dfs matrix start True
