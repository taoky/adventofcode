module Day14 (solve1, solve2) where

import Data.Hashable (hashWithSalt)
import Data.Vector qualified as V
import RIO
import RIO.HashMap qualified as HM
import RIO.Text qualified as T
import Prelude (print)

data Item = Rock | Block | Empty deriving (Show, Eq)

type Matrix = V.Vector (V.Vector Item)

instance Hashable Item where
  hashWithSalt salt Rock = hashWithSalt salt (0 :: Int)
  hashWithSalt salt Block = hashWithSalt salt (1 :: Int)
  hashWithSalt salt Empty = hashWithSalt salt (2 :: Int)

instance Hashable Matrix where
  hashWithSalt salt matrix = hashWithSalt salt (V.toList $ V.map V.toList matrix)

parse :: Text -> Matrix
parse input =
  let chars = (map T.unpack $ T.lines input)
   in V.fromList
        ( map
            ( V.fromList
                . map
                  ( \case
                      '#' -> Block
                      'O' -> Rock
                      '.' -> Empty
                      _ -> error "unexpected char"
                  )
            )
            chars
        )

-- move Rock up until it hits Block or reaches the top
moveRockUp :: (Int, Int) -> Matrix -> (Int, Int)
moveRockUp (row, col) items =
  let moveRockUp' (row', col')
        | row' == 0 = (row', col')
        | items V.! (row' - 1) V.! col' /= Empty = (row', col')
        | otherwise = moveRockUp' (row' - 1, col')
   in moveRockUp' (row, col)

-- Very ugly code as it repeats for every direction
moveRockLeft :: (Int, Int) -> Matrix -> (Int, Int)
moveRockLeft (row, col) items =
  let moveRockLeft' (row', col')
        | col' == 0 = (row', col')
        | items V.! row' V.! (col' - 1) /= Empty = (row', col')
        | otherwise = moveRockLeft' (row', col' - 1)
   in moveRockLeft' (row, col)

moveRockDown :: (Int, Int) -> Matrix -> (Int, Int)
moveRockDown (row, col) items =
  let moveRockDown' (row', col')
        | row' == length items - 1 = (row', col')
        | items V.! (row' + 1) V.! col' /= Empty = (row', col')
        | otherwise = moveRockDown' (row' + 1, col')
   in moveRockDown' (row, col)

moveRockRight :: (Int, Int) -> Matrix -> (Int, Int)
moveRockRight (row, col) items =
  let moveRockRight' (row', col')
        | col' == length (V.head items) - 1 = (row', col')
        | items V.! row' V.! (col' + 1) /= Empty = (row', col')
        | otherwise = moveRockRight' (row', col' + 1)
   in moveRockRight' (row, col)

setMap :: (Int, Int) -> Item -> Matrix -> Matrix
setMap (row, col) item matrix =
  matrix V.// [(row, (matrix V.! row) V.// [(col, item)])]

moveOne :: (Int, Int) -> Matrix -> ((Int, Int) -> Matrix -> (Int, Int)) -> Matrix
moveOne (row, col) items moveFunc = case items V.! row V.! col of
  Rock ->
    let (newRow, newCol) = moveFunc (row, col) items
     in setMap (newRow, newCol) Rock $ setMap (row, col) Empty items
  _ -> items

moveAllUp :: Matrix -> Matrix
moveAllUp items =
  let rowNum = length items
      colNum = length (V.head items)
      moveAll' items' (row, col)
        | row == rowNum = items'
        | col == colNum = moveAll' items' (row + 1, 0)
        | otherwise = moveAll' (moveOne (row, col) items' moveRockUp) (row, col + 1)
   in moveAll' items (0, 0)

moveAllLeft :: Matrix -> Matrix
moveAllLeft items =
  let rowNum = length items
      colNum = length (V.head items)
      moveAll' items' (row, col)
        | row == rowNum = moveAll' items' (0, col + 1)
        | col == colNum = items'
        | otherwise = moveAll' (moveOne (row, col) items' moveRockLeft) (row + 1, col)
   in moveAll' items (0, 0)

moveAllDown :: Matrix -> Matrix
moveAllDown items =
  let rowNum = length items
      colNum = length (V.head items)
      moveAll' items' (row, col)
        | row == -1 = items'
        | col == colNum = moveAll' items' (row - 1, 0)
        | otherwise = moveAll' (moveOne (row, col) items' moveRockDown) (row, col + 1)
   in moveAll' items (rowNum - 1, 0)

moveAllRight :: Matrix -> Matrix
moveAllRight items =
  let rowNum = length items
      colNum = length (V.head items)
      moveAll' items' (row, col)
        | row == rowNum = moveAll' items' (0, col - 1)
        | col == -1 = items'
        | otherwise = moveAll' (moveOne (row, col) items' moveRockRight) (row + 1, col)
   in moveAll' items (0, colNum - 1)

moveCycle :: Matrix -> Matrix
moveCycle items = moveAllRight $ moveAllDown $ moveAllLeft $ moveAllUp items

applyCycle :: Matrix -> Int -> Matrix
applyCycle items 0 = items
applyCycle items cnt = applyCycle (moveCycle items) (cnt - 1)

getLoad :: Matrix -> Int
getLoad items = sum $ V.zipWith (\row num -> num * length (V.filter (== Rock) row)) (V.reverse items) (V.fromList [1 ..])

solve1 :: Text -> IO ()
solve1 input =
  let items = parse input
      movedItems = moveAllUp items
   in print $ getLoad movedItems

solve2 :: Text -> IO ()
solve2 input =
  let items = parse input
      maxCnt = 1000000000
      -- Slow.
      loopF cnt logs items' = case HM.lookup items' logs of
        Just cnt' ->
          let remaining = (maxCnt - cnt') `mod` (cnt - cnt')
           in getLoad $ applyCycle items' remaining
        Nothing -> loopF (cnt + 1) (HM.insert items' cnt logs) (moveCycle items')
   in print $ loopF 0 HM.empty items
