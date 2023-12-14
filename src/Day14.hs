module Day14 (solve1, solve2) where

import Data.Hashable (hashWithSalt)
import RIO
import RIO.HashMap qualified as HM
import RIO.List.Partial (head, (!!))
import RIO.Text qualified as T
import Prelude (print)

data Item = Rock | Block | Empty deriving (Show, Eq)

instance Hashable Item where
  hashWithSalt salt Rock = hashWithSalt salt (0 :: Int)
  hashWithSalt salt Block = hashWithSalt salt (1 :: Int)
  hashWithSalt salt Empty = hashWithSalt salt (2 :: Int)

parse :: Text -> [[Item]]
parse input =
  let chars = (map T.unpack $ T.lines input)
   in map
        ( map
            ( \case
                '#' -> Block
                'O' -> Rock
                '.' -> Empty
                _ -> error "unexpected char"
            )
        )
        chars

-- move Rock up until it hits Block or reaches the top
moveRockUp :: (Int, Int) -> [[Item]] -> (Int, Int)
moveRockUp (row, col) items =
  let moveRockUp' (row', col')
        | row' == 0 = (row', col')
        | items !! (row' - 1) !! col' /= Empty = (row', col')
        | otherwise = moveRockUp' (row' - 1, col')
   in moveRockUp' (row, col)

-- Very ugly code as it repeats for every direction
moveRockLeft :: (Int, Int) -> [[Item]] -> (Int, Int)
moveRockLeft (row, col) items =
  let moveRockLeft' (row', col')
        | col' == 0 = (row', col')
        | items !! row' !! (col' - 1) /= Empty = (row', col')
        | otherwise = moveRockLeft' (row', col' - 1)
   in moveRockLeft' (row, col)

moveRockDown :: (Int, Int) -> [[Item]] -> (Int, Int)
moveRockDown (row, col) items =
  let moveRockDown' (row', col')
        | row' == length items - 1 = (row', col')
        | items !! (row' + 1) !! col' /= Empty = (row', col')
        | otherwise = moveRockDown' (row' + 1, col')
   in moveRockDown' (row, col)

moveRockRight :: (Int, Int) -> [[Item]] -> (Int, Int)
moveRockRight (row, col) items =
  let moveRockRight' (row', col')
        | col' == length (head items) - 1 = (row', col')
        | items !! row' !! (col' + 1) /= Empty = (row', col')
        | otherwise = moveRockRight' (row', col' + 1)
   in moveRockRight' (row, col)

setMap :: (Int, Int) -> Item -> [[Item]] -> [[Item]]
setMap (row, col) item =
  zipWith
    ( \rIdx r ->
        ( if rIdx == row
            then
              zipWith
                (\cIdx x -> (if cIdx == col then item else x))
                [0 ..]
                r
            else r
        )
    )
    [0 ..]

moveOne :: (Int, Int) -> [[Item]] -> ((Int, Int) -> [[Item]] -> (Int, Int)) -> [[Item]]
moveOne (row, col) items moveFunc = case items !! row !! col of
  Rock ->
    let (newRow, newCol) = moveFunc (row, col) items
     in setMap (newRow, newCol) Rock $ setMap (row, col) Empty items
  _ -> items

moveAllUp :: [[Item]] -> [[Item]]
moveAllUp items =
  let rowNum = length items
      colNum = length (head items)
      moveAll' items' (row, col)
        | row == rowNum = items'
        | col == colNum = moveAll' items' (row + 1, 0)
        | otherwise = moveAll' (moveOne (row, col) items' moveRockUp) (row, col + 1)
   in moveAll' items (0, 0)

moveAllLeft :: [[Item]] -> [[Item]]
moveAllLeft items =
  let rowNum = length items
      colNum = length (head items)
      moveAll' items' (row, col)
        | row == rowNum = moveAll' items' (0, col + 1)
        | col == colNum = items'
        | otherwise = moveAll' (moveOne (row, col) items' moveRockLeft) (row + 1, col)
   in moveAll' items (0, 0)

moveAllDown :: [[Item]] -> [[Item]]
moveAllDown items =
  let rowNum = length items
      colNum = length (head items)
      moveAll' items' (row, col)
        | row == -1 = items'
        | col == colNum = moveAll' items' (row - 1, 0)
        | otherwise = moveAll' (moveOne (row, col) items' moveRockDown) (row, col + 1)
   in moveAll' items (rowNum - 1, 0)

moveAllRight :: [[Item]] -> [[Item]]
moveAllRight items =
  let rowNum = length items
      colNum = length (head items)
      moveAll' items' (row, col)
        | row == rowNum = moveAll' items' (0, col - 1)
        | col == -1 = items'
        | otherwise = moveAll' (moveOne (row, col) items' moveRockRight) (row + 1, col)
   in moveAll' items (0, colNum - 1)

moveCycle :: [[Item]] -> [[Item]]
moveCycle items = moveAllRight $ moveAllDown $ moveAllLeft $ moveAllUp items

applyCycle :: [[Item]] -> Int -> [[Item]]
applyCycle items 0 = items
applyCycle items cnt = applyCycle (moveCycle items) (cnt - 1)

getLoad :: [[Item]] -> Int
getLoad items = sum $ zipWith (\row num -> num * length (filter (== Rock) row)) (reverse items) [1 ..]

solve1 :: Text -> IO ()
solve1 input =
  let items = parse input
      movedItems = moveAllUp items
   in print $ getLoad movedItems

solve2 :: Text -> IO ()
solve2 input =
  let items = parse input
      -- Slow.
      loopF cnt maxCnt logs items' = case HM.lookup items' logs of
        Just cnt' ->
          let remaining = (maxCnt - cnt') `mod` (cnt - cnt')
           in getLoad $ applyCycle items' remaining
        Nothing -> loopF (cnt + 1) maxCnt (HM.insert items' cnt logs) (moveCycle items')
   in print $ loopF 0 1000000000 HM.empty items
