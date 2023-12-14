module Day14 (solve1, solve2) where

import RIO
import RIO.Text qualified as T
import RIO.List.Partial (head, (!!))
import Prelude (print)

data Item = Rock | Block | Empty deriving (Show, Eq)

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
    let rowNum = length items
        colNum = length (head items)
        moveRockUp' (row', col')
          | row' == 0 = (row', col')
          | items !! (row' - 1) !! col' /= Empty = (row', col')
          | otherwise = moveRockUp' (row' - 1, col')
     in moveRockUp' (row, col)

setMap :: (Int, Int) -> Item -> [[Item]] -> [[Item]]
setMap (row, col) item = zipWith (\ rIdx r
  -> (if rIdx == row then
          zipWith
            (\ cIdx x -> (if cIdx == col then item else x)) [0 .. ] r
      else
          r)) [0..]

-- find all Rocks, move them up, and return the new items
moveAll :: [[Item]] -> [[Item]]
moveAll items =
    let rowNum = length items
        colNum = length (head items)
        moveOne (row, col) items' = case items' !! row !! col of
            Rock -> let (newRow, newCol) = moveRockUp (row, col) items'
                     in if newRow == row then items' else setMap (row, col) Empty $ setMap (newRow, newCol) Rock items'
            _ -> items'
        moveAll' items' (row, col)
          | row == rowNum = items'
          | col == colNum = moveAll' items' (row + 1, 0)
          | otherwise = moveAll' (moveOne (row, col) items') (row, col + 1)
     in moveAll' items (0, 0)

solve1 :: Text -> IO ()
solve1 input = let items = parse input
                   movedItems = moveAll items
                in print $ sum $ zipWith (\ row num -> num * length (filter (==Rock) row)) (reverse movedItems) [1..]

solve2 :: Text -> IO ()
solve2 input = print "Not implemented"
