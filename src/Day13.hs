module Day13 (solve1, solve2) where

import Data.Attoparsec.Text qualified as P
import RIO
import RIO.List.Partial (head, (!!))
import Prelude (print)

-- Ash: ., Rock: #
data Item = Ash | Rock deriving (Show, Eq)

parser :: P.Parser [[[Item]]]
parser = do
  P.sepBy1' (P.many1' (P.many1' (P.choice ["." $> Ash, "#" $> Rock]) <* P.endOfLine)) P.endOfLine <* P.endOfInput

diffItems :: ([Item], [Item]) -> Int
diffItems (xs, ys) = length . filter not $ zipWith (==) xs ys

countHorizontalMirrored :: [[Item]] -> Int -> Maybe Int
countHorizontalMirrored items row =
  let rowNum = length items
      checkCnt = min row (rowNum - row)
      l = takeWhile (< checkCnt) [0 ..]
      hs = zip (map (\i -> items !! (row + i)) l) (map (\i -> items !! (row - 1 - i)) l)
   in (if (row >= 1) && (row < rowNum) then Just $ sum $ map diffItems hs else Nothing)

countVerticalMirrored :: [[Item]] -> Int -> Maybe Int
countVerticalMirrored items col =
  let colNum = length (head items)
      checkCnt = min col (colNum - col)
      getCol x n = map (!! n) x
      l = takeWhile (< checkCnt) [0 ..]
      vs = zip (map (\i -> getCol items (col + i)) l) (map (\i -> getCol items (col - 1 - i)) l)
   in (if (col >= 1) && (col < colNum) then Just $ sum $ map diffItems vs else Nothing)

findHorizontalMirror :: [[Item]] -> Maybe Int
findHorizontalMirror items =
  let inner [] = Nothing
      inner (x : xs) = if countHorizontalMirrored items x == Just 0 then Just x else inner xs
   in inner [0 .. length items - 1]

findHorizontalMirrorWithOne :: [[Item]] -> Maybe Int
findHorizontalMirrorWithOne items =
  let inner [] = Nothing
      inner (x : xs) = if countHorizontalMirrored items x == Just 1 then Just x else inner xs
   in inner [0 .. length items - 1]

findVerticalMirror :: [[Item]] -> Maybe Int
findVerticalMirror items =
  let inner [] = Nothing
      inner (x : xs) = if countVerticalMirrored items x == Just 0 then Just x else inner xs
   in inner [0 .. length (head items) - 1]

findVerticalMirrorWithOne :: [[Item]] -> Maybe Int
findVerticalMirrorWithOne items =
  let inner [] = Nothing
      inner (x : xs) = if countVerticalMirrored items x == Just 1 then Just x else inner xs
   in inner [0 .. length (head items) - 1]

solve1 :: Text -> IO ()
solve1 input =
  let manyItems = case P.parseOnly parser input of
        Left err -> error err
        Right x -> x
      horizontals = map findHorizontalMirror manyItems
      verticals = map findVerticalMirror manyItems
   in print
        $ sum
        $ zipWith
          ( \x y ->
              ( case (x, y) of
                  (Nothing, Nothing) -> error "unexpected"
                  (Just x', Nothing) -> x' * 100
                  (Nothing, Just x') -> x'
                  (Just _, Just _) -> error "unexpected"
              )
          )
          horizontals
          verticals

solve2 :: Text -> IO ()
solve2 input =
  let manyItems = case P.parseOnly parser input of
        Left err -> error err
        Right x -> x
      horizontals = map findHorizontalMirrorWithOne manyItems
      verticals = map findVerticalMirrorWithOne manyItems
   in print
        $ sum
        $ zipWith
          ( \x y ->
              ( case (x, y) of
                  (Nothing, Nothing) -> error "unexpected"
                  (Just x', Nothing) -> x' * 100
                  (Nothing, Just x') -> x'
                  (Just _, Just _) -> error "unexpected"
              )
          )
          horizontals
          verticals
