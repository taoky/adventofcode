module Day3 (solve1, solve2) where

import Data.Char (isDigit)
import RIO
import RIO.List.Partial (head, (!!))
import RIO.Text qualified as T
import Prelude (print, Foldable (foldl))
import Utils
import Data.List (nub)

read2DArray :: Text -> [[Char]]
read2DArray input = map T.unpack $ T.lines input

-- true: symbol, false: number/empty
generateMask2DArray :: [[Char]] -> [[Bool]]
generateMask2DArray = map (map (\x -> not (isDigit x || x == '.')))

safeModify :: a -> [[a]] -> Int -> Int -> [[a]]
safeModify v l x y
  | x < 0 || x >= length l = l
  | y < 0 || y >= length (head l) = l
  | otherwise = take x l ++ [take y (l !! x) ++ [v] ++ drop (y + 1) (l !! x)] ++ drop (x + 1) l

setAdjacent2DArray :: [[Bool]] -> (Int, Int) -> [[Bool]]
setAdjacent2DArray m (x, y) =
  let setTrue = safeModify True
      adjustments = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1), (x - 1, y - 1), (x - 1, y + 1), (x + 1, y - 1), (x + 1, y + 1)]
   in foldl (\acc (a, b) -> setTrue acc a b) m adjustments

generateAdjacent2DArray :: [[Bool]] -> [[Bool]]
generateAdjacent2DArray m = let
    truePos = [(x, y) | x <- [0 .. length m-1], y <- [0 .. length (head m)-1], m !! x !! y]
    in foldl setAdjacent2DArray m truePos

findHead :: [Char] -> Int -> Int
findHead l y
    | y >= 0 && isDigit (l !! y) = findHead l (y - 1)
    | otherwise = y + 1

findTail :: [Char] -> Int -> Int
findTail l y
    | y < length l && isDigit (l !! y) = findTail l (y + 1)
    | otherwise = y - 1

getNumbers :: [[Bool]] -> [[Char]] -> [Int]
getNumbers mask' map_ =
    let -- find all (x, y) satisfying mask[x][y] and isDigit map[x][y]
        truePos = [(x, y) | x <- [0 .. length map_-1], y <- [0 .. length (head map_)-1], mask' !! x !! y && isDigit (map_ !! x !! y)]
        getNumberLeft (x, y) = (x, findHead (map_ !! x) y)
        leftPos = nub $ map getNumberLeft truePos
        -- Assuming that y is the first digit
        getNumber (x, y) =
            let -- find minimal ly <= y: isDigit map[x][ly] ly--
                -- ly = findHead (map_ !! x) y
                ry = findTail (map_ !! x) y
                numberText = take (ry - y + 1) $ drop y $ map_ !! x
            in charListToUnsigned numberText
    in map getNumber leftPos

solve1 :: Text -> IO ()
solve1 input =
  let map' = read2DArray input
      mask' = generateMask2DArray map'
      adjMask = generateAdjacent2DArray mask'
   in 
      print $ sum $ getNumbers adjMask map'
    --   print $ getNumbers adjMask map'

solve2 :: Text -> IO ()
solve2 input = print "solve2"
