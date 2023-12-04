module Day2 (solve1, solve2) where

import Data.List (groupBy, maximumBy, sortBy)
import Data.Text qualified as T
import RIO
import RIO.List.Partial (head, last)
import Utils
import Prelude (print)

data Color = Red | Green | Blue deriving (Show, Eq, Ord)

data Retrieval = Retrieval {game :: Int, red :: Int, green :: Int, blue :: Int} deriving (Show, Eq, Ord)

-- Input like "Game 1"
getGame :: Text -> Int
getGame s = stringToUnsigned $ last $ T.split (== ' ') s

-- Return [gameID, redMax, greenMax, blueMax]
processLine :: Text -> Retrieval
processLine line =
  -- split by ":"
  let (gameText, bagText) = splitTwo ": " line
      game = getGame gameText
      bag = concatMap (T.splitOn ", ") $ T.splitOn "; " bagText
      group = groupBy (\x y -> fst x == fst y) $ sortBy (comparing fst) $ map checkColorCount bag
      maxInGroup = map (maximumBy (comparing snd)) group
      red = snd $ head $ filter (\x -> fst x == Red) maxInGroup
      green = snd $ head $ filter (\x -> fst x == Green) maxInGroup
      blue = snd $ head $ filter (\x -> fst x == Blue) maxInGroup
      checkColorCount x =
        let (countText, color) = splitTwo " " x
            count = stringToUnsigned countText
         in case color of
              "red" -> (Red, count)
              "green" -> (Green, count)
              "blue" -> (Blue, count)
              _ -> error $ "unknown color" <> show x
   in Retrieval {game, red, green, blue}

-- The Elf would first like to know which games would have been
-- possible if the bag contained only
-- 12 red cubes, 13 green cubes, and 14 blue cubes?
solve1 :: Text -> IO ()
solve1 input =
  let linesOfInput = T.lines input
      results = map processLine linesOfInput
   in print $ sum $ map (\x -> if red x <= 12 && green x <= 13 && blue x <= 14 then game x else 0) results

solve2 :: Text -> IO ()
solve2 input =
  let linesOfInput = T.lines input
      results = map processLine linesOfInput
   in print $ sum $ map (\x -> red x * green x * blue x) results
