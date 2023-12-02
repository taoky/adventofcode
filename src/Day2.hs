module Day2 (solve1, solve2) where

import Data.List (groupBy, maximumBy, sortBy)
import RIO
import RIO.Text qualified as T
import Utils (stringToUnsigned)
import Prelude (head, last, print)

data Color = Red | Green | Blue deriving (Show, Eq, Ord)

data Retrieval = Retrieval {game :: Int, red :: Int, green :: Int, blue :: Int} deriving (Show, Eq, Ord)

-- Input like "Game 1"
getGame :: Text -> Int
getGame s = do
  stringToUnsigned $ last $ T.split (== ' ') s

-- Return [gameID, redMax, greenMax, blueMax]
processLine :: Text -> Retrieval
processLine line =
  -- split by ":"
  case T.split (== ':') line of
    (gameText : bagText) ->
      let game = getGame gameText
          bag = concatMap (T.split (== ',')) $ concatMap (T.split (== ';')) bagText
       in do
            let group = groupBy (\x y -> fst x == fst y) $ sortBy (comparing fst) $ map checkColorCount bag
            let maxInGroup = map (maximumBy (comparing snd)) group
            let red = snd $ head $ filter (\x -> fst x == Red) maxInGroup
            let green = snd $ head $ filter (\x -> fst x == Green) maxInGroup
            let blue = snd $ head $ filter (\x -> fst x == Blue) maxInGroup
            Retrieval {game, red, green, blue}
      where
        checkColorCount x =
          let y = T.strip x
           in case T.split (== ' ') y of
                (countText : color) ->
                  let count = stringToUnsigned countText
                   in ( case color of
                          ["red"] -> (Red, count)
                          ["green"] -> (Green, count)
                          ["blue"] -> (Blue, count)
                          _ -> error $ "unknown color" <> show color
                      )
                _ -> error $ "unknown color" <> show y
    _ -> error "unknown line"

-- The Elf would first like to know which games would have been
-- possible if the bag contained only
-- 12 red cubes, 13 green cubes, and 14 blue cubes?
solve1 :: Text -> IO ()
solve1 input = do
  let linesOfInput = T.lines input
      results = map processLine linesOfInput
   in print $ sum $ map (\x -> if red x <= 12 && green x <= 13 && blue x <= 14 then game x else 0) results

solve2 :: Text -> IO ()
solve2 input = do
  let linesOfInput = T.lines input
      results = map processLine linesOfInput
   in print $ sum $ map (\x -> red x * green x * blue x) results
