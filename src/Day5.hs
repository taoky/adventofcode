-- Parser reference: https://github.com/SmartHypercube/aoc2023-haskell/blob/master/src/day2.hs
module Day5 (solve1, solve2) where

import Data.Attoparsec.Text qualified as P
import Data.Char qualified as Char
import Data.List (nub, sort)
import RIO
import RIO.HashMap qualified as H
import RIO.HashMap.Partial qualified as H'
import RIO.List.Partial (head, minimum, tail)
import Utils
import Prelude (print)

data MapItem = MapItem
  { sourceStart :: Int,
    destinationStart :: Int,
    rangeLength :: Int
  }

word :: P.Parser Text
word = P.takeWhile1 Char.isAlpha

parser :: P.Parser ([Int], HashMap Text (Text, [MapItem]))
parser = do
  -- parse "seeds: 79 14 55 13"
  seeds <- "seeds: " *> P.sepBy1 P.decimal P.skipSpace <* P.endOfLine
  P.skipSpace
  ml <- P.many' $ do
    -- parse "seed-to-soil map:"
    from <- word <* "-to-"
    to' <- word <* " map:" <* P.endOfLine
    -- parse multiple lines of "2 3 4" (MapItem) until an empty line
    mapItems <- P.many' $ do
      f <- P.decimal <* " " -- from
      s <- P.decimal <* " " -- to
      t <- P.decimal <* P.endOfLine -- length
      return MapItem {destinationStart = f, sourceStart = s, rangeLength = t}
    P.skipSpace
    pure $ H.singleton from (to', mapItems)
  P.endOfInput
  let m = mconcat ml
  pure (seeds, m)

getDest :: Int -> [MapItem] -> Int
getDest v mapItems = case filter (\x -> sourceStart x <= v && v < sourceStart x + rangeLength x) mapItems of
  mapItem : _ -> v - sourceStart mapItem + destinationStart mapItem
  [] -> v

findLocation :: Int -> Text -> HashMap Text (Text, [MapItem]) -> Int
findLocation oldv k m =
  let (to', mapItems) = m H'.! k
      -- find the first MapItem that contains the seed
      realDest = getDest oldv mapItems
   in if to' /= "location" then findLocation realDest to' m else realDest

-- Range [a, b]
pairUpRange :: (Num a) => [a] -> [(a, a)]
pairUpRange [] = []
pairUpRange (x : y : xs) = (x, y + x - 1) : pairUpRange xs
pairUpRange _ = error "pairUp: odd number of elements"

-- It's slow!
-- expandSeedRange :: [Int] -> [Int]
-- expandSeedRange m =
--     let -- take every two seed into a pair
--         pairs = pairUp m
--         expanded = concatMap (\(x, y) -> [x .. x+y]) pairs
--     in nub $ sort expanded

findLocationWithRange :: (Int, Int) -> Text -> HashMap Text (Text, [MapItem]) -> [(Int, Int)]
findLocationWithRange oldRange k m =
  let (to', mapItems) = m H'.! k
      convertedMapItems = map (\x -> (sourceStart x, sourceStart x + rangeLength x - 1)) mapItems
      pointsofRange = nub $ sort ([x | x <- concatMap (\(a, b) -> [a, b]) convertedMapItems, x >= fst oldRange && x <= snd oldRange] ++ [fst oldRange, snd oldRange])
      -- [1, 2, 3, 4, ...] -> [(1, 2), (2, 3), ...]. If length is 1, then [(x, x)]
      rangePairs' = if length pointsofRange >= 2 then zip pointsofRange (tail pointsofRange) else [(head pointsofRange, head pointsofRange)]
      -- from second item in rangePairs' fst shall be += 1
      rangePairs = if length rangePairs' >= 2 then filter (uncurry (<=)) $ head rangePairs' : map (\(a, b) -> (a + 1, b)) (tail rangePairs') else rangePairs'
      dest = map (\x -> (getDest (fst x) mapItems, getDest (snd x) mapItems)) rangePairs
   in if to' /= "location"
        then -- trace ("findLocationWithRange: " <> tshow oldRange <> " " <> tshow k <> " " <> tshow dest) $
          concatMap (\x -> assert' (uncurry (<=) x) $ findLocationWithRange x to' m) dest
        else -- trace ("findLocationWithRange: " <> tshow oldRange <> " " <> tshow k <> " " <> tshow dest)
          dest

solve1 :: Text -> IO ()
solve1 input =
  let (seeds, maps) = case P.parseOnly parser input of
        Left err -> error err
        Right x -> x
   in print $ minimum $ map (\x -> findLocation x "seed" maps) seeds

solve2 :: Text -> IO ()
solve2 input =
  let (seedRanges, maps) = case P.parseOnly parser input of
        Left err -> error err
        Right x -> x
      locationRanges = concatMap (\x -> findLocationWithRange x "seed" maps) $ pairUpRange seedRanges
   in print $ minimum $ concatMap (\(a, b) -> [a, b]) locationRanges
