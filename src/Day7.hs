module Day7 (solve1, solve2) where

import Data.List (elemIndex, group, sort, sortBy)
import Data.Text qualified as T
import RIO
import RIO.List (maximumMaybe)
import RIO.List.Partial (head)
import Utils
import Prelude (print)

-- Assuming that length of t is 5
getType :: Text -> Int
getType t =
  let groups = group $ sort $ T.unpack t
      isFiveOfAKind = any (\x -> length x == 5) groups
      isFourOfAKind = any (\x -> length x == 4) groups
      isFullHouse = any (\x -> length x == 3) groups && any (\x -> length x == 2) groups
      isThreeOfAKind = any (\x -> length x == 3) groups && not isFullHouse
      isTwoPair = length (filter (\x -> length x == 2) groups) == 2
      isOnePair = length (filter (\x -> length x == 2) groups) == 1 && length (filter (\x -> length x == 1) groups) == 3
      isHighCard = length groups == 5
   in case (isFiveOfAKind, isFourOfAKind, isFullHouse, isThreeOfAKind, isTwoPair, isOnePair, isHighCard) of
        (True, _, _, _, _, _, _) -> 6
        (_, True, _, _, _, _, _) -> 5
        (_, _, True, _, _, _, _) -> 4
        (_, _, _, True, _, _, _) -> 3
        (_, _, _, _, True, _, _) -> 2
        (_, _, _, _, _, True, _) -> 1
        (_, _, _, _, _, _, True) -> 0
        _ -> error "getType: impossible"

-- J could be any card
getType2 :: Text -> Int
getType2 t =
  let groups = group $ sort $ T.unpack t
      countJ = length $ filter (== 'J') $ T.unpack t
      groupsWithoutJ = filter (\x -> head x /= 'J') groups
      maximumLength = fromMaybe 0 (maximumMaybe $ map length groupsWithoutJ)
      isFiveOfAKind = maximumLength + countJ == 5
      isFourOfAKind = maximumLength + countJ == 4
      isFullHouse =
        maximumLength
          + countJ
          == 3
          && ( (countJ /= 1 && any (\x -> length x == 2) groupsWithoutJ)
                 || (countJ == 1 && length (filter (\x -> length x == 2) groupsWithoutJ) == 2)
             )
      isThreeOfAKind = maximumLength + countJ == 3 && not isFullHouse
      isTwoPair = countJ == 0 && length (filter (\x -> length x == 2) groupsWithoutJ) == 2
      isOnePair = maximumLength + countJ == 2 && not isTwoPair
      isHighCard = maximumLength + countJ == 1
   in case (isFiveOfAKind, isFourOfAKind, isFullHouse, isThreeOfAKind, isTwoPair, isOnePair, isHighCard) of
        (True, _, _, _, _, _, _) -> 6
        (_, True, _, _, _, _, _) -> 5
        (_, _, True, _, _, _, _) -> 4
        (_, _, _, True, _, _, _) -> 3
        (_, _, _, _, True, _, _) -> 2
        (_, _, _, _, _, True, _) -> 1
        (_, _, _, _, _, _, True) -> 0
        _ -> error "getType: impossible"

cardOrder :: [Char]
cardOrder = "23456789TJQKA"

cardOrder2 :: [Char]
cardOrder2 = "J23456789TQKA"

indexOf :: Char -> [Char] -> Int
indexOf c order = case elemIndex c order of
  Just x -> x
  Nothing -> error "indexOf: impossible"

charCompare :: Char -> Char -> [Char] -> Ordering
charCompare a b order = compare (indexOf a order) (indexOf b order)

stringCompare :: String -> String -> [Char] -> Ordering
stringCompare [] [] _ = EQ
stringCompare [] _ _ = LT
stringCompare _ [] _ = GT
stringCompare (a : as) (b : bs) o = case charCompare a b o of
  EQ -> stringCompare as bs o
  x -> x

myCompare :: (Text, Int) -> (Text, Int) -> (Text -> Int) -> [Char] -> Ordering
myCompare a b typefunc order =
  let a' = typefunc $ fst a
      b' = typefunc $ fst b
      -- compare with cardOrder
      fallbackOrdering = stringCompare (T.unpack $ fst a) (T.unpack $ fst b) order
   in if a' > b' then GT else if a' < b' then LT else fallbackOrdering

parse :: Text -> [(Text, Int)]
parse input =
  let ls = T.lines input
   in map (\x -> let (a, b) = splitTwo " " x in (a, stringToUnsigned b)) ls

solve1 :: Text -> IO ()
solve1 input =
  let input' = parse input
      sortedInput = sortBy compare1 input'
      enumerateInput = zip sortedInput ([1 ..] :: [Int])
      compare1 a b = myCompare a b getType cardOrder
   in print $ sum $ map (\x -> snd x * snd (fst x)) enumerateInput

-- print enumerateInput

solve2 :: Text -> IO ()
solve2 input =
  let input' = parse input
      sortedInput = sortBy compare2 input'
      enumerateInput = zip sortedInput ([1 ..] :: [Int])
      compare2 a b = myCompare a b getType2 cardOrder2
   in print $ sum $ map (\x -> snd x * snd (fst x)) enumerateInput

-- print enumerateInput
