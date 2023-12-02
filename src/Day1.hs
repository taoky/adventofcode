{-# LANGUAGE OverloadedStrings #-}

module Day1 (solve1, solve2) where

import Data.Char (isDigit)
import Data.Either (fromRight)
import Data.List (find)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Text.Read (decimal)
import RIO.Text (Text)
import RIO.Text qualified as T
import RIO.Text.Partial qualified as T'
import Prelude

filterFirstAndLastNumeric :: Text -> Text
-- filterFirstAndLastNumeric s = maybeToList (find isDigit s) ++ maybeToList (find isDigit (reverse s))
filterFirstAndLastNumeric s = T.singleton (T'.head (T.filter isDigit s)) `T.append` T.singleton (T'.last (T.filter isDigit s))

wordToDigit :: Map.Map Text Text
wordToDigit = Map.fromList [("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9")]

-- stringTupleListToTextTupleList :: [(String, String)] -> [(Text, Text)]
-- stringTupleListToTextTupleList = map (Data.Bifunctor.bimap pack pack)

-- replaceWordWithDigit :: String -> String
-- replaceWordWithDigit s = unpack (foldl (\acc (old, new) -> replace old new acc) (pack s) (stringTupleListToTextTupleList wordToDigit))

searchSubstring :: Text -> [Text] -> Maybe Text
-- searchSubstring s = find (`isInfixOf` s)
searchSubstring s = find (`T.isInfixOf` s)

-- digit, or words in wordToDigit
findFirstNumeric :: Text -> Maybe Int
findFirstNumeric s = do
  let maybeDigit = find isDigit (T.unpack s)
  case maybeDigit of
    Just digit -> Just (read [digit] :: Int)
    Nothing -> do
      case searchSubstring s (Map.keys wordToDigit) of
        -- Just key -> Just (read (fromJust (Map.lookup key wordToDigit)) :: Int)
        Just key -> Just (fst (fromRight (0, T.empty) (decimal (fromJust (Map.lookup key wordToDigit)))))
        Nothing -> Nothing

getFirstLineNumber :: Text -> Bool -> Int
getFirstLineNumber s r = go r 1
  where
    go :: Bool -> Int -> Int
    go isReverse lineNumber = do
      -- starts from 1, 2, 3, ... characters
      let (first, rest) = T.splitAt (if isReverse then T.length s - lineNumber else lineNumber) s
      case findFirstNumeric (if isReverse then rest else first) of
        Just number -> number
        Nothing -> go isReverse (lineNumber + 1)

getLineNumber :: Text -> Int
getLineNumber s = 10 * getFirstLineNumber s False + getFirstLineNumber s True

stringToInt :: Text -> Int
-- stringToInt str = read (T.unpack str) :: Int
stringToInt str = fst (fromRight (0, T.empty) (decimal str))

solve1 :: Text -> IO ()
solve1 input =
  let linesOfInput = T.lines input
      filteredLines = map (stringToInt . filterFirstAndLastNumeric) linesOfInput
   in print $ sum filteredLines

solve2 :: Text -> IO ()
solve2 input =
  let linesOfInput = T.lines input
      filteredLines = map getLineNumber linesOfInput
   in print $ sum filteredLines

-- in print filteredLines