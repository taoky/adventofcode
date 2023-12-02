module Day1 (solve1, solve2) where

import Prelude (print)
import Data.Char (digitToInt, isDigit)
import Data.List (find)
import Data.Text.Read (decimal)
import RIO.Text (Text)
import RIO.Text qualified as T
import RIO.Text.Partial qualified as T'
import RIO

extractFirstAndLastDigits :: Text -> Text
extractFirstAndLastDigits s = T.singleton (T'.head (T.filter isDigit s)) `T.append` T.singleton (T'.last (T.filter isDigit s))

wordToDigit :: [(Text, Int)]
wordToDigit =
  [ ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)
  ]

searchSubstring :: Text -> [(Text, Int)] -> Maybe Int
searchSubstring s = fmap snd . find (\(key, _) -> T.isInfixOf key s)

-- digit, or words in wordToDigit
findFirstNumeric :: Text -> Maybe Int
findFirstNumeric s = do
  let maybeDigit = T.find isDigit s
  case maybeDigit of
    Just digit -> Just (digitToInt digit)
    Nothing -> do
      searchSubstring s wordToDigit

-- string -> isReverse -> first line number
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

-- throws error when it could not parse
stringToInt :: Text -> Int
stringToInt str = either (error . show) fst (decimal str)

solve1 :: Text -> IO ()
solve1 input =
  let linesOfInput = T.lines input
      filteredLines = map (stringToInt . extractFirstAndLastDigits) linesOfInput
   in print $ sum filteredLines

solve2 :: Text -> IO ()
solve2 input =
  let linesOfInput = T.lines input
      filteredLines = map getLineNumber linesOfInput
   in print $ sum filteredLines
