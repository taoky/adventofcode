module Day1 (solve1, solve2) where

import Data.Char (digitToInt, isDigit)
import Data.List (find)
import RIO
import RIO.Text qualified as T
import RIO.Text.Partial qualified as T'
import Utils (stringToUnsigned)
import Prelude (print)

extractFirstAndLastDigits :: Text -> Text
extractFirstAndLastDigits s = T.singleton (T'.head (T.filter isDigit s)) <> T.singleton (T'.last (T.filter isDigit s))

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
findFirstNumeric s =
  let maybeDigit = T.find isDigit s
   in case maybeDigit of
        Just digit -> Just (digitToInt digit)
        Nothing -> searchSubstring s wordToDigit

-- string -> isReverse -> first line number
getFirstLineNumber :: Text -> Bool -> Int
getFirstLineNumber s r = go r 1
  where
    go :: Bool -> Int -> Int
    go isReverse lineNumber =
      -- starts from 1, 2, 3, ... characters
      let (fst_, rst) = T.splitAt (if isReverse then T.length s - lineNumber else lineNumber) s
       in case findFirstNumeric (if isReverse then rst else fst_) of
            Just number -> number
            Nothing -> go isReverse (lineNumber + 1)

getLineNumber :: Text -> Int
getLineNumber s = 10 * getFirstLineNumber s False + getFirstLineNumber s True

solve1 :: Text -> IO ()
solve1 input =
  let linesOfInput = T.lines input
      filteredLines = map (stringToUnsigned . extractFirstAndLastDigits) linesOfInput
   in print $ sum filteredLines

solve2 :: Text -> IO ()
solve2 input =
  let linesOfInput = T.lines input
      filteredLines = map getLineNumber linesOfInput
   in print $ sum filteredLines
