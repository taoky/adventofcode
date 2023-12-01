module Day1 where

import Data.Char (isDigit)
import Data.List (find, isInfixOf)
import Data.Maybe (maybeToList, fromJust)
-- import Data.Text (Text, pack, replace, unpack)
import Data.Bifunctor (bimap)
import qualified Data.Map as Map

filterFirstAndLastNumeric :: String -> String
filterFirstAndLastNumeric s = maybeToList (find isDigit s) ++ maybeToList (find isDigit (reverse s))

wordToDigit = Map.fromList [("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9")]

-- stringTupleListToTextTupleList :: [(String, String)] -> [(Text, Text)]
-- stringTupleListToTextTupleList = map (Data.Bifunctor.bimap pack pack)

-- replaceWordWithDigit :: String -> String
-- replaceWordWithDigit s = unpack (foldl (\acc (old, new) -> replace old new acc) (pack s) (stringTupleListToTextTupleList wordToDigit))

searchSubstring :: String -> [String] -> Maybe String
searchSubstring s = find (`isInfixOf` s)

-- digit, or words in wordToDigit
findFirstNumeric :: String -> Maybe Int
findFirstNumeric s = do
    let maybeDigit = find isDigit s
    case maybeDigit of
        Just digit -> Just (read [digit] :: Int)
        Nothing -> do
            case searchSubstring s (Map.keys wordToDigit) of
                Just key -> Just (read (fromJust (Map.lookup key wordToDigit)) :: Int)
                Nothing -> Nothing

getFirstLineNumber :: String -> Bool -> Int
getFirstLineNumber s r = go s r 1
    where
        go :: String -> Bool -> Int -> Int
        go s isReverse lineNumber = do
            -- starts from 1, 2, 3, ... characters
            let (first, rest) = splitAt (if isReverse then length s - lineNumber else lineNumber) s
            case findFirstNumeric (if isReverse then rest else first) of
                Just number -> number
                Nothing -> go s isReverse (lineNumber + 1)

getLineNumber :: String -> Int
getLineNumber s = 10 * getFirstLineNumber s False + getFirstLineNumber s True

stringToInt :: String -> Int
stringToInt str = read str :: Int

solve1 :: String -> IO ()
solve1 input =
  let linesOfInput = lines input
      filteredLines = map (stringToInt . filterFirstAndLastNumeric) linesOfInput
   in print $ sum filteredLines

solve2 :: String -> IO ()
solve2 input =
  let linesOfInput = lines input
      filteredLines = map getLineNumber linesOfInput
   in print $ sum filteredLines
    -- in print filteredLines