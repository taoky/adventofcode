-- Use zzh's solution as a ref at first.
-- It seems easier to use String ([Char]) instead of Text here.
module Day12 (solve1, solve2) where

import Data.Attoparsec.Text qualified as P
import Data.MemoCombinators qualified as M
import RIO
import RIO.List qualified as L
import RIO.List.Partial (head, tail, (!!))
import Prelude (print)

-- replaceQuestionMark :: String -> [String]
-- replaceQuestionMark [] = [[]]
-- replaceQuestionMark (c : cs)
--   | c == '?' = [x : rest | x <- ['#', '.'], rest <- replaceQuestionMark cs]
--   | otherwise = [c : rest | rest <- replaceQuestionMark cs]

-- groupCount :: String -> [Int]
-- groupCount input = map length $ filter (\x -> head x == '#') $ L.group input

enlarge :: ([Char], [Int]) -> ([Char], [Int])
enlarge (x, y) = (L.intercalate "?" $ replicate 5 x, concat $ replicate 5 y)

fastCount :: [Char] -> [Int] -> Int
fastCount x y =
  let firstChar = head x
      isPrefixedByNNonDots n s = notElem '.' (take n s)
      firstGroupCount = head y
   in if null x
        then -- trace (tshow (x, y)) $ null x
          if null y then 1 else 0
        else
          if null y
            then -- check if the rest of x contains hash
              if '#' `elem` x then 0 else 1
            else case firstChar of
              '.' -> fastCountMemo (tail x) y
              '#' ->
                if isPrefixedByNNonDots firstGroupCount x && (length x == firstGroupCount || (length x > firstGroupCount && x !! firstGroupCount /= '#'))
                  then fastCountMemo (drop (firstGroupCount + 1) x) (tail y)
                  else 0
              _ -> fastCountMemo ("#" <> tail x) y + fastCountMemo ("." <> tail x) y

fastCountMemo :: [Char] -> [Int] -> Int
fastCountMemo = M.memo2 (M.list M.char) (M.list M.integral) fastCount

parser :: P.Parser [([Char], [Int])]
parser = do
  res <- P.many' $ do
    row <- P.many' (P.notChar ' ')
    P.skipSpace
    nums <- P.sepBy1' P.decimal (P.char ',')
    P.endOfLine
    pure (row, nums)
  P.endOfInput
  pure res

solve1 :: Text -> IO ()
solve1 input =
  let linesOfInput = case P.parseOnly parser input of
        Left err -> error err
        Right x -> x
   in --    in print $ sum $ map (\(x, y) -> length $ filter (== y) $ map groupCount $ replaceQuestionMark x) linesOfInput
      print $ sum $ map (uncurry fastCountMemo) linesOfInput

-- print $ map (uncurry fastCount) linesOfInput

solve2 :: Text -> IO ()
solve2 input =
  let linesOfInput = case P.parseOnly parser input of
        Left err -> error err
        Right x -> x
   in -- VERY SLOW!: print $ sum $ map ((\(x, y) -> length $ filter (== y) $ map groupCount $ replaceQuestionMark x) . enlarge) linesOfInput
      -- Still slow but at least solvable.
      print $ sum $ map (uncurry fastCountMemo . enlarge) linesOfInput
