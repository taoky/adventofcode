-- Use zzh's solution as a ref at first.
-- It seems easier to use String ([Char]) instead of Text here.
module Day12 (solve1, solve2) where

import Data.Attoparsec.Text qualified as P
import RIO
import RIO.HashMap qualified as HM
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

fastCount :: [Char] -> [Int] -> HashMap ([Char], [Int]) Int -> (Int, HashMap ([Char], [Int]) Int)
fastCount x y hm =
  let firstChar = head x
      isPrefixedByNNonDots n s = notElem '.' (take n s)
      firstGroupCount = head y
      wrapper r = (r, HM.insert (x, y) r hm)
      wrapper' (r, hm') = (r, HM.insert (x, y) r hm')
      res
        | null x && null y = wrapper 1
        | null x = wrapper 0
        | null y && '#' `elem` x = wrapper 0
        | null y = wrapper 1
        | length x < sum y + length y - 1 = wrapper 0
        | otherwise = case firstChar of
            '.' -> wrapper' $ fastCount (tail x) y hm
            '#' ->
              if isPrefixedByNNonDots firstGroupCount x && (length x == firstGroupCount || (length x > firstGroupCount && x !! firstGroupCount /= '#'))
                then wrapper' $ fastCount (drop (firstGroupCount + 1) x) (tail y) hm
                else wrapper 0
            _ -> do
              let (r1, hm1) = fastCount ("#" <> tail x) y hm
                  (r2, hm2) = fastCount ("." <> tail x) y hm1
               in wrapper' (r1 + r2, hm2)
   in case HM.lookup (x, y) hm of
        Just v -> (v, hm)
        Nothing -> res

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
      -- print $ sum $ map (uncurry fastCount) linesOfInput
      print
        $ fst
        $ foldl'
          ( \acc (x, y) ->
              let res = fastCount x y (snd acc)
               in first (fst acc +) res
          )
          (0, HM.empty)
          linesOfInput

-- print $ map (uncurry fastCount) linesOfInput

solve2 :: Text -> IO ()
solve2 input =
  let linesOfInput = case P.parseOnly parser input of
        Left err -> error err
        Right x -> x
   in -- VERY SLOW!: print $ sum $ map ((\(x, y) -> length $ filter (== y) $ map groupCount $ replaceQuestionMark x) . enlarge) linesOfInput
      -- Still slow but at least solvable.
      -- print $ sum $ map (uncurry fastCount . enlarge) linesOfInput
      print
        $ fst
        $ foldl'
          ( \acc (x, y) ->
              let res = fastCount x y (snd acc)
               in first (fst acc +) res
          )
          (0, HM.empty)
        $ map enlarge linesOfInput
