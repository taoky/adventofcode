module Day15 (solve1, solve2) where

import Data.Char (ord)
import Data.Text qualified as DT
import RIO
import RIO.List.Partial ((!!))
import RIO.Text qualified as T
import RIO.Text.Partial (last)
import Utils
import Prelude (print)

data Step = Step {label :: Text, num :: Int} deriving (Show)

-- text -> current value -> result
getHash :: Text -> Int -> Int
getHash input cv =
  case T.uncons input of
    Just (c, rest) ->
      let v = (cv + ord c) * 17 `mod` 256
       in getHash rest v
    Nothing -> cv

parseStep :: Text -> (Step, Bool)
parseStep input =
  let isRemoval = last input == '-'
      (label, num) = if isRemoval then (fst $ splitTwo "-" input, -1) else second stringToUnsigned $ splitTwo "=" input
   in (Step {label = label, num = num}, isRemoval)

removeLabelInList :: Text -> [Step] -> [Step]
removeLabelInList label' = filter (\s -> label s /= label')

updateOrAppendLabelInList :: Step -> [Step] -> [Step]
updateOrAppendLabelInList step list =
  let label' = label step
      num' = num step
      isLabelInList = any (\s -> label s == label') list
   in if isLabelInList then map (\s -> if label s == label' then s {num = num'} else s) list else list ++ [step]

solve1 :: Text -> IO ()
solve1 input =
  let segments = DT.splitOn "," (T.strip input)
      result = map (`getHash` 0) segments
   in print $ sum result

solve2 :: Text -> IO ()
solve2 input =
  let segments = DT.splitOn "," (T.strip input)
      handleSegment :: Text -> [[Step]] -> [[Step]]
      handleSegment segment store =
        let (step, isRemoval) = parseStep segment
            labelHash = getHash (label step) 0
         in take labelHash store
              ++ ( if isRemoval
                     then [removeLabelInList (label step) (store !! labelHash)]
                     else [updateOrAppendLabelInList step (store !! labelHash)]
                 )
              ++ drop (labelHash + 1) store
      loopF :: [Text] -> [[Step]] -> [[Step]]
      loopF [] store = store
      loopF (segment : rest) store = loopF rest (handleSegment segment store)
      boxes = loopF segments (replicate 256 [])
   in print
        $ sum
        $ zipWith
          ( \idx box ->
              idx
                * sum (zipWith (\idx' step -> idx' * num step) [1 ..] box)
          )
          [1 ..]
          boxes
