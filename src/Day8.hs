module Day8 (solve1, solve2) where

import Data.Attoparsec.Text qualified as P
import Data.Char qualified as Char
import Data.List (foldl1')
import RIO
import RIO.HashMap qualified as H
import RIO.HashMap.Partial qualified as H'
import RIO.Text qualified as T
import RIO.Text.Partial (last)
import Prelude (print)

word :: P.Parser Text
word = P.takeWhile1 Char.isAlpha

parser :: P.Parser (Text, HashMap Text (Text, Text))
parser = do
  plan <- word <* P.endOfLine
  P.endOfLine
  ml <- P.many' $ do
    k <- word <* " = ("
    v1 <- word <* ", "
    v2 <- word <* ")"
    P.endOfLine
    pure $ H.singleton k (v1, v2)
  P.skipSpace
  P.endOfInput
  let m = mconcat ml
  pure (plan, m)

walk :: Text -> HashMap Text (Text, Text) -> Text -> Int -> (Text -> Bool) -> Int
walk plan m pos iteration endCondition =
  let (vLeft, vRight) = m H'.! pos
      currentDirection = T.index plan (iteration `mod` T.length plan)
      nextPos = case currentDirection of
        'L' -> vLeft
        'R' -> vRight
        _ -> error "invalid direction"
   in if endCondition pos then iteration else walk plan m nextPos (iteration + 1) endCondition

-- SLOW!
-- walkInBatch :: Text -> HashMap Text (Text, Text) -> [Text] -> Int -> (Text -> Bool) -> Int
-- walkInBatch plan m poses iteration endCondition =
--   let vChoices = map (m H'.!) poses
--       currentDirection = T.index plan (iteration `mod` T.length plan)
--       nextPoses = map (\(vLeft, vRight) -> case currentDirection of
--         'L' -> vLeft
--         'R' -> vRight
--         _ -> error "invalid direction") vChoices
--    in if all (==True) $ map endCondition nextPoses then iteration else walkInBatch plan m nextPoses (iteration + 1) endCondition

solve1 :: Text -> IO ()
solve1 input =
  let (plan, maps) = case P.parseOnly parser input of
        Left err -> error err
        Right x -> x
   in print $ walk plan maps "AAA" 0 (== "ZZZ")

solve2 :: Text -> IO ()
solve2 input =
  let (plan, maps) = case P.parseOnly parser input of
        Left err -> error err
        Right x -> x
      starts = filter (\x -> last x == 'A') $ H.keys maps
      steps = map (\start -> walk plan maps start 0 (\x -> last x == 'Z')) starts
   in print $ foldl1' lcm (map fromIntegral steps :: [Int64])
