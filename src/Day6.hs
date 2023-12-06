module Day6 (solve1, solve2) where

import Data.Attoparsec.Text qualified as P
import Data.Text qualified as T
import RIO
import Utils
import Prelude (print)

parser :: P.Parser [(Int, Int)]
parser = do
  times <- "Time:" *> P.skipSpace *> P.sepBy1 P.decimal P.skipSpace <* P.endOfLine
  distances <- "Distance:" *> P.skipSpace *> P.sepBy1 P.decimal P.skipSpace <* P.endOfLine
  P.skipSpace
  P.endOfInput
  pure $ zip times distances

-- (m - x)x
getDest :: Int -> Int -> Int
getDest m x = (m - x) * x

-- count of getDest m x > n
getCnt :: Int -> Int -> Int
getCnt m n = [b | x <- [1 .. m], let { b = getDest m x }, b > n] & length

intListToText :: [Int] -> Text
intListToText = T.intercalate "" . map tshow

solve1 :: Text -> IO ()
solve1 input =
  let input' = case P.parseOnly parser input of
        Left err -> error err
        Right x -> x
   in print $ product $ map (uncurry getCnt) input'

solve2 :: Text -> IO ()
solve2 input =
  let input' = case P.parseOnly parser input of
        Left err -> error err
        Right x -> x
      time' = map fst input'
      distance' = map snd input'
      time = stringToUnsigned $ intListToText time'
      distance = stringToUnsigned $ intListToText distance'
   in print $ getCnt time distance
