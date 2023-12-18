module Day18 (solve1, solve2) where

import Data.Attoparsec.Text qualified as P
import Data.Text.Read (hexadecimal)
import RIO
import RIO.Text qualified as T
import Prelude (print)

data Direction = Up | Dow | Lef | Righ deriving (Show, Eq)

step :: (Int, Int) -> Direction -> Int -> (Int, Int)
step (x, y) Up n = (x - n, y)
step (x, y) Dow n = (x + n, y)
step (x, y) Lef n = (x, y - n)
step (x, y) Righ n = (x, y + n)

parser :: P.Parser [(Direction, Int, Text)]
parser = do
  res <- P.many' $ do
    dir <- P.choice ["U" $> Up, "D" $> Dow, "L" $> Lef, "R" $> Righ]
    P.skipSpace
    len <- P.decimal
    P.skipSpace
    name <- "(#" *> P.takeWhile1 (/= ')') <* ")"
    P.endOfLine
    pure (dir, len, name)
  P.endOfInput
  pure res

getArea :: [(Direction, Int, Text)] -> (Int, Int) -> Int
getArea [] _ = 0
getArea ((dir, step', _) : xs) (x, y) =
  let nextPoint = step (x, y) dir step'
   in case dir of
        Up -> -step' * y + getArea xs nextPoint
        Dow -> step' * y + getArea xs nextPoint
        Lef -> getArea xs nextPoint
        Righ -> getArea xs nextPoint

getCircumference :: [(Direction, Int, Text)] -> Int
getCircumference [] = 0
getCircumference ((_, step', _) : xs) =
  let
   in step' + getCircumference xs

parseHex :: Text -> (Direction, Int)
parseHex input =
  let firstFive = either (\_ -> error "unexpected number string") fst (hexadecimal $ T.take 5 input)
      dir = case T.drop 5 input of
        "0" -> Righ
        "1" -> Dow
        "2" -> Lef
        "3" -> Up
        _ -> error "unexpected dir string"
   in (dir, firstFive)

getAns :: [(Direction, Int, Text)] -> Int
getAns l =
  -- Just like Day 10
  let area = getArea l (0, 0)
      circ = getCircumference l
      interCnt = area - circ `div` 2 + 1
   in interCnt + circ

solve1 :: Text -> IO ()
solve1 input =
  let l = case P.parseOnly parser input of
        Left err -> error err
        Right x -> x
   in print $ getAns l

solve2 :: Text -> IO ()
solve2 input =
  let l = case P.parseOnly parser input of
        Left err -> error err
        Right x -> x
      l' = map (\(_, _, name) -> let (dir, s) = parseHex name in (dir, s, "")) l
   in print $ getAns l'
