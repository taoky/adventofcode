module Day18 (solve1, solve2) where

import Data.Attoparsec.Text qualified as P
import RIO
import Prelude (print)

data Direction = Up | Dow | Lef | Righ deriving (Show, Eq)

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

solve1 :: Text -> IO ()
solve1 input =
  let l = case P.parseOnly parser input of
        Left err -> error err
        Right x -> x
   in print l

solve2 :: Text -> IO ()
solve2 input = print "Not implemented yet"
