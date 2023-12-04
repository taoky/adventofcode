module Day4 (solve1, solve2) where

import Data.Text qualified as T
import RIO
import RIO.List.Partial (head, last, (!!))
import Utils
import Prelude (print)

extractCards :: Text -> [Int]
extractCards x = map stringToUnsigned $ filter (not . T.null) $ T.split (== ' ') x

processLine :: Text -> (Int, Int)
processLine line =
  case T.splitOn ": " line of
    (cardText : resultText) ->
      let card = stringToUnsigned $ last $ T.split (== ' ') cardText
          (win, chosen) = case T.splitOn " | " $ head resultText of
            (winText : chosenText) ->
              let winIDs = extractCards winText
                  chosenIDs = extractCards $ head chosenText
               in (winIDs, chosenIDs)
            _ -> error "unknown cards"
          winCnt = length $ filter (`elem` win) chosen
       in (card, winCnt)
    _ -> error "unknown line"

-- CardIdx, WinCnt, CardCnt -> Mask
calMask :: Int -> Int -> Int -> [Int]
calMask cardIdx winCnt cardCnt =
  let mask' = replicate cardCnt 0
      mask'' = take (cardIdx - 1) mask' ++ replicate (winCnt + 1) 1 ++ drop (cardIdx + winCnt) mask'
   in assert' (length mask'' == cardCnt) mask''

-- f(acc, idx, item) -> acc -> list -> acc
foldlWithIndex' :: (a -> Int -> b -> a) -> a -> [b] -> a
foldlWithIndex' f acc xs = fst $ foldl' (\(acc', idx) x -> (f acc' idx x, idx + 1)) (acc, 0) xs

solve1 :: Text -> IO ()
solve1 input =
  let linesOfInput = T.lines input
      results = map processLine linesOfInput
   in print $ sum $ map (\x -> let y = snd x in if y == 0 then 0 else 2 ^ (y - 1) :: Int) results

solve2 :: Text -> IO ()
solve2 input =
  let linesOfInput = T.lines input
      results = map processLine linesOfInput
      cardNum = length results
      masks = map (\x -> uncurry calMask x cardNum) results
   in print
        $ fst
        $ foldlWithIndex'
          ( \acc idx x ->
              let -- acc: (currentScore, currentMask)
                  current = (snd acc !! idx) + 1
                  nextMask = zipWith (+) (snd acc) (map (* current) x)
                  nextScore = fst acc + current
               in (nextScore, nextMask)
          )
          (0, replicate cardNum 0)
          masks
