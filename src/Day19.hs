module Day19 (solve1, solve2) where

import Data.Attoparsec.Text qualified as P
import Data.Char qualified as Char
import RIO
import RIO.HashMap qualified as HM
import RIO.HashMap.Partial qualified as HM'
import Prelude (print)

data Comparison = LessThan | MoreThan | LessEqualThan | MoreEqualThan deriving (Show, Eq)

data Condition = Condition
  { variableName :: Text,
    comparison :: Comparison,
    value :: Int,
    target :: Text
  }
  deriving (Show, Eq)

data Rule = Rule
  { conditions :: [Condition],
    final :: Text
  }
  deriving (Show, Eq)

data InputPart = InputPart
  { x :: Int,
    m :: Int,
    a :: Int,
    s :: Int
  }
  deriving (Show, Eq)

-- (a, b) => [a, b]
data InputPartRange = InputPartRange
  { x :: (Int, Int),
    m :: (Int, Int),
    a :: (Int, Int),
    s :: (Int, Int)
  }
  deriving (Show, Eq)

conditionParser :: P.Parser Condition
conditionParser = do
  variableName <- P.takeWhile1 Char.isAlpha
  comparison <- P.choice [P.string "<" $> LessThan, P.string ">" $> MoreThan]
  value <- P.decimal
  void ":"
  target <- P.takeWhile1 Char.isAlpha
  pure $ Condition variableName comparison value target

rulesParser :: P.Parser (HashMap Text Rule)
rulesParser = do
  res <- P.many' $ do
    name <- P.takeWhile1 (/= '{') <* P.string "{"
    conditions <- P.sepBy1 conditionParser (P.string ",")
    void ","
    final <- P.takeWhile1 (/= '}') <* P.string "}"
    P.endOfLine
    pure $ HM.singleton name (Rule conditions final)
  P.endOfLine
  pure $ mconcat res

inputParser :: P.Parser [InputPart]
inputParser = do
  P.many' $ do
    void "{x="
    x <- P.decimal
    void ",m="
    m <- P.decimal
    void ",a="
    a <- P.decimal
    void ",s="
    s <- P.decimal
    void "}"
    P.endOfLine
    pure $ InputPart x m a s

parser :: P.Parser (HashMap Text Rule, [InputPart])
parser = do
  rules <- rulesParser
  input <- inputParser
  P.endOfInput
  pure (rules, input)

-- Accept -> True, Reject -> False
do1 :: InputPart -> HashMap Text Rule -> Text -> Bool
do1 (InputPart x m a s) rules k =
  let rule = rules HM'.! k
      conditionMatches =
        map
          ( \(Condition variableName comparison value target) ->
              if case comparison of
                LessThan -> case variableName of
                  "x" -> x < value
                  "m" -> m < value
                  "a" -> a < value
                  "s" -> s < value
                  _ -> error "unexpected var"
                MoreThan -> case variableName of
                  "x" -> x > value
                  "m" -> m > value
                  "a" -> a > value
                  "s" -> s > value
                  _ -> error "unexpected var"
                _ -> error "unexpected comparison"
                then Just target
                else Nothing
          )
          (conditions rule)
      -- first Just in conditionMatches
      firstJust = case filter isJust conditionMatches of
        [] -> Nothing
        x' : _ -> x'
      nextRule = case firstJust of
        Nothing -> final rule
        Just target -> target
   in ((nextRule == "A") || ((nextRule /= "R") && do1 (InputPart x m a s) rules nextRule))

rating :: InputPart -> Int
rating (InputPart x m a s) = x + m + a + s

compress :: (Int, Int) -> Comparison -> Int -> Maybe (Int, Int)
compress (x, y) comparison value =
  let (x', y') = case comparison of
        LessThan -> (x, min y (value - 1))
        MoreThan -> (max x (value + 1), y)
        LessEqualThan -> (x, min y value)
        MoreEqualThan -> (max x value, y)
   in if x' <= y' then Just (x', y') else Nothing

revCondition :: Condition -> Condition
revCondition (Condition variableName comparison value target) =
  let comparison' = case comparison of
        LessThan -> MoreEqualThan
        MoreThan -> LessEqualThan
        LessEqualThan -> error "unexpected comparison"
        MoreEqualThan -> error "unexpected comparison"
   in Condition variableName comparison' value target

compressRange :: InputPartRange -> Condition -> Maybe InputPartRange
compressRange (InputPartRange x m a s) (Condition variableName comparison value _) =
  let newx = compress x comparison value
      newm = compress m comparison value
      newa = compress a comparison value
      news = compress s comparison value
      res = case variableName of
        "x" -> case newx of
          Nothing -> Nothing
          Just newx' -> Just (newx', m, a, s)
        "m" -> case newm of
          Nothing -> Nothing
          Just newm' -> Just (x, newm', a, s)
        "a" -> case newa of
          Nothing -> Nothing
          Just newa' -> Just (x, m, newa', s)
        "s" -> case news of
          Nothing -> Nothing
          Just news' -> Just (x, m, a, news')
        _ -> error "unexpected var"
   in case res of
        Nothing -> Nothing
        Just (x', m', a', s') -> Just (InputPartRange x' m' a' s')

do2 :: InputPartRange -> HashMap Text Rule -> Text -> [InputPartRange]
do2 ipr rules k =
  let rule = rules HM'.! k
      newRanges =
        foldl'
          ( \(res, currentIpr, isContinue) c ->
              if not isContinue
                then (res, currentIpr, isContinue)
                else case compressRange currentIpr c of
                  Nothing -> (res, currentIpr, isContinue)
                  Just x ->
                    let revRange = compressRange currentIpr $ revCondition c
                        newRes = res ++ [(x, target c)]
                     in case revRange of
                          Nothing -> (newRes, currentIpr, False)
                          Just x' -> (newRes, x', True)
          )
          ([], ipr, True)
          (conditions rule)
      newRanges' = (\(x, y, _) -> (x, y)) newRanges
      newRanges'' = fst newRanges' ++ [(snd newRanges', final rule)]
   in concat
        $ mapMaybe
          ( \x ->
              let r = fst x
                  t = snd x
               in if t == "R"
                    then Nothing
                    else
                      if t == "A"
                        then Just [r]
                        else Just $ do2 r rules t
          )
          newRanges''

combinations :: InputPartRange -> Int
combinations (InputPartRange x m a s) = (snd x - fst x + 1) * (snd m - fst m + 1) * (snd a - fst a + 1) * (snd s - fst s + 1)

solve1 :: Text -> IO ()
solve1 input =
  let (rules, inputParts) = case P.parseOnly parser input of
        Left err -> error err
        Right res -> res
   in print $ sum $ map rating $ filter (\x -> do1 x rules "in") inputParts

solve2 :: Text -> IO ()
solve2 input =
  let (rules, _) = case P.parseOnly parser input of
        Left err -> error err
        Right res -> res
      startRange = InputPartRange (1, 4000) (1, 4000) (1, 4000) (1, 4000)
   in print $ sum $ map combinations $ do2 startRange rules "in"
