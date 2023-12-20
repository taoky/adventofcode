module Day20 (solve1, solve2) where

import Data.Attoparsec.Text qualified as P
import Data.Char qualified as Char
import RIO
import RIO.HashMap qualified as HM
import RIO.HashMap.Partial qualified as HM'
import Prelude (print)

data ElementType = Broadcaster | FlipFlop Bool | Conjunction (HashMap Text Bool) deriving (Show, Eq)

data Element = Element
  { eleType :: ElementType,
    name :: Text, -- for debugging purpose
    outputs :: [Text]
  }
  deriving (Show, Eq)

data Signal = Signal
  { sender :: Text,
    receiver :: Text,
    value :: Bool -- High -> True, Low -> False
  }
  deriving (Show, Eq)

getInputs :: HashMap Text Element -> Text -> [Element]
getInputs elements name =
  filter (\x -> name `elem` outputs x) $ HM.elems elements

parser :: P.Parser (HashMap Text Element)
parser = do
  res <- P.many' $ do
    eleType <-
      P.choice
        [ Broadcaster <$ "broadcaster",
          FlipFlop False <$ "%",
          Conjunction HM.empty <$ "&"
        ]
    name <- P.takeTill (== ' ')
    void " -> "
    outputs <- P.sepBy1 (P.takeWhile1 Char.isAlpha) (P.string ", ")
    let name' = if name == "" then "broadcaster" else name
    P.endOfLine
    pure $ HM.singleton name' (Element {eleType, outputs, name = name'})
  let res' = mconcat res
      updateConjunction conj =
        let -- get all inputs of current conj
            inputs = getInputs res' $ name conj
         in conj {eleType = Conjunction $ HM.fromList [(name x, False) | x <- inputs]}
   in pure
        $ HM.map
          ( \x -> case eleType x of
              Conjunction _ -> updateConjunction x
              _ -> x
          )
          res'

-- too ugly
simulate :: HashMap Text Element -> [Signal] -> Maybe Text -> (HashMap Text Element, [Signal], (Int, Int), Bool)
simulate elements signals matchSender =
  let go elements' signal =
        let receiverName = receiver signal
            senderName = sender signal
            signalValue = value signal
            receiverElement = elements' HM'.! receiverName
            updatedReceiver = case eleType receiverElement of
              Broadcaster -> receiverElement
              FlipFlop x -> if value signal then receiverElement else receiverElement {eleType = FlipFlop $ not x}
              Conjunction h -> receiverElement {eleType = Conjunction $ HM.insert senderName signalValue h}
            newSignals = case eleType updatedReceiver of
              Broadcaster -> [Signal {sender = receiverName, receiver = x, value = signalValue} | x <- outputs receiverElement]
              FlipFlop x -> if value signal then [] else [Signal {sender = receiverName, receiver = y, value = x} | y <- outputs receiverElement]
              Conjunction h ->
                if all snd $ HM.toList h
                  then [Signal {sender = receiverName, receiver = x, value = False} | x <- outputs receiverElement]
                  else [Signal {sender = receiverName, receiver = x, value = True} | x <- outputs receiverElement]
            hasMatchSender = case matchSender of
              Nothing -> False
              Just x -> x == senderName && signalValue
         in if not (HM.member receiverName elements')
              then (elements', [], hasMatchSender)
              else (HM.insert receiverName updatedReceiver elements', newSignals, hasMatchSender)
      go' elements' [] x = (elements', [], x, False)
      go' elements' (x : xs) (y1, y2) =
        let (elements'', signals', hasMatchSender) = go elements' x
            res = go' elements'' (xs <> signals') $ if value x then (y1 + 1, y2) else (y1, y2 + 1)
         in if hasMatchSender then (\(a, b, c, _) -> (a, b, c, True)) res else res
   in go' elements signals (0, 0)

applySimulate :: HashMap Text Element -> [Signal] -> Int -> (Int, Int)
applySimulate _ _ 0 = (0, 0)
applySimulate elements signals n =
  let (elements', _, (x, y), _) = simulate elements signals Nothing
      (x', y') = applySimulate elements' signals (n - 1)
   in (x + x', y + y')

-- getConjuctionState :: HashMap Text Element -> Text -> Bool
-- getConjuctionState elements label =
--   let ele = eleType $ elements HM'.! label
--    in case ele of
--         Conjunction h -> all snd $ HM.toList h
--         _ -> False

-- Directly compute to rx is TOO SLOW
applySimulate2 :: HashMap Text Element -> [Signal] -> Text -> Int
applySimulate2 elements signals label =
  -- shouldStop will be true when label sends a True (High) signal
  let (elements', _, _, shouldStop) = simulate elements signals (Just label)
   in -- shouldStop' = not $ getConjuctionState elements' label
      if shouldStop then 1 else 1 + applySimulate2 elements' signals label

solve1 :: Text -> IO ()
solve1 input =
  let input' = case P.parseOnly parser input of
        Left err -> error err
        Right x -> x
      signalQueue = [Signal {sender = "button", receiver = "broadcaster", value = False}]
   in -- print $ simulate input' signalQueue
      print $ uncurry (*) $ applySimulate input' signalQueue 1000

-- Assuming that rx is connected to a conjunction, and then it is connected to 4 conjunctions
-- https://www.reddit.com/media?url=https%3A%2F%2Fi.redd.it%2F2023-day-20-visualization-of-the-input-couldnt-solve-part-2-v0-x6u0v3t9ee7c1.png%3Fs%3D5580b3d46217b1865624e48c4c3a878124599ccb
solve2 :: Text -> IO ()
solve2 input =
  let input' = case P.parseOnly parser input of
        Left err -> error err
        Right x -> x
      signalQueue = [Signal {sender = "button", receiver = "broadcaster", value = False}]
      rxInputs = map name $ concatMap (getInputs input' . name) $ getInputs input' "rx"
   in print $ foldl' lcm 1 $ map (applySimulate2 input' signalQueue) rxInputs
