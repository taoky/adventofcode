module Day22 (solve1, solve2) where

import Data.Attoparsec.Text qualified as P
import Data.Hashable (hashWithSalt)
import RIO
import RIO.HashSet qualified as HS
import RIO.List qualified as L
import RIO.List.Partial (head)
-- import Utils
import Prelude (print)

data Item = Item {start :: (Int, Int, Int), end :: (Int, Int, Int)} deriving (Show, Eq)

instance Hashable Item where
  hashWithSalt salt (Item (x1, y1, z1) (x2, y2, z2)) = hashWithSalt salt (x1, y1, z1, x2, y2, z2)

itemSort :: [Item] -> [Item]
itemSort = L.sortOn (\i -> let (_, _, z) = start i in z)

parser :: P.Parser [Item]
parser = do
  res <- P.many' $ do
    x1 <- P.decimal
    void ","
    y1 <- P.decimal
    void ","
    z1 <- P.decimal
    void "~"
    x2 <- P.decimal
    void ","
    y2 <- P.decimal
    void ","
    z2 <- P.decimal
    P.endOfLine
    pure $ Item (x1, y1, z1) (x2, y2, z2)
  P.endOfInput
  -- sort res by z
  let res' = itemSort res
  pure res'

isXYCovering :: Item -> Item -> Bool
isXYCovering (Item (x1, y1, _) (x2, y2, _)) (Item (x1', y1', _) (x2', y2', _)) =
  let xCovering = x1 <= x2' && x1' <= x2
      yCovering = y1 <= y2' && y1' <= y2
   in xCovering && yCovering

getSupporting :: Item -> [Item] -> [Item]
getSupporting item underItems =
  let (_, _, itemMinZ) = start item
   in filter (\i -> let (_, _, z) = end i in z == itemMinZ - 1 && isXYCovering item i) underItems

isSupporting :: Item -> [Item] -> Bool
isSupporting item underItems = not $ null $ getSupporting item underItems

getGap :: Item -> [Item] -> Int
getGap item underItems =
  let (_, _, itemMinZ) = start item
      interesting = filter (isXYCovering item) underItems
      maxZinInteresting = L.maximumMaybe $ map (\i -> let (_, _, z) = end i in z) interesting
    in case maxZinInteresting of
         Nothing -> itemMinZ - 1
         Just maxZinInteresting' -> itemMinZ - maxZinInteresting' - 1

-- lower :: Item -> Maybe Item
-- lower = lowerN 1

lowerN :: Int -> Item -> Maybe Item
lowerN 0 item = Just item
lowerN n (Item (x1, y1, z1) (x2, y2, z2)) =
  if z1 > n then Just $ Item (x1, y1, z1 - n) (x2, y2, z2 - n) else Nothing

gravity :: [Item] -> [Item]
gravity blocks =
  let gravityOnOne xs x =
        let gap = getGap x xs
        in case lowerN gap x of
             Nothing -> error "unexpected here: it should always be able to lower"
             Just lower' -> lower' : xs
        -- Slow (takes most of the time)
        -- let lower' = lower x
        --  in case lower' of
        --       Nothing -> x : xs
        --       Just lower'' -> if isSupporting x xs then x : xs else gravityOnOne xs lower''
   in foldl' gravityOnOne [] blocks

gravityWithCounter :: [Item] -> [Item] -> (Int, [Item])
gravityWithCounter inits blocks =
  let gravityOnOne (counter, xs) x =
        let z = (\(_, _, z') -> z') $ start x
         in if isSupporting x xs || z <= 1
              then (counter, x : xs)
              else (counter + 1, xs)
   in foldl' gravityOnOne (0, inits) blocks

solve1 :: Text -> IO ()
solve1 input =
  let blocks = case P.parseOnly parser input of
        Left err -> error err
        Right res -> res
      fallenBlocks = itemSort $ gravity blocks
      revFallenBlocks = L.reverse fallenBlocks
      go :: [Item] -> HS.HashSet Item -> HS.HashSet Item
      go [] hs = hs
      go (x : xs) hs =
        let supporting = getSupporting x xs
         in -- minZ = (\(_, _, z) -> z) $ start x
            -- assert' (not (null supporting) || minZ == 1) $
            if length supporting == 1
              then go xs (HS.insert (head supporting) hs)
              else go xs hs
   in print (length fallenBlocks - length (go revFallenBlocks HS.empty))

solve2 :: Text -> IO ()
solve2 input =
  let blocks = case P.parseOnly parser input of
        Left err -> error err
        Right res -> res
      fallenBlocks = itemSort $ gravity blocks
   in print $ sum $ [fst $ gravityWithCounter xs ys | (xs, ys) <- zip (L.inits fallenBlocks) (drop 1 $ L.tails fallenBlocks)]
