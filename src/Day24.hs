module Day24 (solve1, solve2) where

import Data.Attoparsec.Text qualified as P
import RIO
import RIO.List qualified as L
import Data.SBV qualified as SBV
import Prelude (print)
import Data.Maybe (fromJust)

data HailStone = HailStone {x :: Int, y :: Int, z :: Int, vx :: Int, vy :: Int, vz :: Int} deriving (Show, Eq)

data CrossStatus = Cross (Double, Double) | CrossInPast (Double, Double) | NoCross deriving (Show, Eq)

parser :: P.Parser [HailStone]
parser = do
  res <- P.many' $ do
    x <- P.decimal
    void "," <* P.skipSpace
    y <- P.decimal
    void "," <* P.skipSpace
    z <- P.decimal
    void " @" <* P.skipSpace
    vx <- P.signed P.decimal
    void "," <* P.skipSpace
    vy <- P.signed P.decimal
    void "," <* P.skipSpace
    vz <- P.signed P.decimal
    P.endOfLine
    pure $ HailStone {x, y, z, vx, vy, vz}
  P.endOfInput
  pure res

cross1 :: HailStone -> HailStone -> CrossStatus
cross1 HailStone {x = x1, y = y1, vx = vx1, vy = vy1} HailStone {x = x2, y = y2, vx = vx2, vy = vy2} =
  let x1_ = fromIntegral x1
      y1_ = fromIntegral y1
      x2_ = fromIntegral x2
      y2_ = fromIntegral y2
      vx1_ = fromIntegral vx1
      vy1_ = fromIntegral vy1
      vx2_ = fromIntegral vx2
      vy2_ = fromIntegral vy2
      polt1Slope :: Double = vy1_ / vx1_
      polt2Slope :: Double = vy2_ / vx2_
      crossX = (y2_ - vy2_ * x2_ / vx2_ - y1_ + vy1_ * x1_ / vx1_) / (polt1Slope - polt2Slope)
      crossY = y1_ + polt1Slope * (crossX - x1_)
      t1 = (crossX - x1_) / vx1_
      t2 = (crossX - x2_) / vx2_
   in if polt1Slope == polt2Slope
        then NoCross
        else
          if t1 < 0 || t2 < 0
            then CrossInPast (crossX, crossY)
            else Cross (crossX, crossY)

pairs :: [a] -> [(a, a)]
pairs lst = [(x, y) | (x : ys) <- L.tails lst, y <- ys]

-- sReal is much faster than sInt64
sat :: [HailStone] -> IO SBV.SatResult
sat hailstones = SBV.sat $ do
    x <- SBV.sReal "x"
    y <- SBV.sReal "y"
    z <- SBV.sReal "z"
    vx <- SBV.sReal "vx"
    vy <- SBV.sReal "vy"
    vz <- SBV.sReal "vz"
    for_ hailstones $ \HailStone {x = x', y = y', z = z', vx = vx', vy = vy', vz = vz'} -> do
      t <- SBV.sReal "t"
      SBV.constrain $ t SBV..> 0
      SBV.constrain $ x + vx * t SBV..== fromIntegral x' + t * fromIntegral vx'
      SBV.constrain $ y + vy * t SBV..== fromIntegral y' + t * fromIntegral vy'
      SBV.constrain $ z + vz * t SBV..== fromIntegral z' + t * fromIntegral vz'

solve1 :: Text -> IO ()
solve1 input =
  let hailStones = case P.parseOnly parser input of
        Left err -> error err
        Right res -> res
      least = 200000000000000
      most = 400000000000000
   in print
        $ length
        $ filter
          ( \case
              Cross (x, y) -> x >= least && x <= most && y >= least && y <= most
              _ -> False
          )
        $ map (uncurry cross1)
        $ pairs hailStones

solve2 :: Text -> IO ()
solve2 input = do
    let hailStones = case P.parseOnly parser input of
            Left err -> error err
            Right res -> res
    result <- sat hailStones
    let x :: SBV.AlgReal = fromJust $ SBV.getModelValue "x" result
        y = fromJust $ SBV.getModelValue "y" result
        z = fromJust $ SBV.getModelValue "z" result
    print $ (round :: Double -> Int) $ fromRational $ toRational (x + y + z)
