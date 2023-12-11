-- Part 2 algorithm from https://github.com/SmartHypercube/aoc2023-haskell/blob/master/src/day10.hs
module Day10 (solve1, solve2) where

import RIO
import RIO.List.Partial (head, (!!))
import RIO.Text qualified as T
import Safe (atMay)
import Utils
import Prelude (print)

data Pipe = UpDown | LeftRight | UpRight | UpLeft | DownRight | DownLeft | None | Unknown deriving (Show, Eq)

data Direction = Up | Dow | Lef | Righ deriving (Show, Eq)

dx :: Direction -> Int
dx Up = -1
dx Dow = 1
dx Lef = 0
dx Righ = 0

dy :: Direction -> Int
dy Up = 0
dy Dow = 0
dy Lef = -1
dy Righ = 1

rev :: Direction -> Direction
rev Up = Dow
rev Dow = Up
rev Lef = Righ
rev Righ = Lef

allowed :: Pipe -> [Direction]
allowed UpDown = [Up, Dow]
allowed LeftRight = [Lef, Righ]
allowed UpRight = [Up, Righ]
allowed UpLeft = [Up, Lef]
allowed DownRight = [Dow, Righ]
allowed DownLeft = [Dow, Lef]
allowed None = []
allowed Unknown = [Up, Dow, Lef, Righ]

parse :: Text -> [[Pipe]]
parse input =
  let chars = (map T.unpack $ T.lines input)
   in map
        ( map
            ( \case
                '|' -> UpDown
                '-' -> LeftRight
                'L' -> UpRight
                'J' -> UpLeft
                '7' -> DownLeft
                'F' -> DownRight
                '.' -> None
                'S' -> Unknown
                _ -> error "unexpected char"
            )
        )
        chars

-- (x, y): target position; dir: the direction that we come from
-- also returns false if the target position is out of bound
isConnected :: [[Pipe]] -> (Int, Int) -> Direction -> Bool
isConnected pipeMap (x, y) dir =
  let pipe = atMay pipeMap x >>= (`atMay` y)
   in case pipe of
        Nothing -> False
        Just Unknown -> True
        Just pipe' ->
          case dir of
            Up -> pipe' == UpDown || pipe' == DownRight || pipe' == DownLeft
            Dow -> pipe' == UpDown || pipe' == UpRight || pipe' == UpLeft
            Lef -> pipe' == LeftRight || pipe' == UpRight || pipe' == DownRight
            Righ -> pipe' == LeftRight || pipe' == UpLeft || pipe' == DownLeft

-- start = Pipe::Unknown
findStart :: [[Pipe]] -> (Int, Int)
findStart pipeMap =
  let unknowns = filter (\(x, y) -> pipeMap !! x !! y == Unknown) [(x, y) | x <- [0 .. length pipeMap - 1], y <- [0 .. length (head pipeMap) - 1]]
   in assert' (length unknowns == 1) $ head unknowns

-- dir: the direction that we come from
-- return steps to walk until reach "Unknown" (Start)
walk :: [[Pipe]] -> (Int, Int) -> Maybe Direction -> Maybe (Int, Int)
walk pipeMap (x, y) dir =
  let pipe = pipeMap !! x !! y
      targets = [((x + dx dir', y + dy dir'), dir') | dir' <- allowed pipe, (\d -> (Just (rev d) /= dir) && isConnected pipeMap (x + dx dir', y + dy dir') d) dir']
      -- from https://www.reddit.com/r/adventofcode/comments/18evyu9/comment/kcqtow6/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
      -- > Part 2 using one of my favorite facts from graphics engineering:
      -- lets say you have an enclosed shape, and you want to color every pixel inside of it.
      -- How do you know if a given pixel is inside the shape or not?
      -- Well, it turns out: if you shoot a ray in any direction from the pixel
      -- and it crosses the boundary an odd number of times, it's inside.
      -- if it crosses an even number of times, it's outside.
      -- Works for all enclosed shapes, even self-intersecting and non-convex ones.
      -- --------------------------------------------------------------------------
      -- Or another explanation:
      -- Here we "paint color" towards left when we are going up/down
      -- Going up will paint an area, and going down will "unpaint" this
      -- This could easily be implemented with a counter + abs
      -- After we got area and circumference, just use Pick's theorem
      delta = case dir of
        Just Up -> y
        Just Dow -> -y
        _ -> 0
      -- iterate over targets and return the first Just
      -- if no Just, return Nothing
      walkInner :: [((Int, Int), Direction)] -> Maybe (Int, Int)
      walkInner [] = Nothing
      walkInner (((x', y'), dir') : xs) =
        let -- pipe' = trace (tshow (x, y, x', y', pipeMap !! x' !! y')) $ pipeMap !! x' !! y'
            pipe' = pipeMap !! x' !! y'
         in case pipe' of
              Unknown -> Just (1, abs delta)
              None -> Nothing
              _ -> case walk pipeMap (x', y') (Just dir') of
                Just (steps, area) -> Just (steps + 1, abs $ area + delta)
                Nothing -> walkInner xs
   in walkInner targets

solve1 :: Text -> IO ()
solve1 input =
  let inputMap = parse input
      start = findStart inputMap
   in print $ div (fst $ fromMaybe (error "unexpected result") (walk inputMap start Nothing)) 2

solve2 :: Text -> IO ()
solve2 input =
  let inputMap = parse input
      start = findStart inputMap
      (circ, area) = fromMaybe (error "unexpected result") (walk inputMap start Nothing)
   in -- Pick's theorem
      print $ area - div circ 2 + 1
