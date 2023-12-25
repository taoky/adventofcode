{-# OPTIONS -Wno-unused-top-binds #-}
module Main (main) where

import Day1 qualified
import Day2 qualified
import Day3 qualified
import Day4 qualified
import Day5 qualified
import Day6 qualified
import Day7 qualified
import Day8 qualified
import Day9 qualified
import Day10 qualified
import Day11 qualified
import Day12 qualified
import Day13 qualified
import Day14 qualified
import Day15 qualified
import Day16b qualified
import Day17 qualified
import Day18 qualified
import Day19 qualified
import Day20 qualified
import Day21 qualified
import Day22 qualified
import Day23 qualified
import Day24 qualified
import Day25 qualified
import Options.Applicative
import RIO
import Prelude (putStrLn)
import Data.Time (getCurrentTime, diffUTCTime)

data Options = Options
  { day :: Maybe Int
  , test :: Bool
  }

options :: Parser Options
options =
  Options
    <$> optional
      ( option
          auto
          ( long "day"
              <> short 'd'
              <> metavar "DAY"
              <> help "Day to run"
          )
      )
    <*> switch
      ( long "test"
          <> short 't'
          <> help "Use file name with .test suffix"
      )

main :: IO ()
main = solutions =<< execParser opts
  where
    opts =
      info
        (options <**> helper)
        ( fullDesc
            <> progDesc "Run solutions"
            <> header "2023-hs"
        )

dayToModule :: [(Int, [Text -> IO ()])]
dayToModule =
  [ (1, [Day1.solve1, Day1.solve2]),
    (2, [Day2.solve1, Day2.solve2]),
    (3, [Day3.solve1, Day3.solve2]),
    (4, [Day4.solve1, Day4.solve2]),
    (5, [Day5.solve1, Day5.solve2]),
    (6, [Day6.solve1, Day6.solve2]),
    (7, [Day7.solve1, Day7.solve2]),
    (8, [Day8.solve1, Day8.solve2]),
    (9, [Day9.solve1, Day9.solve2]),
    (10, [Day10.solve1, Day10.solve2]),
    (11, [Day11.solve1, Day11.solve2]),
    (12, [Day12.solve1, Day12.solve2]),
    (13, [Day13.solve1, Day13.solve2]),
    (14, [Day14.solve1, Day14.solve2]),
    (15, [Day15.solve1, Day15.solve2]),
    (16, [Day16b.solve1, Day16b.solve2]),
    (17, [Day17.solve1, Day17.solve2]),
    (18, [Day18.solve1, Day18.solve2]),
    (19, [Day19.solve1, Day19.solve2]),
    (20, [Day20.solve1, Day20.solve2]),
    (21, [Day21.solve1, Day21.solve2]),
    (22, [Day22.solve1, Day22.solve2]),
    (23, [Day23.solve1, Day23.solve2]),
    (24, [Day24.solve1, Day24.solve2]),
    (25, [Day25.solve1, Day25.solve2])
  ]

getFileName :: Int -> Bool -> String
getFileName day test = "input/day" ++ show day ++ (if test then ".test" else "")

solutions :: Options -> IO ()
solutions (Options day test) = do
  case day of
    Just d -> do
      let m = lookup d dayToModule
      case m of
        Just x -> do
          contents <- readFileUtf8 (getFileName d test)
          mapM_ (\f -> f contents) x
        Nothing -> putStrLn "No such day"
    Nothing -> do
      -- Run all
      putStrLn "Run all"
      mapM_
        ( \(d, x) -> do
            contents <- readFileUtf8 (getFileName d test)
            putStrLn ("========== Day " ++ show d ++ " ==========")
            startTime <- getCurrentTime
            res <- mapM_ (\f -> f contents) x
            endTime <- getCurrentTime
            putStrLn ("(" ++ show (diffUTCTime endTime startTime) ++ ")")
            pure res
        )
        dayToModule
