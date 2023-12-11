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
import Options.Applicative
import RIO
import Prelude (putStrLn)

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
    (11, [Day11.solve1, Day11.solve2])
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
            putStrLn ("Day " ++ show d)
            mapM_ (\f -> f contents) x
        )
        dayToModule
