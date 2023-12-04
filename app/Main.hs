{-# OPTIONS -Wno-unused-top-binds #-}
module Main (main) where

import Day1 qualified
import Day2 qualified
import Day3 qualified
import Day4 qualified
import Options.Applicative
import RIO
import Prelude (putStrLn)

newtype Options = Options
  { day :: Maybe Int
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
    (4, [Day4.solve1, Day4.solve2])
  ]

solutions :: Options -> IO ()
solutions (Options day) = do
  case day of
    Just d -> do
      let m = lookup d dayToModule
      case m of
        Just x -> do
          contents <- readFileUtf8 ("input/day" ++ show d)
          mapM_ (\f -> f contents) x
        Nothing -> putStrLn "No such day"
    Nothing -> do
      -- Run all
      putStrLn "Run all"
      mapM_
        ( \(d, x) -> do
            contents <- readFileUtf8 ("input/day" ++ show d)
            mapM_ (\f -> f contents) x
        )
        dayToModule
