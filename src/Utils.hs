module Utils (stringToUnsigned, charListToUnsigned) where

import RIO
import Data.Text.Read (decimal)
import RIO.Text qualified as T

-- throws error when it could not parse
stringToUnsigned :: Text -> Int
stringToUnsigned str = either (error . show) fst (decimal str)

charListToUnsigned :: [Char] -> Int
charListToUnsigned = stringToUnsigned . T.pack
