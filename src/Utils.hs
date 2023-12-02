module Utils (stringToUnsigned) where

import RIO
import Data.Text.Read (decimal)

-- throws error when it could not parse
stringToUnsigned :: Text -> Int
stringToUnsigned str = either (error . show) fst (decimal str)
