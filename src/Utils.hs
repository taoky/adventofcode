module Utils (stringToUnsigned, charListToUnsigned, assert') where

import Data.Text.Read (decimal)
import RIO
import RIO.Text qualified as T

-- throws error when it could not parse
stringToUnsigned :: Text -> Int
stringToUnsigned str = either (\e -> error $ "Could not parse " ++ show str ++ ": " ++ show e) fst (decimal str)

charListToUnsigned :: [Char] -> Int
charListToUnsigned = stringToUnsigned . T.pack

-- assert does not work...
assert' :: HasCallStack => Bool -> a -> a
assert' True x = x
assert' False _ = error "assertion failed"
