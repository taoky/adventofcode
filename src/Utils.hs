module Utils (stringToUnsigned, charListToUnsigned, assert', splitTwo) where

import Data.Text qualified as DT
import Data.Text.Read (decimal)
import RIO
import RIO.List.Partial (head)
import RIO.Text qualified as T

-- throws error when it could not parse
stringToUnsigned :: (HasCallStack) => Text -> Int
stringToUnsigned str = either (\e -> error $ "Could not parse " ++ show str ++ ": " ++ show e) fst (decimal str)

charListToUnsigned :: (HasCallStack) => [Char] -> Int
charListToUnsigned = stringToUnsigned . T.pack

-- assert does not work... I don't know why so I made my own assert
assert' :: (HasCallStack) => Bool -> a -> a
assert' True x = x
assert' False _ = error "assertion failed"

splitTwo :: (HasCallStack) => Text -> Text -> (Text, Text)
splitTwo sep str =
  case DT.splitOn sep str of
    (one : two) -> assert' (length two == 1) (one, head two)
    _ -> error "splitTwo: empty"
