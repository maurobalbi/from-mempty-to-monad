module Test.Catch where

import Data.Unit

foreign import unsafeCatch :: (Unit -> String) -> (String -> String) -> String