module Test.UnsafeCatch where

import Data.Unit

foreign import unsafeCatchPurely :: (Unit -> String) -> (String -> String) -> String