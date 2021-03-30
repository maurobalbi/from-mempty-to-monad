module Test.UnsafeCatch where

import Data.Unit (Unit)
import Effect (Effect)

foreign import unsafeCatch :: (String -> Effect Unit) -> (Unit -> Effect Unit) ->  Effect Unit