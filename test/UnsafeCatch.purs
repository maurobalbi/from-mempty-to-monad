module Test.UnsafeCatch where

import Data.Unit (Unit)
import Effect (Effect)

-- Highly unsafe hack to enable catching exceptions thrown in pure code.
-- If you're new to functional programming, take the blue pill.
-- Otherwise, if you know of a better way to do this, please opan an issue.
foreign import unsafeCatch :: (String -> Effect Unit) -> (Unit -> Effect Unit) ->  Effect Unit