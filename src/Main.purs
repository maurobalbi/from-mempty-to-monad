module Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (catchException, error, message, throwException, try)
import Effect.Exception.Unsafe (unsafeThrow, unsafeThrowException)
import Test.Framework (test)
import Test.ListTest (headOrTest)
import Unsafe.Coerce (unsafeCoerce)


evaluate :: forall a. (Unit -> a) -> Effect a
evaluate = unsafeCoerce

main :: Effect Unit
main = catchException (\e -> log $ message e) $ unsafeCoerce $ \_ -> unsafeCoerce unsafeThrowException "abc" $  1


