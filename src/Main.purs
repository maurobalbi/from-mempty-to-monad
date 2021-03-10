module Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (try, catchException, error, message, throwException)
import Effect.Exception.Unsafe (unsafeThrowException)
import Unsafe.Coerce (unsafeCoerce)


evaluate :: forall a. (Unit -> a) -> Effect a
evaluate = unsafeCoerce

main :: Effect Unit
main = catchException (\e -> log $ message e)  do 
    evaluate (\_ ->  unsafeThrowException $ error "test1")
