module Test.Main where

import FMTM.Functor
import Prelude hiding ((<$>))

import Data.List (List)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (catchException, message, error, stack)
import Effect.Exception.Unsafe (unsafeThrow, unsafeThrowException)
import Test.QuickCheck (class Testable, Result, Seed, checkResults, mkSeed, printSummary, quickCheck, quickCheck', quickCheckPure', (/==), (/=?), (<=?), (<?), (<?>), (===), (==?), (>=?), (>?))
import Unsafe.Coerce (unsafeCoerce)

evaluate :: forall a. (Unit -> a) -> Effect a
evaluate = unsafeCoerce

runTest :: String -> Effect Unit -> Effect Unit
runTest testName test = do
  log $ "=== Running test:  " <> testName
  test
  log $ "=== Test finished: " <> testName <> "\n\n"

main :: Effect Unit
main = printErrorMessage $ printErrorMessage $ functorTest

functorTest :: Effect Unit
functorTest = quickCheck \n -> ((_ + 1) <$> Box (n)) == Box (n + 2)

printErrorMessage :: Effect Unit -> Effect Unit
printErrorMessage test = catchException (\e -> log $ message e) do 
    functorTest
