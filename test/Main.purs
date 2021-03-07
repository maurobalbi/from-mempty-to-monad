module Test.Main where

import FMTM.Functor
import Prelude hiding ((<$>))

import Data.List (List)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (catchException, message)
import Test.QuickCheck (printSummary, class Testable, Seed, Result, checkResults, mkSeed, quickCheck, quickCheck', quickCheckPure', (/==), (/=?), (<=?), (<?), (<?>), (===), (==?), (>=?), (>?))

runTest :: String -> Effect Unit -> Effect Unit
runTest testName test = do
  log $ "=== Running test:  " <> testName
  test
  log $ "=== Test finished: " <> testName <> "\n\n"

main :: Effect Unit
main = do
  runTest "Pass - exhaustive" passingTest_exhaustive
  log $ printSummary $ checkResults quickCheck_functor

printErrorMessage :: Effect Unit -> Effect Unit
printErrorMessage test = catchException (\error -> log $ message error) test

passingTest_exhaustive :: Effect Unit
passingTest_exhaustive = quickCheck_boolean (\b -> (b && true) == b)

quickCheck_boolean :: forall a. Testable a => (Boolean -> a) -> Effect Unit
quickCheck_boolean test = quickCheck test

quickCheck_functor :: List (Tuple Seed Result)
quickCheck_functor = quickCheckPure' (mkSeed 1) 100 $ \n q -> ((_ + 1) <$> Box n) == (Box $ n + q)
