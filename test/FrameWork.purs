module Test.Framework(
  notImplementedError
  , testGroup
  , testProperty
  , testCase
  , test
  , TestTree
  , module Lazy
) where

import Prelude

import Control.Error.Util (bool)
import Data.Array (intercalate)
import Data.Foldable (traverse_)
import Data.Lazy (Lazy, defer, force)
import Data.Lazy (Lazy, defer, force) as Lazy
import Data.List (length)
import Data.String (null)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Test.UnsafeCatch (unsafeCatch)
import Test.QuickCheck (Result(..))
import Test.QuickCheck as P
import Unsafe.Coerce (unsafeCoerce)

notImplementedError :: forall a. String -> a
notImplementedError s =  unsafeCoerce $ throw $ "Not implemented: " <> s

data TestTree
  = Single String (Lazy P.Result)
  | Tree String (Array TestTree)


testGroup :: String -> Array TestTree -> TestTree
testGroup = Tree

testCase :: String -> (Unit -> P.Result) -> TestTree
testCase s r= Single s $ defer r 

testProperty :: forall a. P.Testable a => String -> a -> TestTree
testProperty label a = Single label $ go a
  where
    go :: P.Testable a => a -> (Lazy P.Result)
    go prop = defer $ \_ -> case P.checkResults $ P.quickCheckPure' (P.mkSeed 1) 100 prop of 
      {failures, successes, total}
        | successes == total -> P.Success
        | otherwise -> P.Failed $ (show $ length failures) <> "/" <> show total <> " tests failed."

test :: TestTree -> Effect Unit
test  = go ""
  where 
    qualifiedName s s' = bool (intercalate "." [s, s']) s' (null s)
    
    go s (Single s' a) =
      let quote x = "'" <> x <> "'"
          qName = quote (qualifiedName s s')

          printFailure e = do
            log ""
            log $ "FAILED: " <> qName 
            log $ "  " <> e
          printResult (Failed e) = printFailure e
          printResult Success  = log $ "PASSED: " <> qName
        in 
          unsafeCatch printFailure $ (\_ -> printResult $ force a)
    go s (Tree s' ts) = traverse_ (go (qualifiedName s s')) ts


