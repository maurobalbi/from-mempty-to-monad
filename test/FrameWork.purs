module Test.Framework(
  notImplementedError
  , testGroup
  , testProperty
  , testCase
  , test
  , TestTreeF
  , TestTree
  , module Lazy
) where

import Prelude

import Data.Array.NonEmpty (intercalate)
import Data.Either (Either(..))
import Data.Foldable (sequence_)
import Data.Functor.Mu (Mu)
import Data.Generic.Rep (class Generic)
import Data.Lazy (Lazy, defer, force)
import Data.Lazy (Lazy, defer, force) as Lazy
import Data.List (List, length)
import Data.Newtype (overF)
import Data.Semiring.Generic (genericOne)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (catchException, message, throw, try)
import Math (e)
import Matryoshka (class Corecursive, class Recursive, Algebra, cata, embed)
import Test.Catch (unsafeCatch)
import Test.Main (printErrorMessage)
import Test.QuickCheck (Result(..))
import Test.QuickCheck as P
import Test.QuickCheck.Gen (sample)
import Unsafe.Coerce (unsafeCoerce)

notImplementedError :: forall a. String -> a
notImplementedError s =  unsafeCoerce $ throw $ "Not implemented: " <> s

evaluate :: forall a. (Unit -> a) -> a
evaluate = unsafeCoerce

data TestTreeF a
  = Single String (Lazy P.Result)
  | Tree String (Array a)

derive instance functorTestTreeF :: Functor TestTreeF

single :: forall t. Corecursive t (TestTreeF) => String -> (Lazy P.Result) -> t
single s r = embed $ Single s r

tree :: forall t. Corecursive t (TestTreeF) => String -> Array t -> t
tree s t = embed $ Tree s t

type TestTree = Mu TestTreeF

testGroup :: String -> Array TestTree -> TestTree
testGroup = tree

testCase :: String -> (Unit -> P.Result) -> TestTree
testCase s r = single s $ defer r 

testProperty :: forall a. P.Testable a => String -> a -> TestTree
testProperty label a = single label $ go a
  where
    go :: P.Testable a => a -> (Lazy P.Result)
    go prop = defer $ \_ -> case P.checkResults $ P.quickCheckPure' (P.mkSeed 1) 100 prop of 
      {failures, successes, total}
        | successes == total -> P.Success
        | otherwise -> P.Failed $ (show $ length failures) <> "/" <> show total <> " tests failed."

testF :: Algebra (TestTreeF) (Effect Unit)
testF (Single s res ) = log $ unsafeCatch (\_ -> formatResult $ force res) formatError
  where 
    formatError e = "FAILED: '" <> s <> "'" <> "\n  " <> e
    formatResult (Failed e) = formatError e
    formatResult Success = "PASSED: '" <> s <> "'"
testF (Tree s as) = do
  log s 
  sequence_ as

test :: TestTree -> Effect Unit
test = cata testF
