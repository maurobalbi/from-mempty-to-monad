module Test.Framework where

import Prelude

import Data.Array.NonEmpty (intercalate)
import Data.Foldable (traverse_)
import Data.Functor.Mu (Mu)
import Data.Generic.Rep (class Generic)
import Data.List (List, length)
import Data.Semiring.Generic (genericOne)
import Data.Show.Generic (genericShow)
import Data.TacitString (TacitString)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (catchException, message)
import Matryoshka (class Corecursive, class Recursive, Algebra, cata, embed)
import Test.QuickCheck (checkResults)
import Test.QuickCheck as P
import Test.QuickCheck.Gen (sample)

data TestTreeF a
  = Single String Result
  | Tree String (Array a)

derive instance functorTestTreeF :: Functor TestTreeF

instance  showTestTreeF :: Show (TestTreeF TacitString) where
  show (Single s res ) = s <> show res
  show (Tree s a) = show s <> """ : """ <> show a

single :: forall a t. Corecursive t (TestTreeF) => String -> Result -> t
single s r = embed $ Single s r

tree :: forall a t. Corecursive t (TestTreeF) => String -> Array t -> t
tree s t = embed $ Tree s t

type TestTree = Mu TestTreeF

data Result
  = Failure String
  | Success

derive instance genericResult :: Generic Result _

instance showResult :: Show Result where
  show = genericShow

derive instance resultEq :: Eq Result

testGroup :: String -> Array TestTree -> TestTree
testGroup = tree

testCase :: String -> Result -> TestTree
testCase = single

testProperty :: forall a. P.Testable a => String -> a -> TestTree
testProperty label a = single label $ go a
  where
    go :: P.Testable a => a -> Result
    go prop = case checkResults $ P.quickCheckPure' (P.mkSeed 1) 100 prop of 
      {failures, successes, total}
        | successes == total -> Success
        | otherwise-> Failure $ (show $ length failures) <> "/" <> show total <> " tests failed."

testTree :: Algebra (TestTreeF) (Effect Unit)
testTree (Single s res ) = catchException (\e -> printFailure $ message e) $ printResult res
  where 
    printFailure e = do
      log $ "Failed: " <> s
      log $ "  " <> e
    printResult (Failure e) = printFailure e
    printResult Success = log $ "Passed: " <> s
testTree (Tree s as) = do
  log s
  traverse_ identity as

test :: TestTree -> Effect Unit
test = cata testTree

