module Test.ListTest (
    headOrTest,
    module Exports
) where

import Prelude

import FMTM.List (List(..), (:), headOr, headOr2)
import Test.Framework (TestTree, testCase, testGroup, testProperty)
import Test.Framework (test) as Exports
import Test.QuickCheck ((===))


headOrTest :: TestTree
headOrTest =
  testGroup "headOr" [
    testCase "headOr on non-empty list" $ \_ -> headOr2 3 (1 : 2 : Nil) === 1
   , testCase "headOr on empty list" $ \_ -> headOr 3 Nil === 3
   , testProperty "headOr on empty list" $ \n-> n == n + 1
  ]
