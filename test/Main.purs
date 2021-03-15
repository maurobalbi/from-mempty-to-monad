module Test.Main where

import Prelude

import Effect (Effect)
import Test.Framework (TestTree, test, testGroup)
import Test.ListTest (listTest)

testAll :: TestTree 
testAll = testGroup "All" [
  listTest
]

main :: Effect Unit
main = test testAll

