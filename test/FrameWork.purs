module Test.Framework where

import Prelude

data TestTree
  = Single String Result
  | Tree String (Array TestTree)

data Result
  = Failure String
  | Success

derive instance resultEq :: Eq Result
