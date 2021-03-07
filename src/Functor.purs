module FMTM.Functor where

import Prelude

class Functor k where
  fmap :: 
    forall a b.
    (a -> b)
    -> k a
    -> k b

infixl 4 fmap as <$>

data Box a = Box a
derive instance eqBox :: Eq a => Eq (Box a)

instance boxFunctor :: Functor Box where
  fmap f (Box a) = Box $ f a