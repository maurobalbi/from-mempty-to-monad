module FMTM.List where
  
import Prelude

import Data.Array (cons, intercalate)
import Partial.Unsafe (unsafeCrashWith)
import Test.Framework (notImplementedError)
import Unsafe.Coerce (unsafeCoerce)

data List a =
    Nil
    | Cons a (List a)

infixr 6 Cons as :

derive instance eqList :: Eq a => Eq (List a)
derive instance ordList :: Ord a => Ord (List a)

instance showList :: Show a => Show (List a) where
  show Nil = "Nil"
  show xs = "(" <> intercalate " : " (show <$> toArray xs) <> " : Nil)"



-- https://www.joachim-breitner.de/blog/753-Drawing_foldl_and_foldr
foldRight :: forall a b. (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil = b
foldRight f b (h : t) = f h (foldRight f b t)

foldLeft :: forall a b. (b -> a -> b) -> b -> List a -> b
foldLeft f = go
    where
    go b = case _ of
      Nil -> b
      a : as -> go (f b a) as


headOr ::
  forall a.
  a
  -> List a
  -> a
headOr = notImplementedError "headOr"

  
headOr2 ::
  forall a.
  a
  -> List a
  -> a
headOr2 = foldRight const

toArray :: forall a. List a -> Array a
toArray (Cons a xs) = cons a $ toArray xs
toArray Nil = [] 
