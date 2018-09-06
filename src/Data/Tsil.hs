{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language NoImplicitPrelude #-}
module Data.Tsil where

import Protolude hiding (filter)

infixl 5 :>

data Tsil a
  = Nil
  | Tsil a :> a
  deriving (Eq, Functor, Ord, Show, Traversable)

instance Semigroup (Tsil a) where
  xs <> Nil = xs
  xs <> (ys :> y) = (xs <> ys) :> y

instance Monoid (Tsil a) where
  mempty = Nil
  mappend = (<>)

instance Applicative Tsil where
  pure = (Nil :>)
  (<*>) = ap

instance Alternative Tsil where
  empty = Nil
  (<|>) = mappend

instance Monad Tsil where
  return = pure
  Nil >>= _ = Nil
  xs :> x >>= f = (xs >>= f) <> f x

fromList :: [a] -> Tsil a
fromList = foldr (flip (:>)) Nil . reverse

instance Foldable Tsil where
  foldMap _ Nil = mempty
  foldMap f (xs :> x) = foldMap f xs `mappend` f x

  toList = reverse . go
    where
      go Nil = []
      go (xs :> x) = x : go xs

lookup :: Eq a => a -> Tsil (a, b) -> Maybe b
lookup _ Nil = Nothing
lookup a (as :> (a', b))
  | a == a' = Just b
  | otherwise = lookup a as

filter :: (a -> Bool) -> Tsil a -> Tsil a
filter _ Nil = Nil
filter f (xs :> x)
  | f x = filter f xs :> x
  | otherwise = filter f xs
