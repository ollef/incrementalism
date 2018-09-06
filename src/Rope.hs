{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
module Rope where

import Protolude hiding (uncons, unsnoc)

import Data.String
import qualified Data.Text as Text

import Data.SplayTree(SplayTree, Measured(measure))
import qualified Data.SplayTree as SplayTree
import Delta
import Text
import TreeFold

infixr 5 :<
infixl 5 :>

newtype Rope = Rope (SplayTree Delta Text)
  deriving (Show, Semigroup, Monoid)

instance Eq Rope where
  (==) = (==) `on` toText

instance Ord Rope where
  compare = comparing toText

instance IsString Rope where
  fromString = fromText . fromString

instance Measured Delta Rope where
  measure (Rope r) = measure r

chunkLength :: Int
chunkLength = 10

fromText :: Text -> Rope
fromText t
  | Text.null t = mempty
  | otherwise
    = Rope
    $ treeFoldMap SplayTree.singleton
    $ chunks16Of chunkLength t

singleton :: Char -> Rope
singleton = fromText . Text.singleton

toText :: Rope -> Text
toText (Rope r) = Text.concat $ toList r

fromShortText :: Text -> Rope
fromShortText t
  | Text.null t = mempty
  | otherwise = Rope $ SplayTree.singleton t

-- | Split the rope at the nth code unit (not character)
splitAt :: Delta -> Rope -> (Rope, Rope)
splitAt n (Rope r) = case SplayTree.split (> n) r of
  SplayTree.Outside
    | n < 0 -> (mempty, Rope r)
    | otherwise -> (Rope r, mempty)
  SplayTree.Inside pre t post -> (Rope pre <> fromShortText pret, fromShortText postt <> Rope post)
    where
      Delta n' = n - SplayTree.measure pre
      (pret, postt) = split16At n' t

uncons :: Rope -> Maybe (Char, Rope)
uncons (Rope (t SplayTree.:< r)) = case Text.uncons t of
  Nothing -> uncons $ Rope r
  Just (c, t') -> Just (c, Rope $ t' SplayTree.:< r)
uncons (Rope _) = Nothing

pattern (:<) :: Char -> Rope -> Rope
pattern a :< as <- (uncons -> Just (a, as))
  where
    a :< as = singleton a <> as

{-# INLINE unsnoc #-}
unsnoc :: Rope -> Maybe (Rope, Char)
unsnoc (Rope (r SplayTree.:> t)) = case Text.unsnoc t of
  Nothing -> unsnoc $ Rope r
  Just (t', c) -> Just (Rope $ r SplayTree.:> t', c)
unsnoc _ = Nothing

pattern (:>) :: Rope -> Char -> Rope
pattern as :> a <- (unsnoc -> Just (as, a))
  where
    as :> a = as <> singleton a

head :: Rope -> Maybe Char
head = fmap fst . uncons

last :: Rope -> Maybe Char
last = fmap snd . unsnoc

null :: Rope -> Bool
null r = measure r == Delta 0

-------------------------------------------------------------------------------
-- * Breaking by predicate

-- | @span f r = (takeWhile f r, dropWhile f r)@
span :: (Char -> Bool) -> Rope -> (Rope, Rope)
span f (Rope r) = case SplayTree.uncons r of
  Nothing -> (mempty, mempty)
  Just (t, r')
    | Text.null postt -> (Rope (SplayTree.singleton t) <> pre', post')
    | otherwise -> (fromShortText pret, fromShortText postt <> Rope r')
    where
      (pret, postt) = Text.span f t
      (pre', post') = Rope.span f $ Rope r'

-- | @break f = span (not . f)@
break :: (Char -> Bool) -> Rope -> (Rope, Rope)
break f = Rope.span (not . f)

-- | @takeWhile f = fst . span f@
takeWhile :: (Char -> Bool) -> Rope -> Rope
takeWhile f = fst . Rope.span f

-- | @dropWhile f = snd . span f@
dropWhile :: (Char -> Bool) -> Rope -> Rope
dropWhile f = snd . Rope.span f
