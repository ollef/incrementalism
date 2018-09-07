{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
module Layout where

import Data.SplayTree (Measured(..))
-- import Data.Tsil
import Delta
import Rope (Rope)
import qualified Rope

newtype Indent = Indent Delta
  deriving (Eq, Ord, Show, Measured Delta)

instance Semigroup Indent where
  Indent x <> Indent y = Indent $ min x y

instance Monoid Indent where
  mempty = Indent mempty

indented :: Rope -> (Indent, Rope)
indented r = (Indent $ measure indents, rest)
  where
    (indents, rest) = Rope.span (== ' ') r

{-
           a
         a
       a
       result
       b
         b
           b
      <>
           c
       c
       result
       d
         d
           d

      =
        Group (b <> b <> b <> c)

-}

-- data Layout a = Layout
--   [(Indent, Rope)] -- ^ Decreasing indents
--   !(Indent a)
--   (Tsil (Indent, Rope)) -- ^ Increasing indents
--   deriving (Eq, Ord, Show)

-- instance Measured a Rope => Semigroup (Layout a) where
--   Layout pre a mid1 <> Layout mid2 b post = Layout pre (go mid1 mid2) post
--     where
--       go Nil 

-- instance Measured a Rope => Monoid (Layout a) where
--     mempty = Layout mempty mempty mempty
