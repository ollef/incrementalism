{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
module Layout where

import Data.SplayTree (Measured(..))
import Data.Tsil
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

data Layout = Layout
  (Tsil (Indent, Rope))
  !Rope -- XXX
  [(Indent, Rope)]
  deriving (Eq, Ord, Show)
