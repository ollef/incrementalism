{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
module Delta where

import Protolude

import qualified Data.Text.Unsafe as Text

import Data.SplayTree(Measured(measure))

newtype Delta = Delta Int -- ^ utf-16 codeunits
  deriving (Eq, Ord, Show, Num)

instance Semigroup Delta where
  Delta m <> Delta n = Delta $ m + n

instance Monoid Delta where
  mempty = Delta 0

instance Measured Delta Text where
  measure = Delta . Text.lengthWord16

instance Measured Delta Delta where
  measure = id
