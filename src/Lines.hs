{-# language FlexibleInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language NoImplicitPrelude #-}
module Lines where

import Protolude

import Data.String

import Data.SplayTree(SplayTree, Measured(..))
import qualified Data.SplayTree as SplayTree
import Delta
import Rope(Rope)
import qualified Rope
import Text
import TreeFold

newtype Lines = Lines (SplayTree Delta Rope)
  deriving (Show, Measured Delta)

instance Semigroup Lines where
  Lines as@(_ SplayTree.:> (_ Rope.:> '\n')) <> Lines bs = Lines $ as <> bs
  Lines (as SplayTree.:> a) <> Lines (b SplayTree.:< bs) = Lines $ as <> SplayTree.singleton (a <> b) <> bs
  Lines as@(_ SplayTree.:> _) <> _ = Lines as
  _ <> bs = bs

instance Monoid Lines where
  mempty = Lines mempty

instance IsString Lines where
  fromString = fromText . fromString

fromText :: Text -> Lines
fromText
  = Lines
  . treeFoldMap (SplayTree.singleton . Rope.fromText)
  . splitLines
