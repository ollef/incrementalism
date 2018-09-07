{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
module Lex where

import Protolude

import qualified Data.Text as Text

import Data.SplayTree (SplayTree, Measured(measure))
import qualified Data.SplayTree as SplayTree
import Delta
import Rope (Rope)
import qualified Rope
import TreeFold

data Token
  = NotStringLiteral !Rope
  | StringLiteral !Rope -- ^ ".*"
  deriving (Eq, Ord, Show)

instance Measured Delta Token where
  measure (NotStringLiteral r) = measure r
  measure (StringLiteral r) = measure r + 2 -- Two quotation marks

type Tokens = SplayTree Delta Token

data EvenInsideQuote
  = NoQuotes !Rope
  | SomeQuotes !Rope !Tokens !Rope
  deriving (Eq, Ord, Show)

instance Semigroup EvenInsideQuote where
  NoQuotes r1 <> NoQuotes r2 = NoQuotes $ r1 <> r2
  NoQuotes r1 <> SomeQuotes r2 ts r3 = SomeQuotes (r1 <> r2) ts r3
  SomeQuotes r1 ts r2 <> NoQuotes r3 = SomeQuotes r1 ts $ r2 <> r3
  SomeQuotes r1 ts1 r2 <> SomeQuotes r3 ts2 r4 =
    SomeQuotes r1 (ts1 <> SplayTree.singleton (StringLiteral $ r2 <> r3) <> ts2) r4

instance Monoid EvenInsideQuote where
  mempty = NoQuotes mempty

newtype EvenOutsideQuote = EvenOutsideQuote Tokens
  deriving (Eq, Ord, Show, Monoid, Semigroup)

data OddInsideQuote
  = OddInsideQuote !Rope !Tokens
  deriving (Eq, Ord, Show)

data OddOutsideQuote
  = OddOutsideQuote !Tokens !Rope
  deriving (Eq, Ord, Show)

data QuotedTokens
  = EvenQuotes
    !EvenOutsideQuote
    !EvenInsideQuote
  | OddQuotes
    !OddOutsideQuote
    !OddInsideQuote
  deriving (Eq, Ord, Show)

oddEvenOutside :: OddOutsideQuote -> EvenInsideQuote -> OddOutsideQuote
oddEvenOutside (OddOutsideQuote ts r1) (NoQuotes r2) = OddOutsideQuote ts $ r1 <> r2
oddEvenOutside (OddOutsideQuote ts1 r1) (SomeQuotes r2 ts2 r3) =
  OddOutsideQuote (ts1 <> SplayTree.singleton (StringLiteral $ r1 <> r2) <> ts2) r3

oddEvenInside :: OddInsideQuote -> EvenOutsideQuote -> OddInsideQuote
oddEvenInside (OddInsideQuote r ts1) (EvenOutsideQuote ts2) = OddInsideQuote r $ ts1 <> ts2

evenOddOutside :: EvenOutsideQuote -> OddOutsideQuote -> OddOutsideQuote
evenOddOutside (EvenOutsideQuote ts1) (OddOutsideQuote ts2 r) = OddOutsideQuote (ts1 <> ts2) r

evenOddInside :: EvenInsideQuote -> OddInsideQuote -> OddInsideQuote
evenOddInside (NoQuotes r1) (OddInsideQuote r2 ts) = OddInsideQuote (r1 <> r2) ts
evenOddInside (SomeQuotes r1 ts1 r2) (OddInsideQuote r3 ts2) =
  OddInsideQuote r1 (ts1 <> SplayTree.singleton (StringLiteral $ r2 <> r3) <> ts2)

oddOddOutside :: OddOutsideQuote -> OddInsideQuote -> EvenOutsideQuote
oddOddOutside (OddOutsideQuote ts1 r1) (OddInsideQuote r2 ts2) = 
  EvenOutsideQuote (ts1 <> SplayTree.singleton (StringLiteral $ r1 <> r2) <> ts2)

oddOddInside :: OddInsideQuote -> OddOutsideQuote -> EvenInsideQuote
oddOddInside (OddInsideQuote r1 ts1) (OddOutsideQuote ts2 r2) = SomeQuotes r1 (ts1 <> ts2) r2

instance Semigroup QuotedTokens where
  EvenQuotes oq1 iq1 <> EvenQuotes oq2 iq2 = EvenQuotes (oq1 <> oq2) (iq1 <> iq2)
  OddQuotes oq1 iq1 <> EvenQuotes oq2 iq2 = OddQuotes (oddEvenOutside oq1 iq2) (oddEvenInside iq1 oq2)
  EvenQuotes oq1 iq1 <> OddQuotes oq2 iq2 = OddQuotes (evenOddOutside oq1 oq2) (evenOddInside iq1 iq2)
  OddQuotes oq1 iq1 <> OddQuotes oq2 iq2 = EvenQuotes (oddOddOutside oq1 iq2) (oddOddInside iq1 oq2)

instance Monoid QuotedTokens where
  mempty = EvenQuotes mempty mempty

tokeniseText :: Text -> QuotedTokens
tokeniseText t =
  case Rope.fromText <$> pieces of
    [] -> panic "can't happen?"
    [x] -> EvenQuotes (EvenOutsideQuote $ SplayTree.singleton $ NotStringLiteral x) (NoQuotes x)
    x:xs
      | even (length xs) -> do
        let (xs', x') = unsnocNonEmpty x xs
        OddQuotes (OddOutsideQuote (alternate False xs') x') (OddInsideQuote x $ alternate True xs)
      | otherwise -> do
        let (xs', x') = unsnocNonEmpty x xs
        EvenQuotes (EvenOutsideQuote $ alternate False (x:xs)) (SomeQuotes x (alternate False (tl xs')) x')
  where
    pieces = Text.splitOn "\"" t
    tl (_:xs) = xs
    tl [] = panic "Shouldn't happen?"
    alternate :: Bool -> [Rope] -> Tokens
    alternate inside xs = treeFoldMap SplayTree.singleton $ alternate' inside xs
    alternate' _ [] = []
    alternate' inside (x:xs)
      | inside = StringLiteral x : alternate' False xs
      | otherwise = NotStringLiteral x : alternate' True xs

unsnocNonEmpty :: a -> [a] -> ([a], a)
unsnocNonEmpty a [] = ([], a)
unsnocNonEmpty a (b:bs) = first (a:) $ unsnocNonEmpty b bs


  -- -- | quotes == 0 = EvenQuotes
  -- --   (EvenOutsideQuote $ SplayTree.singleton $ NotStringLiteral r)
  -- --   (NoQuotes r)
  -- -- | even quotes = EvenQuotes
  -- --   (EvenOutsideQuote $ treeFoldMap SplayTree.singleton $ splitOutside r)
  -- --   (SomeQuotes r1 _ _)
  -- where
  --   pieces = Text.splitOn '"' t

  --   r = Rope.fromText t
  --   (r1, r2) = Rope.span (/= '"') r
  --   quotes = Text.count "\"" t

-- --     splitRope rope inside
-- --       | Rope.null rope = []
-- --     splitRope ('"' Rope.:< rope) inside = do
-- --       let (pre, post) = Rope.takeWhile (/= '"') rope
-- --       let con
-- --             | inside = StringLiteral
-- --             | outside = NotStringLiteral
-- --       case (Rope.null pre, Rope.null post) of
-- --         (True, True) -> []
-- --         (False, True) -> 

  --   splitOutside rope
  --     | Rope.null rope = []
  --     | otherwise = do
  --       let (pre, post) = Rope.span (/= '"') rope
  --       if Rope.null pre then
  --         splitLit post
  --       else
  --         NotStringLiteral pre : splitLit post

  --   splitLit ('"' Rope.:< rest) = do
  --     let (pre, post) = Rope.span (/= '"') rest
  --     StringLiteral pre : splitOutside (Rope.tail post)
  --   splitLit _ = []

  --   -- splitInside rope
  --   --   | Rope.null rope = []
  --   --   | otherwise = do
  --   --     let (pre, post) = Rope.span (/= '"') rope
  --   --     if Rope.null pre then
  --   --       splitLit
