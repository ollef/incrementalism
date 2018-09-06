-- | Helpers for working with 'Text' in UTF-16 code units
module Text where

import Data.Text(Text)
import qualified Data.Text.Array as Array
import qualified Data.Text.Internal as Text
import qualified Data.Text.Unsafe as Unsafe

clamp16 :: Int -> Text -> Int
clamp16 i t@(Text.Text arr off _len)
  | i <= 0 = 0
  | i >= len = len
  | isLowSurrogate = i - 1
  | otherwise = i
  where
    cp = Array.unsafeIndex arr (off + i)
    isLowSurrogate = 0xDC00 <= cp && cp <= 0xDFFF
    len = Unsafe.lengthWord16 t

take16 :: Int -> Text -> Text
take16 n t = Unsafe.takeWord16 (clamp16 n t) t

drop16 :: Int -> Text -> Text
drop16 n t = Unsafe.dropWord16 (clamp16 n t) t

split16At :: Int -> Text -> (Text, Text)
split16At n t = (Unsafe.takeWord16 n' t, Unsafe.dropWord16 n' t)
  where
    n' = clamp16 n t

chunks16Of :: Int -> Text -> [Text]
chunks16Of n t
  | len == 0 = []
  | len <= n = [t]
  | otherwise = pre : chunks16Of n post
  where
    (pre, post) = split16At n t
    len = Unsafe.lengthWord16 t

-- | Split a string _after_ every newline
splitLines :: Text -> [Text]
splitLines t = case findCharIndex16 '\n' t of
  Nothing -> [t]
  Just i -> do
    let (line, t') = split16At (i + 1) t
    line : splitLines t'

findCharIndex16 :: Char -> Text -> Maybe Int
findCharIndex16 c t = go 0
  where
    len = Unsafe.lengthWord16 t
    go i
      | i < len = case Unsafe.iter t i of
        Unsafe.Iter c' delta
          | c == c' -> Just i
          | otherwise -> go $ i + delta
      | otherwise = Nothing
