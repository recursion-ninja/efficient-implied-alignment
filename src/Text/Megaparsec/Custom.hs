-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Megaparsec.Custom
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Custom utility combinators for 'Text.Megaparsec' parser construction.
--
-----------------------------------------------------------------------------

{-# Language ApplicativeDo #-}
{-# Language BangPatterns #-}
{-# Language FlexibleContexts #-}
{-# Language ImportQualifiedPost #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}

module Text.Megaparsec.Custom
    ( endOfLine
    , fails
    , inlinedSpace
    , isInlinedSpace
    , noneOfThese
    , someOfThese
    ) where

import Data.Char (isSpace)
import Data.Foldable
import Data.Functor (void, ($>))
import Data.List (sort)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Vector.Unboxed (Unbox, Vector, (!))
import Data.Vector.Unboxed qualified as V
import Data.Void
import Text.Megaparsec


-- |
-- Matches one or more elements from the supplied collection.
--
-- Coerces the collection to a sorted, unboxed vector and performs a binary
-- search on the elements to determine if a 'Token s' is part of the collection.
--
-- Preferable to 'Text.Megaparsec.someOf'.
{-# INLINE someOfThese #-}
{-# SPECIALISE someOfThese :: Foldable f => f Char -> Parsec Void  T.Text  T.Text #-}
{-# SPECIALISE someOfThese :: Foldable f => f Char -> Parsec Void LT.Text LT.Text #-}
{-# SPECIALISE someOfThese :: Foldable f => f Char -> Parsec Void  String  String #-}
{-# SPECIALISE someOfThese :: String -> Parsec Void  T.Text  T.Text #-}
{-# SPECIALISE someOfThese :: String -> Parsec Void LT.Text LT.Text #-}
{-# SPECIALISE someOfThese :: String -> Parsec Void  String  String #-}
someOfThese :: (Foldable f, MonadParsec e s m, Token s ~ a, Unbox a) => f a -> m (Tokens s)
someOfThese xs =
    let !uvec = V.fromList . sort $ toList xs
        !cond = withinVec uvec
    in  takeWhile1P Nothing cond


-- |
-- Matches one or more elements /not/ from the supplied collection.
--
-- Coerces the collection to a sorted, unboxed vector and performs a binary
-- search on the elements to determine if a 'Token s' is part of the collection.
--
-- Preferable to 'noneOf'.
{-# INLINE noneOfThese #-}
{-# SPECIALISE noneOfThese :: Foldable f => f Char -> Parsec Void  T.Text  T.Text #-}
{-# SPECIALISE noneOfThese :: Foldable f => f Char -> Parsec Void LT.Text LT.Text #-}
{-# SPECIALISE noneOfThese :: Foldable f => f Char -> Parsec Void  String  String #-}
{-# SPECIALISE noneOfThese :: String -> Parsec Void  T.Text  T.Text #-}
{-# SPECIALISE noneOfThese :: String -> Parsec Void LT.Text LT.Text #-}
{-# SPECIALISE noneOfThese :: String -> Parsec Void  String  String #-}
noneOfThese :: (Foldable f, MonadParsec e s m, Token s ~ a, Unbox a) => f a -> m (Tokens s)
noneOfThese xs =
    let !uvec = V.fromList . sort $ toList xs
        !cond = not . withinVec uvec
    in  takeWhile1P Nothing cond


-- |
-- Custom 'eol' combinator to account for /very/ old Mac file formats ending
-- lines in a single @\'\\r\'@.
{-# INLINE endOfLine #-}
{-# SPECIALISE endOfLine :: Parsec Void  T.Text () #-}
{-# SPECIALISE endOfLine :: Parsec Void LT.Text () #-}
{-# SPECIALISE endOfLine :: Parsec Void  String () #-}
endOfLine :: (Enum (Token s), MonadParsec e s m) => m ()
endOfLine = choice [nl, try (cr *> nl), cr] $> ()
    where
        newLineChar  = enumCoerce '\n'
        carriageChar = enumCoerce '\r'
        nl           = single newLineChar $> ()
        cr           = single carriageChar $> ()


-- |
-- Accepts zero or more Failure messages.
{-# INLINABLE fails #-}
{-# SPECIALISE fails :: [String] -> Parsec Void  T.Text a #-}
{-# SPECIALISE fails :: [String] -> Parsec Void LT.Text a #-}
{-# SPECIALISE fails :: [String] -> Parsec Void  String a #-}
fails :: MonadParsec e s m => [String] -> m a
fails = failure Nothing . S.fromList . fmap Label . mapMaybe nonEmpty


-- |
-- Consumes zero or more whitespace characters that are not newline characters.
{-# INLINE inlinedSpace #-}
{-# SPECIALISE inlinedSpace :: Parsec Void  T.Text () #-}
{-# SPECIALISE inlinedSpace :: Parsec Void LT.Text () #-}
{-# SPECIALISE inlinedSpace :: Parsec Void  String () #-}
inlinedSpace :: (Token s ~ Char, MonadParsec e s m) => m ()
inlinedSpace = void $ takeWhileP (Just "inline space") isInlinedSpace


-- |
-- Returns @True@ if the 'Char' is a space value but not a "newline" character.
{-# INLINE isInlinedSpace #-}
isInlinedSpace :: Char -> Bool
isInlinedSpace c = isSpace c && c /= '\n' && c /= '\r'


-- |
-- Convert one Enum to another through the Int value.
enumCoerce :: (Enum a, Enum b) => a -> b
enumCoerce = toEnum . fromEnum


{-# INLINE withinVec #-}
{-# SPECIALISE withinVec :: Vector Char -> Char -> Bool #-}
withinVec :: (Ord a, Unbox a) => Vector a -> a -> Bool
withinVec v e =
    let -- Perform a binary search on the unboxed vector
        -- to determine if a character is valid.
        --
        -- Equally fast, and uses less memory than a Set.
        {-# INLINE go #-}
        go !lo !hi
            | lo > hi
            = False
            | otherwise
            = let
                  !md = (hi + lo) `div` 2
                  !z  = v ! md
              in  case z `compare` e of
                  EQ -> True
                  LT -> go (md + 1) hi
                  GT -> go lo (md - 1)
    in  go 0 (V.length v - 1)
