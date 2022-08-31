-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.Unboxed.NonEmpty
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# Language DeriveDataTypeable #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language ImportQualifiedPost #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Data.Vector.Unboxed.NonEmpty
    ( Unbox
    , Vector (..)
      -- * Conversion
    , fromNonEmpty
      -- * Useful stuff
    , length
    , toList
    , toNonEmpty
    , (!)
    ) where

import Control.DeepSeq hiding (force)
import Data.Coerce
import Data.Data
import Data.Functor.Alt
import Data.Functor.Bind
import Data.Functor.Classes
import Data.Functor.Extend
import Data.Hashable
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Pointed
import Data.Semigroup.Foldable (Foldable1)
import Data.Semigroup.Foldable qualified as NE
import Data.Semigroup.Traversable
import Data.Vector.Instances ()
import Data.Vector.Unboxed (Unbox)
import Data.Vector.Unboxed qualified as V
import Prelude hiding (filter, length, reverse)
import Test.QuickCheck hiding (generate)


-- |
-- A sequence of values that are repeated multiple times in contiguous blocks.
newtype Vector a
    = NEV { unwrap :: V.Vector a }
    deriving stock (Data, Eq, Ord)
    deriving newtype (Hashable, NFData, Semigroup)


-- |
-- Generation biases towards medium length.
instance (Arbitrary a, Unbox a) => Arbitrary (Vector a) where

    arbitrary = do
        list   <- arbitrary
        values <- case list of
            [] -> pure <$> arbitrary
            xs -> pure xs
        pure . NEV $ V.fromList values


instance (Show a, Unbox a) => Show (Vector a) where

    show = show . unwrap


-- |
-- /O(n)/
--
-- Construct a 'Vector' from a non-empty structure.
{-# INLINE fromNonEmpty #-}
fromNonEmpty :: (Foldable1 f, Unbox a) => f a -> Vector a
fromNonEmpty = NEV . V.fromList . NE.toList . NE.toNonEmpty


-- |
-- /O(1)/ Index a vector.
{-# INLINE (!) #-}
(!) :: Unbox a => Vector a -> Int -> a
(!) v = (V.!) $ unwrap v


-- |
-- /O(1)/
--
-- Length of a vector
{-# INLINE length #-}
length :: Unbox a => Vector a -> Int
length = V.length . unwrap


-- |
-- /O(n)/
--
-- Convert a Vector to a list.
{-# INLINE toList #-}
toList :: Unbox a => Vector a -> [a]
toList = V.toList . unwrap


-- |
-- /O(n)/
--
-- Convert a Vector to a list.
{-# INLINE toNonEmpty #-}
toNonEmpty :: Unbox a => Vector a -> NonEmpty a
toNonEmpty = NE.fromList . V.toList . unwrap
