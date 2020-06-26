-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Vector.NonEmpty
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}


{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Data.Vector.Unboxed.NonEmpty
  ( Unbox
  , Vector(..)
  -- * Construction
  , fromNonEmpty
  , generate
  , singleton
  , unfoldr
  -- * Conversion
  , toVector
  , fromVector
  , unsafeFromVector
  -- * Deconstruction
  , uncons
  -- * Useful stuff
  , (!)
  , filter
  , length
  , reverse
  , toList
  , toNonEmpty
  ) where


import           Control.DeepSeq            hiding (force)
import           Data.Coerce
import           Data.Data
import           Data.Functor.Alt
import           Data.Functor.Bind
import           Data.Functor.Classes
import           Data.Functor.Extend
import           Data.Hashable
import           Data.List.NonEmpty         (NonEmpty)
import qualified Data.List.NonEmpty         as NE
import           Data.Pointed
import           Data.Semigroup.Foldable    (Foldable1)
import qualified Data.Semigroup.Foldable    as NE
import           Data.Semigroup.Traversable
import           Data.Vector.Instances      ()
import           Data.Vector.Unboxed        (Unbox)
import qualified Data.Vector.Unboxed        as V
import           Prelude                    hiding (filter, length, reverse)
import           Test.QuickCheck            hiding (generate)


-- |
-- A sequence of values that are repeated multiple times in contiguous blocks.
newtype Vector a = NEV { unwrap :: V.Vector a }
  deriving stock   (Data, Eq, Ord) -- , Foldable, Traversable)
  deriving newtype ( Hashable
                   , NFData
                   , Semigroup
                   )


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
-- /O(1)/
--
-- A synomym for 'point'.
{-# INLINE singleton #-}
singleton :: Unbox a => a -> Vector a
singleton = NEV . V.singleton


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
-- /O(n)/ Drop elements that do not satisfy the predicate
{-# INLINE filter #-}
filter :: Unbox a => (a -> Bool) -> Vector a -> Vector a
filter f =  NEV . V.filter f . unwrap


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
-- Reverse a vector
{-# INLINE reverse #-}
reverse :: Unbox a => Vector a -> Vector a
reverse = NEV . V.reverse . unwrap


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


-- |
-- /O(n)/
--
-- Construct a vector by repeatedly applying the generator function to a seed.
-- The generator function always yields the next element and either @ Just @ the
-- new seed or 'Nothing' if there are no more elements to be generated.
--
-- > unfoldr (\n -> (n, if n == 0 then Nothing else Just (n-1))) 10
-- >  = <10,9,8,7,6,5,4,3,2,1>
{-# INLINE unfoldr #-}
unfoldr :: Unbox a => (b -> (a, Maybe b)) -> b -> Vector a
unfoldr f = NEV . uncurry V.fromListN . go 0
  where
--  go :: Int -> b -> (Int, [a])
    go n b =
         let (v, mb) = f b
         in  (v:) <$> maybe (n, []) (go (n+1)) mb


-- |
-- /O(n)/
--
-- Construct a vector of the given length by applying the function to each index
generate :: Unbox a => Int -> (Int -> a) -> Vector a
generate n f
  | n < 1     = error $ "Called Vector.Nonempty.generate on a non-positive dimension " <> show n
  | otherwise = NEV $ V.generate n f


-- |
-- /O(1)/
--
-- Get the underlying 'V.Vector'.
toVector :: Vector a -> V.Vector a
toVector = unwrap


-- |
-- /O(1)/
--
-- Attempt to convert a 'V.Vector' to a non-empty 'Vector'.
fromVector :: Unbox a => V.Vector a -> Maybe (Vector a)
fromVector v
  | V.null v  = Nothing
  | otherwise = Just $ NEV v


-- |
-- /O(1)/
--
-- Attempt to convert a 'V.Vector' to a non-empty 'Vector' throwing an
-- error if the vector received is empty.
unsafeFromVector :: Unbox a => V.Vector a -> Vector a
unsafeFromVector v
  | V.null v  = error "NonEmpty.unsafeFromVector: empty vector"
  | otherwise = NEV v




-- | /O(n)/
--
-- 'uncons' produces both the first element of the 'Vector' and a
-- 'Vector' of the remaining elements, if any.
uncons :: Unbox a => Vector a -> (a, Maybe (Vector a))
uncons (NEV v) = (first, stream)
  where
    stream
      | len == 1  = Nothing
      | otherwise = Just . NEV $ V.slice 1 (len-1) v
    first = v V.! 0
    len   = V.length v
