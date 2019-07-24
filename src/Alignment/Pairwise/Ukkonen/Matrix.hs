-----------------------------------------------------------------------------
-- |
-- Module      :  Alignment.Pairwise.Ukkonen.Internal
-- Copyright   :  (c) 2018 Alex Washburn
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-- Direct optimization functionality for binary trees.
-- Implement's Ukkonen's space & time saving algorithm.
--
-- Allocates a "ribbon" down the diagonal of the matrix rather than the entire matrix.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds, DeriveFoldable, DeriveFunctor, FlexibleContexts, TypeFamilies #-}

module Alignment.Pairwise.Ukkonen.Matrix
  ( UkkonenMethodMatrix(..)
  ) where

import           Alignment.Pairwise.Ukkonen.Ribbon           (Ribbon)
import           Data.Key
import           Prelude           hiding (lookup)


-- |
-- Time & space saving data structure for computing only a central "ribbon" of
-- a two-dimensional matrix.
--
-- Allocates a "ribbon" down the diagonal plus an offset of the matrix rather
-- than the entire matrix. The computed ribbon of the matrix is expanded until
-- optimality of the the result can be guaranteed. The ribbon is expanded at most
-- a logrithmic number of times in terms of the matrix dimensions.
--
-- Use the 'createUkkonenMethodMatrix' function to create this effcient structure.
newtype UkkonenMethodMatrix a = U (Ribbon a)
    deriving (Eq, Foldable, Functor)


type instance Key UkkonenMethodMatrix = (Int, Int)


instance Indexable UkkonenMethodMatrix where

    {-# INLINE index #-}
    index (U r) k = r ! k


instance Lookup UkkonenMethodMatrix where

    {-# INLINE lookup #-}
    k `lookup` (U x) = k `lookup` x
