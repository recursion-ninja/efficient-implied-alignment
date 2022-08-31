-----------------------------------------------------------------------------
-- |
-- Module      :  Alignment.Pairwise.Ukkonen
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Direct optimization pairwise alignment using the Needleman-Wunsch algorithm.
-- These functions will allocate an M * N matrix.
--
-----------------------------------------------------------------------------

{-# Language ApplicativeDo #-}
{-# Language ConstraintKinds #-}
{-# Language DerivingStrategies #-}
{-# Language FlexibleContexts #-}
{-# Language ImportQualifiedPost #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language UnboxedTuples #-}

module Alignment.Pairwise.Ukkonen
    ( alignUkkonen
    ) where

import Alignment.Pairwise.Ukkonen.Unboxed (unboxedUkkonenDO)
import Data.Alphabet (Alphabet)
import Data.Key (Indexable, Key)
import Data.SymbolString (SymbolContext, SymbolString)
import Data.TCM (TransitionCostMatrix)


-- |
-- Performs a naive direct optimization.
-- Takes in two characters to run DO on and an overlap function
-- Returns an assignment character, the cost of that assignment, the assignment
-- character with gaps included, the aligned version of the first input character,
-- and the aligned version of the second input character. The process for this
-- algorithm is to generate a traversal matrix, then perform a traceback.
{-# SCC alignUkkonen #-}
{-# INLINE alignUkkonen #-}
{-# SPECIALISE alignUkkonen :: Eq a => Alphabet a -> TransitionCostMatrix -> SymbolString -> SymbolString -> (Word, SymbolString) #-}
alignUkkonen
    :: (Eq a, Foldable f, Indexable f, Key f ~ Int)
    => Alphabet a           -- ^ Alphabet of symbols
    -> TransitionCostMatrix -- ^ Structure defining the transition costs between character states
    -> f SymbolContext      -- ^ First  dynamic character
    -> f SymbolContext      -- ^ Second dynamic character
    -> (Word, SymbolString) -- ^ The cost of the alignment and the alignment context
alignUkkonen = unboxedUkkonenDO

