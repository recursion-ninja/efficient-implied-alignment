-----------------------------------------------------------------------------
-- |
-- Module      :  Alignment.Pairwise.NeedlemanWunsch
-- Copyright   :  (c) 2018 Alex Washburn
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-- Direct optimization pairwise alignment using the Needleman-Wunsch algorithm.
-- These functions will allocate an M * N matrix.
--
-----------------------------------------------------------------------------

{-# Language ConstraintKinds #-}
{-# Language FlexibleContexts #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}

module Alignment.Pairwise.NeedlemanWunsch
    ( naiveDO
    , naiveDOMemo
    ) where

import Alignment.Pairwise.Internal
import Data.Alphabet
import Data.Key
import Data.List.NonEmpty (NonEmpty(..))
import Data.Matrix (fromList)
import Data.SymbolString
import Data.TCM
import Data.Vector.NonEmpty


-- |
-- Performs a naive direct optimization.
-- Takes in two characters to run DO on and a metadata object
-- Returns an assignment character, the cost of that assignment, the assignment
-- character with gaps included, the aligned version of the first input character,
-- and the aligned version of the second input character. The process for this
-- algorithm is to generate a traversal matrix, then perform a traceback.
{-# INLINABLE naiveDO #-}
{-# SPECIALIZE naiveDO :: Alphabet SymbolAmbiguityGroup -> (Int -> Int -> Word) -> Vector SymbolContext -> Vector SymbolContext -> (Word, Vector SymbolContext) #-}
naiveDO
    :: (Foldable f, Indexable f, Key f ~ Int, Ord s)
    => Alphabet s                   -- ^ Alphabet of symbols
    -> (Int -> Int -> Word)         -- ^ Structure defining the transition costs between character states
    -> f SymbolContext              -- ^ First  dynamic character
    -> f SymbolContext              -- ^ Second dynamic character
    -> (Word, Vector SymbolContext) -- ^ The cost of the alignment and the alignment context
naiveDO alphabet costStruct = directOptimization gap (overlap alphabet costStruct) undefined
    $ createNeedlemanWunchMatrix gap
    where gap = encodeAmbiguityGroup alphabet $ gapSymbol alphabet :| []


-- |
-- The same as 'naiveDO' except that the "cost structure" parameter is assumed to
-- be a memoized overlap function.
{-# INLINABLE naiveDOMemo #-}
{-# SPECIALIZE naiveDOMemo :: Alphabet SymbolAmbiguityGroup -> TransitionCostMatrix -> Vector SymbolContext -> Vector SymbolContext -> (Word, Vector SymbolContext) #-}
naiveDOMemo
    :: (Eq s, Foldable f, Indexable f, Key f ~ Int)
    => Alphabet s
    -> TransitionCostMatrix
    -> f SymbolContext
    -> f SymbolContext
    -> (Word, Vector SymbolContext)
naiveDOMemo alphabet tcm = directOptimization gap tcm undefined $ createNeedlemanWunchMatrix gap
    where gap = encodeAmbiguityGroup alphabet $ gapSymbol alphabet :| []


-- |
-- Main function to generate a 'NeedlemanWunchMatrix'. Works as in Needleman-Wunsch,
-- but allows for multiple indel/replacement costs, depending on the symbol change
-- cost function. Also, returns the aligned parent characters, with appropriate
-- ambiguities, as the third of each tuple in the matrix.
--
-- Takes in two 'EncodableDynamicCharacter's and a 'CostStructure'. The first
-- character must be the longer of the two and is the top labeling of the matrix.
-- Returns a 'NeedlemanWunchMatrix'.
{-# INLINABLE createNeedlemanWunchMatrix #-}
{-# SPECIALIZE createNeedlemanWunchMatrix :: SymbolAmbiguityGroup -> (SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> (SymbolAmbiguityGroup, Word)) ->  Vector SymbolContext -> Vector SymbolContext -> NeedlemanWunchMatrix SymbolAmbiguityGroup #-}
createNeedlemanWunchMatrix
    :: (Foldable f, Indexable f, Key f ~ Int)
    => SymbolAmbiguityGroup
    -> (SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> (SymbolAmbiguityGroup, Word))
    -> f SymbolContext
    -> f SymbolContext
    -> NeedlemanWunchMatrix SymbolAmbiguityGroup
createNeedlemanWunchMatrix gap overlapFunction topString leftString = result
    where
        result = fromList
            (rows', cols')
            [ needlemanWunschDefinition gap overlapFunction topString leftString result (i, j)
            | i <- [0 .. rows' - 1]
            , j <- [0 .. cols' - 1]
            ]

        rows' = length leftString + 1
        cols' = length topString + 1
