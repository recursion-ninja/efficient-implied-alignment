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
    ( alignNaively
    , alignNeedlemanWunsch
    ) where

import Alignment.Pairwise.Internal
import Data.Alphabet
import Data.List.NonEmpty (NonEmpty(..))
import Data.Matrix.Unboxed (fromLists)
import Data.SymbolString
import Data.TCM


-- |
-- Performs a naive direct optimization.
-- Takes in two characters to run DO on and a metadata object
-- Returns an assignment character, the cost of that assignment, the assignment
-- character with gaps included, the aligned version of the first input character,
-- and the aligned version of the second input character. The process for this
-- algorithm is to generate a traversal matrix, then perform a traceback.
{-# INLINABLE alignNaively #-}
{-# SPECIALIZE alignNaively :: Alphabet SymbolAmbiguityGroup -> (Int -> Int -> Word) -> SymbolString -> SymbolString -> (Word, SymbolString) #-}
alignNaively
    :: Ord s
    => Alphabet s                   -- ^ Alphabet of symbols
    -> (Int -> Int -> Word)         -- ^ Structure defining the transition costs between character states
    -> SymbolString                 -- ^ First  dynamic character
    -> SymbolString                 -- ^ Second dynamic character
    -> (Word, SymbolString) -- ^ The cost of the alignment and the alignment context
alignNaively alphabet costStruct =
    let gap = encodeAmbiguityGroup alphabet $ gapSymbol alphabet :| []
    in  directOptimization gap (overlap alphabet costStruct) undefined $ createNeedlemanWunchMatrix gap


-- |
-- The same as 'naiveDO' except that the "cost structure" parameter is assumed to
-- be a memoized overlap function.
{-# INLINABLE alignNeedlemanWunsch #-}
{-# SPECIALIZE alignNeedlemanWunsch :: Alphabet SymbolAmbiguityGroup -> TransitionCostMatrix -> SymbolString -> SymbolString -> (Word, SymbolString) #-}
alignNeedlemanWunsch
    :: Eq s => Alphabet s -> TransitionCostMatrix -> SymbolString -> SymbolString -> (Word, SymbolString)
alignNeedlemanWunsch alphabet tcm =
    let gap = encodeAmbiguityGroup alphabet $ gapSymbol alphabet :| []
    in  directOptimization gap tcm undefined $ createNeedlemanWunchMatrix gap


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
createNeedlemanWunchMatrix
    :: SymbolAmbiguityGroup
    -> (SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> (SymbolAmbiguityGroup, Word))
    -> SymbolString
    -> SymbolString
    -> NeedlemanWunchMatrix
createNeedlemanWunchMatrix gap overlapFunction topString leftString = result
    where
        result = fromLists
            [ [ needlemanWunschDefinition gap overlapFunction topString leftString result (i, j)
              | j <- [0 .. cols' - 1]
              ]
            | i <- [0 .. rows' - 1]
            ]

        rows' = length leftString + 1
        cols' = length topString + 1
