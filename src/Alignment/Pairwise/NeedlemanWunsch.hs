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
-- These funtions will allocate an M * N matrix.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds, FlexibleContexts, TypeFamilies #-}

module Alignment.Pairwise.NeedlemanWunsch
  ( naiveDO
--  , naiveDOConst
--  , naiveDOMemo
  ) where

import Alignment.Pairwise.Internal
import Data.Alphabet
import Data.Foldable
import Data.List.NonEmpty      (NonEmpty(..))
import Data.Key
import Data.Matrix.ZeroIndexed (matrix)
import Data.MonoTraversable
import Data.Pointed
import Data.SymbolString


-- |
-- Performs a naive direct optimization.
-- Takes in two characters to run DO on and a metadata object
-- Returns an assignment character, the cost of that assignment, the assignment
-- character with gaps included, the aligned version of the first input character,
-- and the aligned version of the second input character. The process for this
-- algorithm is to generate a traversal matrix, then perform a traceback.
naiveDO
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     , Ord s
     )
  => Alphabet s                         -- ^ Alphabet of symbols
  -> (s -> s -> Word)                   -- ^ Structure defining the transition costs between character states
  -> f (SymbolContext s)                -- ^ First  dynamic character
  -> f (SymbolContext s)                -- ^ Second dynamic character
  -> (Word, NonEmpty (SymbolContext s)) -- ^ The cost of the alignment
                                        --
                                        --   The /ungapped/ character derived from the the input characters' N-W-esque matrix traceback
                                        --
                                        --   The /gapped/ character derived from the the input characters' N-W-esque matrix traceback
                                        --
                                        --   The gapped alignment of the /first/ input character when aligned with the second character
                                        --
                                        --   The gapped alignment of the /second/ input character when aligned with the first character

naiveDO alphabet costStruct = directOptimization (overlap alphabet costStruct) $ createNeedlemanWunchMatrix (gapSymbol alphabet)


{-
-- |
-- The same as 'naiveDO' except that the "cost structure" parameter is ignored.
-- Instead a constant cost is used.
naiveDOConst
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     , Ord s
     )
  => (SymbolAmbiguityGroup s -> SymbolAmbiguityGroup s -> Word)
  -> f (SymbolContext s)
  -> f (SymbolContext s)
  -> (Word, NonEmpty (SymbolContext s))
naiveDOConst _ = directOptimization overlapConst createNeedlemanWunchMatrix


-- |
-- The same as 'naiveDO' except that the "cost structure" parameter is assumed to
-- be a memoized overlap function.
naiveDOMemo
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     , Ord s
     )
  => (SymbolAmbiguityGroup s -> SymbolAmbiguityGroup s -> Word)
  -> f (SymbolContext s)
  -> f (SymbolContext s)
  -> (Word, NonEmpty (SymbolContext s))
naiveDOMemo tcm = directOptimization tcm createNeedlemanWunchMatrix
-}


-- |
-- Main function to generate a 'NeedlemanWunchMatrix'. Works as in Needleman-Wunsch,
-- but allows for multiple indel/replacement costs, depending on the symbol change
-- cost function. Also, returns the aligned parent characters, with appropriate
-- ambiguities, as the third of each tuple in the matrix.
--
-- Takes in two 'EncodableDynamicCharacter's and a 'CostStructure'. The first
-- character must be the longer of the two and is the top labeling of the matrix.
-- Returns a 'NeedlemanWunchMatrix'.
createNeedlemanWunchMatrix
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     , Ord s
     )
  => s
  -> (SymbolAmbiguityGroup s -> SymbolAmbiguityGroup s -> (SymbolAmbiguityGroup s, Word))
  -> f (SymbolContext s)
  -> f (SymbolContext s)
  -> NeedlemanWunchMatrix (SymbolAmbiguityGroup s)
--createNeedlemanWunchMatrix topString leftString overlapFunction = trace renderedMatrix result
createNeedlemanWunchMatrix gap overlapFunction topString leftString = result
  where
    result             = matrix rows cols generatingFunction
    rows               = length leftString + 1
    cols               = length topString  + 1
    generatingFunction = needlemanWunschDefinition gap overlapFunction topString leftString result
--    renderedMatrix     = renderCostMatrix topString leftString result
