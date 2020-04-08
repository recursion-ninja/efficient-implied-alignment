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

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Alignment.Pairwise.NeedlemanWunsch
  ( naiveDO
--  , naiveDOConst
  , naiveDOMemo
  ) where

import           Alignment.Pairwise.Internal
import           Data.Alphabet
import           Data.Key
import           Data.List.NonEmpty          (NonEmpty (..))
import           Data.Matrix                 (fromList)
import           Data.SymbolString
import           Data.TCM
import           Data.Vector.NonEmpty


-- |
-- Performs a naive direct optimization.
-- Takes in two characters to run DO on and a metadata object
-- Returns an assignment character, the cost of that assignment, the assignment
-- character with gaps included, the aligned version of the first input character,
-- and the aligned version of the second input character. The process for this
-- algorithm is to generate a traversal matrix, then perform a traceback.
{-# INLINEABLE naiveDO #-}
{-# SPECIALIZE naiveDO :: Alphabet SymbolAmbiguityGroup -> (Int -> Int -> Word) -> Vector SymbolContext -> Vector SymbolContext -> (Word, Vector SymbolContext) #-}
naiveDO
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     , Ord s
     )
  => Alphabet s                   -- ^ Alphabet of symbols
  -> (Int -> Int -> Word)         -- ^ Structure defining the transition costs between character states
  -> f SymbolContext              -- ^ First  dynamic character
  -> f SymbolContext              -- ^ Second dynamic character
  -> (Word, Vector SymbolContext) -- ^ The cost of the alignment and the alignment context
naiveDO alphabet costStruct = directOptimization (overlap alphabet costStruct) undefined $ createNeedlemanWunchMatrix gap
  where
    gap = encodeAmbiguityGroup alphabet $ gapSymbol alphabet :| []


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
-}


-- |
-- The same as 'naiveDO' except that the "cost structure" parameter is assumed to
-- be a memoized overlap function.
{-# INLINEABLE naiveDOMemo #-}
{-# SPECIALIZE naiveDOMemo :: Alphabet SymbolAmbiguityGroup -> TransitionCostMatrix -> Vector SymbolContext -> Vector SymbolContext -> (Word, Vector SymbolContext) #-}
naiveDOMemo
  :: ( Eq s
     , Foldable f
     , Indexable f
     , Key f ~ Int
     )
  => Alphabet s
  -> TransitionCostMatrix
  -> f SymbolContext
  -> f SymbolContext
  -> (Word, Vector SymbolContext)
naiveDOMemo alphabet tcm = directOptimization tcm undefined $ createNeedlemanWunchMatrix gap
  where
    gap = encodeAmbiguityGroup alphabet $ gapSymbol alphabet :| []


-- |
-- Main function to generate a 'NeedlemanWunchMatrix'. Works as in Needleman-Wunsch,
-- but allows for multiple indel/replacement costs, depending on the symbol change
-- cost function. Also, returns the aligned parent characters, with appropriate
-- ambiguities, as the third of each tuple in the matrix.
--
-- Takes in two 'EncodableDynamicCharacter's and a 'CostStructure'. The first
-- character must be the longer of the two and is the top labeling of the matrix.
-- Returns a 'NeedlemanWunchMatrix'.
{-# INLINEABLE createNeedlemanWunchMatrix #-}
{-# SPECIALIZE createNeedlemanWunchMatrix :: SymbolAmbiguityGroup -> (SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> (SymbolAmbiguityGroup, Word)) ->  Vector SymbolContext -> Vector SymbolContext -> NeedlemanWunchMatrix SymbolAmbiguityGroup #-}
createNeedlemanWunchMatrix
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     )
  => SymbolAmbiguityGroup
  -> (SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> (SymbolAmbiguityGroup, Word))
  -> f SymbolContext
  -> f SymbolContext
  -> NeedlemanWunchMatrix SymbolAmbiguityGroup
--createNeedlemanWunchMatrix topString leftString overlapFunction = trace renderedMatrix result
createNeedlemanWunchMatrix gap overlapFunction topString leftString = result
  where
    result = fromList (rows', cols')
        [ needlemanWunschDefinition gap overlapFunction topString leftString result (i,j)
        | i <- [0 .. rows' - 1]
        , j <- [0 .. cols' - 1]
        ]
    
--    result             = matrix rows' cols' generatingFunction
    rows'              = length leftString + 1
    cols'              = length topString  + 1
--    generatingFunction = needlemanWunschDefinition gap overlapFunction topString leftString result
--    renderedMatrix     = renderCostMatrix topString leftString result
