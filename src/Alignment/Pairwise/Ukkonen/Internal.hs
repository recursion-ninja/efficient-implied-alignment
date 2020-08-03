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

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Alignment.Pairwise.Ukkonen.Internal
  ( ukkonenDO
  ) where

import           Alignment.Pairwise.Internal
import           Alignment.Pairwise.NeedlemanWunsch (naiveDOMemo)
import           Alignment.Pairwise.Ukkonen.Matrix
import qualified Alignment.Pairwise.Ukkonen.Ribbon  as Ribbon
import           Data.Alphabet
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty                 (NonEmpty (..))
import           Data.Maybe                         (isJust)
import           Data.SymbolString
import           Data.TCM
import           Data.Vector.Instances              ()
import           Data.Vector.NonEmpty               hiding (filter)
import           Numeric.Extended.Natural


-- |
-- /O( (n - m + 1 ) * log(n - m + 1) )/, /n/ >= /m/
--
-- Compute the alignment of two dynamic characters and the median states by
-- using Ukkonen's string edit distance algorthim to improve space and time
-- complexity.
{-# INLINEABLE ukkonenDO #-}
{-# SPECIALIZE ukkonenDO :: Alphabet SymbolAmbiguityGroup -> (SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> (SymbolAmbiguityGroup, Word)) -> Vector SymbolContext -> Vector SymbolContext -> (Word, Vector SymbolContext) #-}
ukkonenDO
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     , Ord s
     )
  => Alphabet s
  -> (SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> (SymbolAmbiguityGroup, Word))
  -> f SymbolContext
  -> f SymbolContext
  -> (Word, Vector SymbolContext)
ukkonenDO alphabet overlapFunction lhs rhs
  | noGainFromUkkonenMethod = naiveDOMemo alphabet overlapFunction lhs rhs
  | otherwise               = directOptimization gapGroup overlapFunction (renderCostMatrix gapGroup) (createUkkonenMethodMatrix coefficient alphabet) lhs rhs
  where
    gap       = gapSymbol alphabet
    gapGroup  = encodeAmbiguityGroup alphabet $ gap:|[]

    (_, lesser, longer) = measureCharacters lhs rhs

    -- If the longer character is 50% larger than the shorter character, then
    -- there is no point in using the barriers. Rather, we fill the full matrix
    -- immediately.
    --
    -- Additionally, if the shorter sequence is of length 4 or less, then the
    -- initial barrier will be set adjacent to or beyond the lower left and
    -- upper right corners.
    --
    -- Lastly, a threshold coeffcient is computed as the minimal indel cost from
    -- any symbol in the alphabet to gap. However, if the indel cost for any
    -- symbol is zero, the algorithm will hang, and a naive approach must be taken.
    --
    -- Do not perform Ukkonen's algorithm if and only if:
    --
    -- > longerLen >= 1.5 * lesserLen
    --     OR
    -- > lesserLen <= 4
    --     OR
    -- > coefficient == 0
    noGainFromUkkonenMethod = or
        [     lesserLen <= 4
        , 2 * longerLen >= 3 * lesserLen
        , coefficient == 0
        ]
      where
        longerLen = length longer
        lesserLen = length lesser

    -- /O(2*(a - 1))/
    --
    -- This was taken from Ukkonen's original 1985 paper wherein the coeffcient
    -- delta @(Δ)@ was defined by the minimum transition cost from any symbol in
    -- the alphabet @(Σ)@ to the gap symbol @'-'@.
    --
    -- If there is any transition to a gap from a non-gap for which the cost is
    -- zero, then this coefficient will be zero. This leaves us with no way to
    -- determine if optimality is preserved, and the Ukkonen algorithm will hang.
    -- Consequently, we do not perform Ukkonen's algorithm if the coefficient is
    -- zero.
    coefficient = minimum $ indelCost <$> nonGapGroups
      where
        nonGapGroups   = fmap (encodeAmbiguityGroup alphabet . (:|[])) nonGapSymbols
        nonGapSymbols  = filter (/= gap) $ alphabetSymbols alphabet
        distance x y   = snd $ overlapFunction x y
        indelCost sym  = min (distance sym gapGroup) (distance gapGroup sym)


-- |
-- /O( (n - m + 1 ) * log(n - m + 1) )/, /n/ >= /m/
--
-- Generates an /optimal/, partially-filled-in matrix using Ukkonen's string
-- edit distance algorithm.
--
-- Note that the threshold value is lowered more than described in Ukkonen's
-- paper. This is to handle input elements that contain a gap. In Ukkonen's
-- original description of the algorithm, there was a subtle assumption that
-- input did not contain any gap symbols.
{-# INLINEABLE createUkkonenMethodMatrix #-}
{-# SPECIALIZE createUkkonenMethodMatrix :: Word -> Alphabet SymbolAmbiguityGroup -> TransitionCostMatrix -> Vector SymbolContext -> Vector SymbolContext -> UkkonenMethodMatrix (Cost, Direction, SymbolAmbiguityGroup) #-}
createUkkonenMethodMatrix
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     , Ord s
     )
  => Word -- ^ Coefficient value, representing the /minimum/ transition cost from a state to gap
  -> Alphabet s
  -> TransitionCostMatrix
  -> f SymbolContext
  -> f SymbolContext
  -> UkkonenMethodMatrix (Cost, Direction, SymbolAmbiguityGroup)
createUkkonenMethodMatrix minimumIndelCost alphabet overlapFunction longerTop lesserLeft = U $ ukkonenUntilOptimal startOffset
  where
    -- General values that need to be in scope for the recursive computations.
    longerLen   = length longerTop
    lesserLen   = length lesserLeft
    coefficient = fromEnum minimumIndelCost
    rows        = toEnum lesserLen + 1
    cols        = toEnum longerLen + 1

    -- We start the offset at two rather than at one so that the first doubling
    -- isn't trivially small.
    startOffset = 2 + gapsPresentInInputs

    -- If we are filling up 3/4 of the matrix, quit Ukkonen method and just do standard Neeleman-Wunsch.
    stopOffset  = (3 * lesserLen - 1) `div` 4

    -- The largest value the offset can be, logically.
    maximumOffset = lesserLen

    -- /O(1)/
    --
    -- Necessary to compute the width of a row in the barrier-constrained matrix.
    quasiDiagonalWidth = differenceInLength + 1
      where
        differenceInLength = longerLen - lesserLen

    -- /O(n + m)/
    --
    -- If one or more of the aligned character elements contained a gap, diagonal
    -- directions in the matrix have an "indel" cost. 'gapsPresentInInputs' is
    -- necessary in order to decrement the threshold value to account for this.
    -- This was not described in Ukkonen's original paper, as the inputs were
    -- assumed not to contain any gaps.
    gapsPresentInInputs = longerGaps + lesserGaps
      where
        longerGaps  = countGaps longerTop
        lesserGaps  = countGaps lesserLeft
        countGaps   = length . filter containsGap . toList
        containsGap Gapping{} = True
        containsGap x         = isJust . (/\ gapGroup) $ symbolAlignmentMedian x

    gapGroup = encodeAmbiguityGroup alphabet $ gapSymbol alphabet :|[]

    ukkonenUntilOptimal inOffset
      | inOffset   >= stopOffset    = ukkonenMatrix
      | threshold <= alignmentCost = ukkonenUntilOptimal $ 2 * offset
      | otherwise                   = ukkonenMatrix
      where
        offset | quasiDiagonalWidth + inOffset >= lesserLen = maximumOffset
               | otherwise = inOffset

        ukkonenMatrix      = Ribbon.generate rows cols generatingFunction  $ toEnum offset

        generatingFunction = needlemanWunschDefinition gapGroup overlapFunction longerTop lesserLeft ukkonenMatrix

        ~(cost, _, _)      = ukkonenMatrix ! (lesserLen, longerLen)
        alignmentCost      = unsafeToFinite cost
        computedValue      = coefficient * (quasiDiagonalWidth + offset - gapsPresentInInputs)
        threshold         = toEnum $ max 0 computedValue -- The threshold value must be non-negative
