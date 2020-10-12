-----------------------------------------------------------------------------
-- |
-- Module      :  Alignment.Pairwise.UnboxedUkkonen
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

{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnboxedTuples       #-}

module Alignment.Pairwise.UnboxedUkkonen
  ( unboxedUkkonenDO
  ) where

import           Alignment.Pairwise.Internal (Direction(..), insertGaps, measureCharacters, measureAndUngapCharacters)
import           Control.DeepSeq
import           Control.Monad               (when, unless)
import           Control.Monad.Loops         (iterateUntilM, whileM_)
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Alphabet
import           Data.Bits
import           Data.Coerce
import           Data.DList                  (snoc)
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty          (NonEmpty(..))
import qualified Data.List.NonEmpty          as NE
import           Data.Matrix.Unboxed         (Matrix, unsafeFreeze, unsafeIndex)
--import qualified Data.Matrix.Unboxed         as Z
import           Data.Matrix.Unboxed.Mutable (MMatrix)
import qualified Data.Matrix.Unboxed.Mutable as M
import           Data.STRef
import           Data.SymbolString
import           Data.TCM
import qualified Data.Vector.NonEmpty        as NEV
import qualified Data.Vector.Unboxed.Mutable as V
import           Data.Word                   (Word16)
import           Prelude                     hiding (lookup, zipWith)


-- |
-- Performs a naive direct optimization.
-- Takes in two characters to run DO on and an overlap function
-- Returns an assignment character, the cost of that assignment, the assignment
-- character with gaps included, the aligned version of the first input character,
-- and the aligned version of the second input character. The process for this
-- algorithm is to generate a traversal matrix, then perform a traceback.
{-# SCC unboxedUkkonenDO #-}
{-# INLINE unboxedUkkonenDO #-}
{-# SPECIALISE unboxedUkkonenDO :: Eq a => Alphabet a -> TransitionCostMatrix -> SymbolString -> SymbolString -> (Word, SymbolString) #-}
unboxedUkkonenDO
  :: ( Eq a
     , Foldable f
     , Indexable f
     , Key f ~ Int
     )
  => Alphabet a           -- ^ Alphabet of symbols
  -> TransitionCostMatrix -- ^ Structure defining the transition costs between character states
  -> f SymbolContext      -- ^ First  dynamic character
  -> f SymbolContext      -- ^ Second dynamic character
  -> (Word, SymbolString) -- ^ The cost of the alignment and the alignment context
unboxedUkkonenDO alphabet tcm char1 char2
  | noGainFromUkkonenMethod = buildFullMatrix
  | otherwise               = directOptimization gap tcm buildPartialMatrixMaybe char1 char2
  where
    (_, lesser, longer) = measureCharacters char1 char2

    gap = encodeAmbiguityGroup alphabet $ gapSymbol alphabet :| []

    cost x y = snd $ tcm x y 

    buildFullMatrix = unboxedSwappingDO gap tcm char1 char2

    buildPartialMatrixMaybe = createUkkonenMethodMatrix gap coefficient gapsPresentInInputs tcm

    -- /O(1)/
    --
    -- If the longer character is 50% larger than the shorter character, then
    -- there is no point in using the barriers. Rather, we fill the full matrix
    -- immediately.
    --
    -- Additionally, if the shorter sequence is of length 4 or less, then the
    -- initial barrier will be set adjacent to or beyond the lower left and
    -- upper right corners.
    --
    -- Also, a threshold coeffcient is computed as the minimal indel cost from
    -- any symbol in the alphabet to gap. However, if the indel cost for any
    -- symbol is zero, the algorithm will hang, and a naive approach must be taken.
    --
    -- Lastly, if the sum of the gaps in both strings is equal to or exceeds the
    -- length of the longer string, then the threshold criteria will never be met
    -- by definition.
    --
    -- Do not perform Ukkonen's algorithm if and only if:
    --
    -- > longerLen >= 1.5 * lesserLen
    --     OR
    -- > lesserLen <= 4
    --     OR
    -- > coefficient == 0
    --     OR
    -- > gapsPresentInInputs >= longerLen
    noGainFromUkkonenMethod =     lesserLen <= 4
                           || 2 * longerLen >= 3 * lesserLen
                           || coefficient == 0
                           || gapsPresentInInputs >= longerLen
      where
        longerLen = toEnum $ length longer
        lesserLen = toEnum $ length lesser

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
    coefficient = minimum $ indelCost <$> nonGapElements
      where
        alphabetSize   = length alphabet
        nonGapElements = [ 0 .. alphabetSize - 2 ]
        indelCost i    = min (cost (bit i)  gap   )
                             (cost  gap    (bit i))

    -- /O(n + m)/
    --
    -- If one or more of the aligned character elements contained a gap, diagonal
    -- directions in the matrix have an "indel" cost. 'gapsPresentInInputs' is
    -- necessary in order to decrement the threshold value to account for this.
    -- This was not described in Ukkonen's original paper, as the inputs were assumed
    -- not to contain any gaps.
    gapsPresentInInputs = char1Gaps + char2Gaps
      where
        char1Gaps = toEnum $ countGaps char1
        char2Gaps = toEnum $ countGaps char2
        countGaps = length . filter (hasGap . symbolAlignmentMedian) . toList
        hasGap b  = coerce (b .&. gap) /= (0 :: Word16)


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
{-# SCC createUkkonenMethodMatrix #-}
{-# INLINE createUkkonenMethodMatrix #-}
{-# SPECIALISE createUkkonenMethodMatrix :: SymbolAmbiguityGroup -> Word -> Word -> TransitionCostMatrix -> SymbolString -> SymbolString -> (Word, Matrix Direction) #-}
createUkkonenMethodMatrix
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     )
  => SymbolAmbiguityGroup -- ^ Gap
  -> Word -- ^ Coefficient value, representing the /minimum/ transition cost from a state to gap
  -> Word -- ^ Gaps present in input
  -> TransitionCostMatrix
  -> f SymbolContext -- ^ First  (shorter) dynamic character
  -> f SymbolContext -- ^ Second (longer) dynamic character
  -> (Word, Matrix Direction)
createUkkonenMethodMatrix gap minimumIndelCost gapsPresentInInputs tcm lesserLeft longerTop = finalMatrix
  where
    -- General values that need to be in scope for the recursive computations.
    longerLen   = length longerTop
    lesserLen   = length lesserLeft

    -- We start the offset at two rather than at one so that the first doubling
    -- isn't trivially small.
    startOffset = 2 + gapsPresentInInputs

    -- /O(1)/
    --
    -- Necessary to compute the width of a row in the barrier-constrained matrix.
    quasiDiagonalWidth = toEnum $ differenceInLength + 1
      where
        differenceInLength = longerLen - lesserLen

    needToResizeBand :: forall s. MMatrix s Word -> STRef s Word -> ST s Bool
    needToResizeBand mCost offsetRef = do
        offset        <- readSTRef offsetRef
        if   quasiDiagonalWidth + offset > toEnum longerLen
        then pure False
        else do
                alignmentCost <- M.unsafeRead mCost (lesserLen, longerLen)
                let threshold -- The threshold value must be non-negative
                      | quasiDiagonalWidth + offset <= gapsPresentInInputs = 0
                      | otherwise = minimumIndelCost * (quasiDiagonalWidth + offset - gapsPresentInInputs)
                pure $ threshold <= alignmentCost
      
    finalMatrix = runST $ do
        (mCost, mDir) <- buildInitialBandedMatrix gap tcm lesserLeft longerTop startOffset
        offsetRef <- newSTRef startOffset
        whileM_ (needToResizeBand mCost offsetRef) $ do
          previousOffset <- readSTRef offsetRef
          let currentOffset = previousOffset `shiftL` 1 -- Multiply by 2
          writeSTRef offsetRef currentOffset
          expandBandedMatrix gap tcm lesserLeft longerTop mCost mDir previousOffset currentOffset

        c <- M.unsafeRead mCost (lesserLen, longerLen)
        m <- unsafeFreeze mDir

        pure (c, m)

               
{-# SCC        buildInitialBandedMatrix #-}
{-# INLINABLE  buildInitialBandedMatrix #-}
{-# SPECIALISE buildInitialBandedMatrix :: forall s. SymbolAmbiguityGroup -> TransitionCostMatrix -> SymbolString -> SymbolString -> Word -> ST s (MMatrix s Word, MMatrix s Direction) #-}
buildInitialBandedMatrix
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     )
  => SymbolAmbiguityGroup -- ^ Gap
  -> TransitionCostMatrix
  -> f SymbolContext -- ^ First  (shorter) dynamic character
  -> f SymbolContext -- ^ Second (longer)  dynamic character
  -> Word
  -> ST s (MMatrix s Word, MMatrix s Direction)
buildInitialBandedMatrix gap tcm lesserLeft longerTop o = fullMatrix
  where
    -- Note: "offset" cannot cause "width" to exceed "cols"
    (offset, cost, rows, cols, width, quasiDiagonalWidth) = ukkonenConstants tcm lesserLeft longerTop o

    fullMatrix = do
      
      ---------------------------------------
      -- Allocate required space           --
      ---------------------------------------

      mCost <- M.new (rows, cols)
      mDir  <- M.new (rows, cols)

      ---------------------------------------
      -- Define some generalized functions --
      ---------------------------------------
      let (write, internalCell, leftColumn, leftBoundary, rightBoundary, rightColumn) = cellDefinitions gap longerTop cost tcm mCost mDir

      -- Define how to compute values to an entire row of the Ukkonen matrix.
      let writeRow i =
            -- Precomute some values that will be used for the whole row
            let start = max  0         $ i - offset
                stop  = min (cols - 1) $ i - offset + width - 1
                leftElement = symbolAlignmentMedian $ lesserLeft ! (i - 1)
                insertCost  = cost gap leftElement
                firstCell
                  | i <= offset = leftColumn
                  | otherwise   = leftBoundary

                finalCell
                  | i <= cols - quasiDiagonalWidth - offset = rightBoundary
                  | otherwise = rightColumn
            in  do -- Write to the first cell of the Ukkonen band
                   firstCell leftElement insertCost i start >>= write (i, start)
                   -- Write to the all the intermediary cells of the Ukkonen band
                   for_ [start + 1 .. stop - 1] $ \j ->
                       internalCell leftElement insertCost i j >>= write (i, j)
                   -- Write to the last cell of the Ukkonen band
                   finalCell leftElement insertCost i stop >>= write (i, stop)

      ---------------------------------------
      -- Compute all values of the matrix  --
      ---------------------------------------

      -- Write to the origin to seed the first row.
      write (0, 0) (0, DiagArrow)

      -- Write the first row to seed subsequent rows.
      for_ [1 .. min (cols - 1) (width - offset - 1)] $ \j ->
        let topElement    = symbolAlignmentMedian $ longerTop ! (j - 1)
            firstCellCost = cost gap topElement
        in  do firstPrevCost <- M.unsafeRead mCost (0, j - 1)
               write (0,j) (firstCellCost + firstPrevCost, LeftArrow)

      -- Loop through the remaining rows.
      for_ [1 .. rows - 1] writeRow

      -- Return the matricies for possible expansion
      pure (mCost, mDir)



{-# SCC        expandBandedMatrix #-}
{-# INLINABLE  expandBandedMatrix #-}
{-# SPECIALISE expandBandedMatrix :: forall s. SymbolAmbiguityGroup -> TransitionCostMatrix -> SymbolString -> SymbolString -> MMatrix s Word -> MMatrix s Direction ->  Word -> Word -> ST s () #-}
-- |
-- Given a partially computed alignment matrix,
-- will expand the computed region to the new specified offset.
--
--
-- Dimensions: 13 ⨉ 17
--  ⊗ ┃  ⁎ α1 α2 α3 α4 α5 α6 α7 α8 α9 α0 α1 α2 α3 α4 α5 α6 
-- ━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--  ⁎ ┃ 0↖ 0← 0← 0← 0← 0← 0← 0← 0← 0← 0← 0← 0← 0← 0← 0← 0← 
-- α1 ┃ 0↑ 0↖ 0↖ 0↖ 0← 0← 0← 0← 0← 0← 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0← 
-- α2 ┃ 0↑ 0↖ 0↖ 0↖ 0← 0← 0← 0← 0← 0← 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0← 
-- α3 ┃ 0↑ 0↑ 0↑ 0↖ 0↖ 0↖ 0← 0← 0← 0← 0← 0← 0← 0← 0← 0← 0↖ 
-- α4 ┃ 0↑ 0↑ 0↑ 0↖ 0↖ 0← 0↖ 0← 0↖ 0← 0← 0← 0← 0← 0← 0← 0← 
-- α5 ┃ 0↑ 0↑ 0↑ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0← 0← 0← 0← 0← 0← 0← 0← 
-- α6 ┃ 0↑ 0↖ 0↖ 0↖ 0↑ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0← 
-- α7 ┃ 0↑ 0↑ 0↑ 0↑ 0↖ 0↖ 0↖ 0↖ 0↖ 0← 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 
-- α8 ┃ 0↑ 0↑ 0↑ 0↑ 0↑ 0↖ 0← 0↖ 0↑ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 
-- α9 ┃ 0↑ 0↑ 0↑ 0↑ 0↖ 0↑ 0↖ 0← 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 
-- α0 ┃ 0↑ 0↑ 0↑ 0↑ 0↑ 0↖ 0↑ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖
-- α1 ┃ 0↑ 0↑ 0↑ 0↑ 0↖ 0↑ 0↖ 0↖ 0↖ 0← 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖
-- α2 ┃ 0↑ 0↖ 0↖ 0↖ 0↑ 0↑ 0↑ 0↖ 0↑ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖ 0↖
--
--      ┌───────────────w───────────────┐
--      │              ┏━━━━━━━co━━━━━━━┪
--      ┢━━━━━qd━━━━━━┓┠─po─┐┌────Δo────┨
--  ⊗ ┃ ┃0  1  2  3  4┃┃5  6││7  8  9 10┃11 12 13 14 15 16 
-- ━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--  0 ┃ ██ ██ ██ ██ ██ ▓▓ ▓▓ ▒▒ ▒▒ ▒▒ ▒▒
--  1 ┃ ▓▓ ██ ██ ██ ██ ██ ▓▓ ▓▓ ▒▒ ▒▒ ▒▒ ▒▒
--  2 ┃ ▓▓ ▓▓ ██ ██ ██ ██ ██ ▓▓ ▓▓ ▒▒ ▒▒ ▒▒ ▒▒
--  3 ┃ ▒▒ ▓▓ ▓▓ ██ ██ ██ ██ ██ ▓▓ ▓▓ ▒▒ ▒▒ ▒▒ ▒▒
--  4 ┃ ▒▒ ▒▒ ▓▓ ▓▓ ██ ██ ██ ██ ██ ▓▓ ▓▓ ▒▒ ▒▒ ▒▒ ▒▒
--  5 ┃ ▒▒ ▒▒ ▒▒ ▓▓ ▓▓ ██ ██ ██ ██ ██ ▓▓ ▓▓ ▒▒ ▒▒ ▒▒ ▒▒
--  6 ┃ ▒▒ ▒▒ ▒▒ ▒▒ ▓▓ ▓▓ ██ ██ ██ ██ ██ ▓▓ ▓▓ ▒▒ ▒▒ ▒▒ ▒▒ 
--  7 ┃    ▒▒ ▒▒ ▒▒ ▒▒ ▓▓ ▓▓ ██ ██ ██ ██ ██ ▓▓ ▓▓ ▒▒ ▒▒ ▒▒
--  8 ┃       ▒▒ ▒▒ ▒▒ ▒▒ ▓▓ ▓▓ ██ ██ ██ ██ ██ ▓▓ ▓▓ ▒▒ ▒▒
--  9 ┃          ▒▒ ▒▒ ▒▒ ▒▒ ▓▓ ▓▓ ██ ██ ██ ██ ██ ▓▓ ▓▓ ▒▒ 
--  0 ┃             ▒▒ ▒▒ ▒▒ ▒▒ ▓▓ ▓▓ ██ ██ ██ ██ ██ ▓▓ ▓▓
--  1 ┃                ▒▒ ▒▒ ▒▒ ▒▒ ▓▓ ▓▓ ██ ██ ██ ██ ██ ▓▓
--  2 ┃                   ▒▒ ▒▒ ▒▒ ▒▒ ▓▓ ▓▓ ██ ██ ██ ██ ██
--
--
-- w  : Width
-- qd : Quasi-diagonal
-- co : Current Offset
-- po : Previous Offset
-- Δo : Difference in Offset
--
-- Note:
-- w  = qd + co
-- co = po + Δo
--
-- And often:
-- co = 2*po = 2*Δo
--
-- ██ : The core band
--       * Previously computed, sections may need to be recomputed
-- 
-- ▓▓ : The previous extension
--       * Previously computed, sections may need to be recomputed
--
-- ▒▒ : The new extension
--       * Needs to be computed
--
expandBandedMatrix
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     )
  => SymbolAmbiguityGroup -- ^ Gap
  -> TransitionCostMatrix
  -> f SymbolContext -- ^ First  (shorter) dynamic character
  -> f SymbolContext -- ^ Second (longer)  dynamic character
  -> MMatrix s Word
  -> MMatrix s Direction
  -> Word
  -> Word
  -> ST s ()
expandBandedMatrix gap tcm lesserLeft longerTop mCost mDir po co = updateBand
  where    
    -- Note: "offset" cannot cause "width + quasiDiagonalWidth" to exceed "2 * cols"
    (offset, cost, rows, cols, width, quasiDiagonalWidth) = ukkonenConstants tcm lesserLeft longerTop co
    prevOffset = fromEnum po    
    w  = width
    qd = quasiDiagonalWidth

    updateBand = do

      ---------------------------------------
      -- Allocate mutable state variables  --
      ---------------------------------------

      tailStart <- newSTRef cols

      t0' <- newSTRef (-1)
      t1' <- newSTRef $ qd + fromEnum po

      ---------------------------------------
      -- Define some generalized functions --
      ---------------------------------------

      let (write, internalCell, leftColumn, leftBoundary, rightBoundary, rightColumn) = cellDefinitions gap longerTop cost tcm mCost mDir

      let computeCell leftElement insertCost i j = {-# SCC recomputeCell #-} do
            e@(c,_) <-internalCell leftElement insertCost i j
            oldCost <- M.unsafeRead mCost (i    , j    )
            write (i,j) e
            pure (c == oldCost, j+1)

      let recomputeRange leftElement insertCost i x y = do
            lastDiff <- newSTRef 0
            for_ [x .. y] $ \j -> do
              (same, _) <- computeCell leftElement insertCost i j
              unless same $ writeSTRef lastDiff j
            readSTRef lastDiff

      -- Define how to compute values to an entire row of the Ukkonen matrix.
      let extendRow i =
            -- Precomute some values that will be used for the whole row
            let start0 =  max 0          $ i - offset
                start3 =  min  cols      $ i + w - offset - prevOffset - 1
                goUpTo =  max 0          ( i - prevOffset) - 1
                stop   =  min (cols - 1) $ i + w - offset - 1
                leftElement = symbolAlignmentMedian $ lesserLeft ! (i - 1)
                insertCost  = cost gap leftElement
                firstCell
                  | i <= offset = leftColumn
                  | otherwise   = leftBoundary

                lastCell
                  | i <= cols - quasiDiagonalWidth - offset = rightBoundary
                  | otherwise = rightColumn

                b0 = start0
                e0 = goUpTo
                b1 = start3
                e1 = stop

                continueRecomputing (same, j) = same || j >= stop
                computeCell' ~(_,j) = computeCell leftElement insertCost i j
                internalCell' j = internalCell leftElement insertCost i j >>= write (i,j)
                recomputeUntilSame j = snd <$> iterateUntilM continueRecomputing computeCell' (False, j)
            in  do -- First, we fill in 0 or more cells of the left region of
                   -- the expanded band. This is the region [b0, e0] computed
                   -- above. 
                   --  ⊗ ┃  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
                   -- ━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                   --  0 ┃ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  1 ┃ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  2 ┃ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  3 ┃ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  4 ┃ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --      b0    e0
                   --     ┏━━━━━━━━┓
                   --  5 ┃ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --      
                   --  6 ┃ ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  7 ┃    ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒
                   --  8 ┃       ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒
                   --  9 ┃          ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒
                   -- 10 ┃             ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██
                   -- 11 ┃                ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██
                   -- 12 ┃                   ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██
                   --

                   -- Conditionally write to the first cell of the Ukkonen band
                   if   i > prevOffset
                   then firstCell leftElement insertCost i start0 >>= write (i, b0)
                   else pure ()

                   for_ [b0+1 .. e0] internalCell'

                   -- Next, we assign to s0 the value t0 + 1 from the previous row.
                   -- The cell t0 is up to where the values were recomputed in
                   -- the previous row. We add 1 to t0 because the a cell in the
                   -- current row might use t0 in a *diagonal* computation.
                   -- We recompute the cells in the range [e0 + 1, s0].
                   -- We assign to t0 the last cell in the range [s1, s2] which
                   -- was updated for the next row.
                   -- We remember cells t0 for the next row.
                   --  ⊗ ┃  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
                   -- ━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                   --  0 ┃ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  1 ┃ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  2 ┃ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  3 ┃ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  4 ┃ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --            e0    s0
                   --              ┏━━━━━┓
                   --  5 ┃ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --      
                   --  6 ┃ ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  7 ┃    ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒
                   --  8 ┃       ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒
                   --  9 ┃          ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒
                   -- 10 ┃             ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██
                   -- 11 ┃                ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██
                   -- 12 ┃                   ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██
                   --
                   s0 <- (\x -> min (x+1) e1) <$> readSTRef t0'
                   writeSTRef t0' (-1)

                   when (s0 > e0 && toEnum i > po) $
                       recomputeRange leftElement insertCost i (e0+1) s0 >>= writeSTRef t0'
                   t0 <- readSTRef t0'

                   -- If s0 = t0, we recompute the cell (s0 + 1).
                   -- If the cost is the same, we stop here and remember the cell
                   -- before we stopped.
                   -- If the cost is not the same, we update cell (s0 + 1) and
                   -- move on to (s0 + 2).
                   -- This proceedure continues until (s0 + n) has the same cost
                   -- as before, or *until we reach b1.*
                   -- We remember the cell (s0 + n - 1) as t0 for the next row.
                   --  ⊗ ┃  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
                   -- ━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                   --  0 ┃ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  1 ┃ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  2 ┃ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  3 ┃ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  4 ┃ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --                  s0    t0
                   --                    ╔═════╗
                   --  5 ┃ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --      
                   --  6 ┃ ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  7 ┃    ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒
                   --  8 ┃       ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒
                   --  9 ┃          ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒
                   -- 10 ┃             ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██
                   -- 11 ┃                ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██
                   -- 12 ┃                   ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██
                   --
                   if      s0 == t0 && s0 > 0
                   then recomputeUntilSame (s0 + 1) >>= writeSTRef t0' . pred
                   else if s0 <= e0 && e0 > 0
                   then recomputeUntilSame (e0 + 1) >>= writeSTRef t0' . pred
                   else pure ()

                   -- Next, we assign to s1 the value t1 from the previous row.
                   -- We recompute the cells in the range [s1, b1 - 1].
                   -- If any cell in the range was updated, we assign to s1 to t1.
                   -- We remember cell t1 for the next row.
                   --  ⊗ ┃  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
                   -- ━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                   --  0 ┃ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  1 ┃ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  2 ┃ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  3 ┃ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  4 ┃ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --                                 s1       b1
                   --                                ┏━━━━━━━━┓
                   --  5 ┃ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --      
                   --  6 ┃ ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  7 ┃    ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒
                   --  8 ┃       ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒
                   --  9 ┃          ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒
                   -- 10 ┃             ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██
                   -- 11 ┃                ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██
                   -- 12 ┃                   ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██
                   --
                   s1 <- readSTRef t1'
                   t1 <- recomputeRange leftElement insertCost i s1 $ b1 - 1
                   -- If no cells were updated, a zero value is returned.
                   -- In this case, the "last" updated cell for the next row is b1.
                   writeSTRef t1' $ if t1 == 0 then b1 else s1

                   -- Lastly, we fill in 0 or more cells of the left region of
                   -- the expanded band. This is the region [b1, e1] computed
                   -- above. 
                   --  ⊗ ┃  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
                   -- ━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                   --  0 ┃ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  1 ┃ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  2 ┃ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  3 ┃ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  4 ┃ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --                                          b1       e1
                   --                                         ┏━━━━━━━━━━━┓
                   --  5 ┃ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --      
                   --  6 ┃ ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒ ▒▒
                   --  7 ┃    ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒ ▒▒
                   --  8 ┃       ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒ ▒▒
                   --  9 ┃          ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██ ▒▒
                   -- 10 ┃             ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██ ██
                   -- 11 ┃                ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██ ██
                   -- 12 ┃                   ▒▒ ▒▒ ▒▒ ▒▒ ██ ██ ██ ██ ██ ██ ██
                   --                   
                   for_ [b1 .. e1 - 1] internalCell'

                   -- Conditionally write to the last cell of the Ukkonen band
                   when (i < rows - fromEnum po) $
                     lastCell leftElement insertCost i stop >>= write (i, stop)

      ---------------------------------------
      -- Compute all values of the matrix  --
      ---------------------------------------

      let start = quasiDiagonalWidth + prevOffset

      -- Extend the first row to seed subsequent rows.
      for_ [start .. min (cols - 1) (w - offset - 1)] $ \j ->
        let topElement    = symbolAlignmentMedian $ longerTop ! (j - 1)
            firstCellCost = cost gap topElement
        in  do firstPrevCost <- M.unsafeRead mCost (0, j - 1)
               write (0,j) (firstCellCost + firstPrevCost, LeftArrow)

      writeSTRef tailStart start

      -- Loop through the remaining rows.
      for_ [1 .. rows - 1] extendRow


-- |
-- Performs a naive direct optimization.
-- Takes in two characters to run DO on and an overlap function
-- Returns an assignment character, the cost of that assignment, the assignment
-- character with gaps included, the aligned version of the first input character,
-- and the aligned version of the second input character. The process for this
-- algorithm is to generate a traversal matrix, then perform a traceback.
{-# SCC unboxedSwappingDO #-}
{-# INLINE unboxedSwappingDO #-}
{-# SPECIALISE unboxedSwappingDO :: SymbolAmbiguityGroup -> TransitionCostMatrix -> SymbolString -> SymbolString -> (Word, SymbolString) #-}
unboxedSwappingDO
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     )
  => SymbolAmbiguityGroup -- ^ Gap
  -> TransitionCostMatrix -- ^ Structure defining the transition costs between character states
  -> f SymbolContext      -- ^ First  dynamic character
  -> f SymbolContext      -- ^ Second dynamic character
  -> (Word, SymbolString) -- ^ The cost of the alignment and the alignment context
unboxedSwappingDO gap tcm = directOptimization gap tcm (buildFullDirectionMatrix gap tcm)


buildFullDirectionMatrix
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     )
  => SymbolAmbiguityGroup     -- ^ Gap
  -> TransitionCostMatrix     -- ^ Structure defining the transition costs between character states
  -> f SymbolContext          -- ^ First  dynamic character
  -> f SymbolContext          -- ^ Second dynamic character
  -> (Word, Matrix Direction) -- ^ The cost of the alignment and the direction matrix
buildFullDirectionMatrix gap tcm leftChar topChar = fullMatrix
  where
    cost x y   = snd $ tcm x y
    rows       = length leftChar + 1
    cols       = length topChar  + 1

    fullMatrix = runST $ do
      mDir <- M.new (rows, cols)
      vOne <- V.new cols
      vTwo <- V.new cols

      let write v p@(_,!j) !c !d = V.unsafeWrite v j c *> M.unsafeWrite mDir p d

      write vOne (0,0) 0 DiagArrow

      -- Special case the first row
      -- We need to ensure that there are only Left Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 1,
      -- since the diagonal and upward values are "out of bounds."
      for_ [1 .. cols - 1] $ \j ->
        let topElement    = symbolAlignmentMedian $ topChar ! (j - 1)
            firstCellCost = cost gap topElement
        in  do firstPrevCost <- V.unsafeRead vOne (j - 1)
               write vOne (0,j) (firstCellCost + firstPrevCost) LeftArrow

      for_ [1 .. rows - 1] $ \i ->
        let (prev, curr)
              | odd i     = (vOne, vTwo)
              | otherwise = (vTwo, vOne)
            leftElement   = symbolAlignmentMedian $ leftChar ! (i - 1)
            -- Special case the first cell of each row
            -- We need to ensure that there are only Up Arrow values in the directional matrix.
            -- We can also reduce the number of comparisons the first row makes from 3 to 1,
            -- since the diagonal and leftward values are "out of bounds."
            firstCellCost = cost leftElement gap 
        in  do firstPrevCost <- V.unsafeRead prev 0
               write curr (i,0) (firstCellCost + firstPrevCost) UpArrow
               -- Finish special case for first cell of each row
               -- Begin processing all other cells in the curr vector
               for_ [1 .. cols - 1] $ \j ->
                 let topElement  = symbolAlignmentMedian $ topChar ! (j - 1)
                     deleteCost  = cost topElement gap
                     (alignElem, alignCost) = tcm topElement leftElement
                     insertCost  = cost gap        leftElement
                 in  do diagCost <- V.unsafeRead prev $ j - 1
                        topCost  <- V.unsafeRead prev   j
                        leftCost <- V.unsafeRead curr $ j - 1
                        let xs = [ ( alignCost + diagCost, DiagArrow)
                                 , (deleteCost + leftCost, LeftArrow)
                                 , (insertCost +  topCost, UpArrow  )
                                 ]
                        let (c,d) = getMinimalResult gap alignElem xs
                        write curr (i,j) c d

      let v | odd  rows = vOne
            | otherwise = vTwo
      c <- V.unsafeRead v (cols - 1)
      m <- unsafeFreeze mDir
      pure (c, m)


directOptimization
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     )
  => SymbolAmbiguityGroup     -- ^ Gap
  -> TransitionCostMatrix     -- ^ Structure defining the transition costs between character states
  -> (SymbolString -> SymbolString -> (Word, Matrix Direction))
  -> f SymbolContext          -- ^ First  dynamic character
  -> f SymbolContext          -- ^ Second dynamic character
  -> (Word, SymbolString)
directOptimization gap overlapλ matrixFunction char1 char2 =
    let (swapped, gapsLesser, gapsLonger, shorterChar, longerChar) = measureAndUngapCharacters gap char1 char2
        (alignmentCost, ungappedAlignment) =
          case (shorterChar, longerChar) of
            (Nothing, Just ys) ->
                let f x = let m = symbolAlignmentMedian x in insertElement (fst $ overlapλ m gap) m
                in  (0, Just $ f <$> ys)
            (Just xs, Just ys) ->
                let (cost, traversalMatrix) = matrixFunction xs ys
                    uncommutedContext = traceback gap overlapλ traversalMatrix xs ys
                in  (cost, Just uncommutedContext)
            _                  -> (0, Nothing)

        transformation    = if swapped then fmap reverseContext else id
        regappedAlignment = insertGaps gap gapsLesser gapsLonger ungappedAlignment
        alignmentContext  = transformation regappedAlignment
    in  (alignmentCost, alignmentContext)


{-# SCC traceback #-}
traceback
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     )
  => SymbolAmbiguityGroup     -- ^ Gap
  -> TransitionCostMatrix
  -> Matrix Direction
  -> f SymbolContext          -- ^ First  (shorter) dynamic character
  -> f SymbolContext          -- ^ Second (longer)  dynamic character
  -> SymbolString
traceback gap overlapFunction alignMatrix lesserChar longerChar = force alignmentContext
  where
    f x y = fst $ overlapFunction x y

    alignmentContext = dlistToDynamic $ go startPoint
    dlistToDynamic = NEV.fromNonEmpty . NE.fromList . toList

    longerLen = length longerChar
    lesserLen = length lesserChar
    rows      = lesserLen + 1
    cols      = longerLen + 1

    startPoint = (rows - 1, cols - 1)

    go !p@(~(!i, !j))
      | p == (0,0) = mempty
      | otherwise  =
        let previousSequence = go (row', col')

            directionArrow = unsafeIndex alignMatrix p

            (# !row', !col', !localContext #) =
                case directionArrow of
                  LeftArrow -> let j' = j - 1
                                   te = symbolAlignmentMedian $ longerChar ! j'
                                   e  = insertElement (f te gap) te
                               in (# i , j', e #)
                  UpArrow   -> let i' = i - 1
                                   le = symbolAlignmentMedian $ lesserChar ! i'
                                   e  = deleteElement (f gap le) le
                               in (# i', j, e #)
                  DiagArrow -> let i' = i - 1
                                   j' = j - 1
                                   te = symbolAlignmentMedian $ longerChar ! j'
                                   le = symbolAlignmentMedian $ lesserChar ! i'
                                   e  = alignElement (f te le) te le
                               in (# i', j', e #)

        in  previousSequence `snoc` localContext


getMinimalResult
  :: Foldable f
  => SymbolAmbiguityGroup
  -> SymbolAmbiguityGroup
  -> f (Word, Direction)
  -> (Word, Direction)
getMinimalResult gap alignElem opts =
    let v@(~(c,d)) = minimum opts
    in  if   d == DiagArrow && alignElem == gap
        then (c, LeftArrow)
        else v


{--
-- |
-- Serializes an alignment matrix to a 'String'. Uses input characters for row
-- and column labelings.
--
-- Useful for debugging purposes.
renderCostMatrix
  :: Foldable f
  => SymbolAmbiguityGroup
  -> f SymbolContext
  -> f SymbolContext
  -> Z.Matrix Word
  -> Z.Matrix Direction
  -> String
renderCostMatrix gapGroup lhs rhs cMat dMat = unlines
    [ dimensionPrefix
    , headerRow
    , barRow
    , renderedRows
    ]
  where
    (_,lesser,longer) = measureCharacters lhs rhs
    longerTokens      = toShownIntegers longer
    lesserTokens      = toShownIntegers lesser
    toShownIntegers   = mapWithKey (\k -> (<> show ((k+1) `mod` 10)) . renderContext) . toList
    matrixTokens      = fmap (fmap showCell) . Z.toLists $ Z.zip cMat dMat
    showCell (c,d)    = show c <> show d
    maxPrefixWidth    = maxLengthOf lesserTokens
    maxColumnWidth    = max (maxLengthOf longerTokens) . maxLengthOf $ fold matrixTokens
    maxLengthOf       = maximum . fmap length

    colCount = length longer + 1
    rowCount = length lesser + 1

    dimensionPrefix  = " " <> unwords
        [ "Dimensions:"
        , show rowCount
        , "⨉"
        , show colCount
        ]

    headerRow = fold
        [ " "
        , pad maxPrefixWidth "⊗"
        , "┃ "
        , pad maxColumnWidth "⁎"
        , concatMap (pad maxColumnWidth) longerTokens
        ]

    barRow    = fold
        [ " "
        , bar maxPrefixWidth
        , "╋"
        , concatMap (const (bar maxColumnWidth)) $ undefined : longerTokens
        ]
      where
        bar n = replicate (n+1) '━'

    renderedRows = unlines . zipWithKey renderRow ("⁎":lesserTokens) $ matrixTokens
      where
        renderRow _k e vs = " " <> pad maxPrefixWidth e <> "┃ " <> concatMap (pad maxColumnWidth) vs

    renderContext (Align  x _ _) = if x == gapGroup then "—" else "α"
    renderContext (Delete x _  ) = if x == gapGroup then "—" else "δ"
    renderContext (Insert x   _) = if x == gapGroup then "—" else "ι"
    renderContext Gapping{}      = "—"

    pad :: Int -> String -> String
    pad n e = replicate (n - len) ' ' <> e <> " "
      where
        len = length e
--}

ukkonenConstants
  :: Foldable f
  => (SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> (SymbolAmbiguityGroup, Word))
  -> f a
  -> f a
  -> Word
  -> (Int, SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> Word, Int, Int, Int, Int)
ukkonenConstants tcm lesserLeft longerTop o =
    (offset, cost, rows, cols, width, quasiDiagonalWidth)
  where    
    -- Note: "offset" cannot cause "width + quasiDiagonalWidth" to exceed "2 * cols"
    offset      = let o' = fromEnum o in  min o' $ cols - quasiDiagonalWidth
    cost x y    = snd $ tcm x y 
    longerLen   = length longerTop
    lesserLen   = length lesserLeft
    rows        = length lesserLeft + 1
    cols        = length longerTop  + 1
    width       = quasiDiagonalWidth + (offset `shiftL` 1) -- Multiply by 2
    quasiDiagonalWidth = differenceInLength + 1
      where
        differenceInLength = longerLen - lesserLen


cellDefinitions
  :: ( Indexable f
     , Key f ~ Int
     , PrimMonad s
     )
  => SymbolAmbiguityGroup
  -> f SymbolContext
  -> (SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> Word)
  -> (SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> (SymbolAmbiguityGroup, Word))
  -> MMatrix (PrimState s) Word
  -> MMatrix (PrimState s) Direction
  -> ( (Int, Int) -> (Word, Direction) -> s ()
     , SymbolAmbiguityGroup -> Word -> Int -> Int -> s (Word, Direction)
     , SymbolAmbiguityGroup -> Word -> Int -> Int -> s (Word, Direction)
     , SymbolAmbiguityGroup -> Word -> Int -> Int -> s (Word, Direction)
     , SymbolAmbiguityGroup -> Word -> Int -> Int -> s (Word, Direction)
     , SymbolAmbiguityGroup -> Word -> Int -> Int -> s (Word, Direction)
     )
cellDefinitions gap longerTop cost tcm mCost mDir =
    (write, internalCell, leftColumn, leftBoundary, rightBoundary, rightColumn)
  where
      ---------------------------------------
      -- Define some generalized functions --
      ---------------------------------------

      -- Write to a single cell of the current vector and directional matrix simultaneously
      write !p ~(!c, !d) = M.unsafeWrite mCost p c *> M.unsafeWrite mDir p d

      -- Write to an internal cell (not on a boundary) of the matrix.
      internalCell leftElement insertCost i j =
            let topElement = symbolAlignmentMedian $ longerTop ! (j - 1)
                -- Preserve the gap in the top (longer) string
                -- NOTE: Should we still do this branch?
            in  if topElement == gap
                then (\x -> (x, LeftArrow)) <$> M.unsafeRead mCost (i, j - 1)
                else let deleteCost = cost topElement gap
                         (alignElem, alignCost) = tcm topElement leftElement
                     in  do diagCost <- M.unsafeRead mCost (i - 1, j - 1)
                            topCost  <- M.unsafeRead mCost (i - 1, j    )
                            leftCost <- M.unsafeRead mCost (i    , j - 1)
                            pure $ getMinimalResult gap alignElem
                                     [ ( alignCost + diagCost, DiagArrow)
                                     , (deleteCost + leftCost, LeftArrow)
                                     , (insertCost +  topCost, UpArrow  )
                                     ]

      -- Define how to compute the first cell of the first "offest" rows.
      -- We need to ensure that there are only Up Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 1,
      -- since the diagonal and leftward values are "out of bounds."
      leftColumn _leftElement insertCost i j = {-# SCC leftColumn #-} do
            firstPrevCost <- M.unsafeRead mCost (i - 1, j)
            pure (insertCost + firstPrevCost, UpArrow)

      -- Define how to compute the first cell of the remaining rows.
      -- We need to ensure that there are no Left Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 2,
      -- since the leftward values are "out of bounds."
      leftBoundary leftElement insertCost i j =
            let topElement = symbolAlignmentMedian $ longerTop ! (j - 1)
                (alignElem, alignCost) = tcm topElement leftElement
            in  do diagCost <- M.unsafeRead mCost (i - 1, j - 1)
                   topCost  <- M.unsafeRead mCost (i - 1, j    )
                   pure $ getMinimalResult gap alignElem
                            [ ( alignCost + diagCost, DiagArrow)
                            , (insertCost +  topCost, UpArrow  )
                            ]

      -- Define how to compute the last cell of the first "rows - offest" rows.
      -- We need to ensure that there are only Left Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 1,
      -- since the diagonal and upward values are "out of bounds."
      rightBoundary leftElement _insertCost i j = {-# SCC rightBoundary #-}
            let topElement = symbolAlignmentMedian $ longerTop ! (j - 1)
                deleteCost = cost topElement gap
                (alignElem, alignCost) = tcm topElement leftElement
            in  do diagCost <- M.unsafeRead mCost (i - 1, j - 1)
                   leftCost <- M.unsafeRead mCost (i    , j - 1)
                   pure $ getMinimalResult gap alignElem
                            [ ( alignCost + diagCost, DiagArrow)
                            , (deleteCost + leftCost, LeftArrow)
                            ]

      -- Define how to compute the last cell of the last "offest" rows.
      -- We need to ensure that there are no Up Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 2,
      -- since the upward values are "out of bounds."
      rightColumn = {-# SCC rightColumn #-} internalCell

