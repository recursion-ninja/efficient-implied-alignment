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
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnboxedTuples       #-}

module Alignment.Pairwise.UnboxedUkkonen
  ( unboxedUkkonenDO
  ) where

import           Alignment.Pairwise.Internal (Direction(..), deleteGaps, insertGaps, measureCharacters, measureAndUngapCharacters)
import           Control.DeepSeq
import           Control.Monad.Loops         (iterateUntilM, whileM_)
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
import qualified Data.Matrix.Unboxed         as Z
import           Data.Matrix.Unboxed.Mutable (MMatrix)
import qualified Data.Matrix.Unboxed.Mutable as M
import           Data.STRef
import           Data.SymbolString
import           Data.TCM
import qualified Data.Vector.NonEmpty        as NEV
import qualified Data.Vector.Unboxed.Mutable as V
import           Data.Word                   (Word16)
import           Prelude                     hiding (lookup, zipWith)

import Debug.Trace
--trace = const id
tr' p s x = if p then trace (s <> ": " <> show x) x else x
tr s x = trace (s <> ": " <> show x) x


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
    (_, longer, lesser) = measureCharacters char1 char2

    gap = encodeAmbiguityGroup alphabet $ gapSymbol alphabet :| []

    cost x y = snd $ tcm x y
    
    buildFullMatrix = unboxedSwappingDO gap tcm char1 char2

    buildPartialMatrixMaybe = createUkkonenMethodMatrix gap coefficient gapsPresentInInputs cost

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
{-# SPECIALISE createUkkonenMethodMatrix :: SymbolAmbiguityGroup -> Word -> Word -> (SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> Word) -> SymbolString -> SymbolString -> (Word, Matrix Direction) #-}
createUkkonenMethodMatrix
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     )
  => SymbolAmbiguityGroup -- ^ Gap
  -> Word -- ^ Coefficient value, representing the /minimum/ transition cost from a state to gap
  -> Word -- ^ Gaps present in input
  -> (SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> Word)
  -> f SymbolContext -- ^ First  (shorter) dynamic character
  -> f SymbolContext -- ^ Second (longer) dynamic character
  -> (Word, Matrix Direction)
createUkkonenMethodMatrix gap minimumIndelCost gapsPresentInInputs cost lesserLeft longerTop = finalMatrix
  where
    -- General values that need to be in scope for the recursive computations.
    longerLen   = tr "longerLen" $ length longerTop
    lesserLen   = tr "lesserLen" $ length lesserLeft

    -- We start the offset at two rather than at one so that the first doubling
    -- isn't trivially small.
    startOffset = 2 + gapsPresentInInputs

    -- /O(1)/
    --
    -- Necessary to compute the width of a row in the barrier-constrained matrix.
    quasiDiagonalWidth = toEnum $ differenceInLength + 1
      where
        differenceInLength = tr "differenceInLength" $ longerLen - lesserLen

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
        (mCost, mDir) <- buildInitialBandedMatrix gap cost lesserLeft longerTop startOffset
        x <- unsafeFreeze mCost
        y <- unsafeFreeze mDir
        trace (renderCostMatrix gap longerTop lesserLeft x y) $ pure ()
        offsetRef <- newSTRef startOffset
        whileM_ (needToResizeBand mCost offsetRef) $ do
          previousOffset <- readSTRef offsetRef
          let currentOffset = previousOffset `shiftL` 1 -- Multiply by 2
          writeSTRef offsetRef currentOffset
          expandBandedMatrix gap cost lesserLeft longerTop mCost mDir previousOffset currentOffset
          x' <- unsafeFreeze mCost
          y' <- unsafeFreeze mDir
          trace (renderCostMatrix gap lesserLeft longerTop x' y') $ pure ()

        c <- M.unsafeRead mCost (lesserLen, longerLen)
        m <- unsafeFreeze mDir

        pure (c, m)

               
{-# SCC buildInitialBandedMatrix #-}
buildInitialBandedMatrix
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     )
  => SymbolAmbiguityGroup -- ^ Gap
  -> (SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> Word)
  -> f SymbolContext -- ^ First  (shorter) dynamic character
  -> f SymbolContext -- ^ Second (longer)  dynamic character
  -> Word
  -> ST s (MMatrix s Word, MMatrix s Direction)
buildInitialBandedMatrix gap cost lesserLeft longerTop o = fullMatrix
  where
    -- Note: "offset" cannot cause "width" to exceed "cols"
    offset    = let o' = fromEnum o in  min o' $ cols - quasiDiagonalWidth
    longerLen = length longerTop
    lesserLen = length lesserLeft
    rows      = length lesserLeft + 1
    cols      = length longerTop  + 1
    width     = quasiDiagonalWidth + (offset `shiftL` 1) -- Multiply by 2
    quasiDiagonalWidth = differenceInLength + 1
      where
        differenceInLength = longerLen - lesserLen

    fullMatrix = do
      
      ---------------------------------------
      -- Allocate required space           --
      ---------------------------------------

      mCost <- M.new (rows, cols)
      mDir  <- M.new (rows, cols)

      ---------------------------------------
      -- Define some generalized functions --
      ---------------------------------------

      -- Write to a single cell of the current vector and directional matrix simultaneously
      let write !p ~(!c, !d)
            | p == (17,15) && trace (fold ["Writing to ", show p, " ", show c]) False = undefined
            | otherwise = M.unsafeWrite mCost p c *> M.unsafeWrite mDir p d

      -- Write to an internal cell (not on a boundary) of the matrix.
      let internalCell leftElement insertCost i j
            -- Preserve the gap in the left (lesser) string
            | leftElement == gap = (\x -> (x, UpArrow)) <$> M.unsafeRead mCost (i - 1, j)
            | otherwise = {-# SCC internalCell_expanding #-}
              let topElement = symbolAlignmentMedian $ longerTop ! (j - 1)
                  p = (i,j)
                  t = (17,16)
                  -- Preserve the gap in the top (longer) string
              in  if topElement == gap
                  then (\x -> (x, LeftArrow)) <$> M.unsafeRead mCost (i, j - 1)
                  else let deleteCost = tr' (p == t) "deleteCost" $ cost topElement gap
                           alignCost  = tr' (p == t)  "alignCost" $ cost topElement leftElement
                       in  do diagCost <- tr' (p == t) "diagCost" <$> M.unsafeRead mCost (i - 1, j - 1)
                              topCost  <- tr' (p == t)  "topCost" <$> M.unsafeRead mCost (i - 1, j    )
                              leftCost <- tr' (p == t) "leftCost" <$> M.unsafeRead mCost (i    , j - 1)
                              pure . (\x -> if p == t then trace ("<:> " <> show x) x else x) $ minimum
                                  [ ( alignCost + diagCost, DiagArrow)
                                  , (deleteCost + leftCost, LeftArrow)
                                  , ((tr' (p == t) "insertCost" insertCost) +  topCost, UpArrow  )
                                  ]
      
      -- Define how to compute the first cell of the first "offest" rows.
      -- We need to ensure that there are only Up Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 1,
      -- since the diagonal and leftward values are "out of bounds."
      let leftColumn _leftElement insertCost i j = {-# SCC leftColumn #-} do
            firstPrevCost <- M.unsafeRead mCost (i - 1, j)
            pure (insertCost + firstPrevCost, UpArrow)

      -- Define how to compute the first cell of the remaining rows.
      -- We need to ensure that there are no Left Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 2,
      -- since the leftward values are "out of bounds."
      let leftBoundary leftElement insertCost i j
            -- Preserve the gap in the left (lesser) string
            | leftElement == gap = (\x -> (x, UpArrow)) <$> M.unsafeRead mCost (i - 1, j)
            | otherwise = {-# SCC leftBoundary #-}
              let topElement = symbolAlignmentMedian $ longerTop ! (j - 1)
                  alignCost  = cost topElement leftElement
              in  do diagCost <- M.unsafeRead mCost (i - 1, j - 1)
                     topCost  <- M.unsafeRead mCost (i - 1, j    )
                     pure $ minimum
                         [ ( alignCost + diagCost, DiagArrow)
                         , (insertCost +  topCost, UpArrow  )
                         ]

      -- Define how to compute the last cell of the first "rows - offest" rows.
      -- We need to ensure that there are only Left Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 1,
      -- since the diagonal and upward values are "out of bounds."
      let rightBoundary leftElement _insertCost i j = {-# SCC rightBoundary #-}
            let topElement = symbolAlignmentMedian $ longerTop ! (j - 1)
            -- Preserve the gap in the top (longer) string
            in  if topElement == gap
                then (\x -> (x, LeftArrow)) <$> M.unsafeRead mCost (i, j - 1)
                else let deleteCost = cost topElement    gap
                         alignCost  = cost topElement leftElement
                     in  do diagCost <- M.unsafeRead mCost (i - 1, j - 1)
                            leftCost <- M.unsafeRead mCost (i    , j - 1)
                            pure $ minimum
                                [ ( alignCost + diagCost, DiagArrow)
                                , (deleteCost + leftCost, LeftArrow)
                                ]

      -- Define how to compute the last cell of the last "offest" rows.
      -- We need to ensure that there are no Up Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 2,
      -- since the upward values are "out of bounds."
      let rightColumn = {-# SCC rightColumn #-} internalCell

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
        in  if topElement == gap
            then ((\x -> (x, LeftArrow)) <$> M.unsafeRead mCost (0, j - 1)) >>= write (0,j)
            else let firstCellCost = cost gap topElement
                 in  do firstPrevCost <- M.unsafeRead mCost (0, j - 1)
                        write (0,j) (firstCellCost + firstPrevCost, LeftArrow)

      -- Loop through the remaining rows.
      for_ [1 .. rows - 1] writeRow

      -- Return the matricies for possible expansion
      pure (mCost, mDir)


{-# SCC expandBandedMatrix #-}
expandBandedMatrix
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     )
  => SymbolAmbiguityGroup -- ^ Gap
  -> (SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> Word)
  -> f SymbolContext -- ^ First  (shorter) dynamic character
  -> f SymbolContext -- ^ Second (longer)  dynamic character
  -> MMatrix s Word
  -> MMatrix s Direction
  -> Word
  -> Word
  -> ST s ()
expandBandedMatrix gap cost lesserLeft longerTop mCost mDir po co = updateBand
  where
    
    -- Note: "offset" cannot cause "width + quasiDiagonalWidth" to exceed "2 * cols"
    offset      = let o' = fromEnum co in  min o' $ cols - quasiDiagonalWidth
    prevOffset  = fromEnum po
    longerLen   = length longerTop
    lesserLen   = length lesserLeft
    rows        = length lesserLeft + 1
    cols        = length longerTop  + 1
    width       = quasiDiagonalWidth + (offset `shiftL` 1) -- Multiply by 2
    quasiDiagonalWidth = differenceInLength + 1
      where
        differenceInLength = longerLen - lesserLen

    updateBand = do

      ---------------------------------------
      -- Allocate mutable state variables  --
      ---------------------------------------

      headStop  <- newSTRef cols
      tailStart <- newSTRef cols

      ---------------------------------------
      -- Define some generalized functions --
      ---------------------------------------

      -- Write to a single cell of the current vector and directional matrix simultaneously
      let write !p ~(!c, !d) = M.unsafeWrite mCost p c *> M.unsafeWrite mDir p d

      -- Write to an internal cell (not on a boundary) of the matrix.
      let internalCell leftElement insertCost i j
            -- Preserve the gap in the left (lesser) string
            | leftElement == gap = (\x -> (x, UpArrow)) <$> M.unsafeRead mCost (i - 1, j)
            | otherwise = {-# SCC internalCell_expanding #-}
              let topElement = symbolAlignmentMedian $ longerTop ! (j - 1)
                  -- Preserve the gap in the top (longer) string
              in  if topElement == gap
                  then (\x -> (x, LeftArrow)) <$> M.unsafeRead mCost (i, j - 1)
                  -- Normal Needleman-Wunsch Logic
                  else let  deleteCost = cost topElement    gap
                            alignCost   = cost topElement leftElement
                       in  do diagCost <- M.unsafeRead mCost (i - 1, j - 1)
                              topCost  <- M.unsafeRead mCost (i - 1, j    )
                              leftCost <- M.unsafeRead mCost (i    , j - 1)
                              pure $ minimum
                                  [ ( alignCost + diagCost, DiagArrow)
                                  , (deleteCost + leftCost, LeftArrow)
                                  , (insertCost +  topCost, UpArrow  )
                                  ]
      
      -- Define how to compute the first cell of the first "offest" rows.
      -- We need to ensure that there are only Up Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 1,
      -- since the diagonal and leftward values are "out of bounds."
      let leftColumn _leftElement insertCost i j = {-# SCC leftColumn #-} do
            firstPrevCost <- M.unsafeRead mCost (i - 1, j)
            pure (insertCost + firstPrevCost, UpArrow)

      -- Define how to compute the first cell of the remaining rows.
      -- We need to ensure that there are no Left Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 2,
      -- since the leftward values are "out of bounds."
      -- Define how to compute the first cell of the remaining rows.
      -- We need to ensure that there are no Left Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 2,
      -- since the leftward values are "out of bounds."
      let leftBoundary leftElement insertCost i j
            -- Preserve the gap in the left (lesser) string
            | leftElement == gap = (\x -> (x, UpArrow)) <$> M.unsafeRead mCost (i - 1, j)
            | otherwise = {-# SCC leftBoundary #-}
              let topElement = symbolAlignmentMedian $ longerTop ! (j - 1)
                  alignCost  = cost topElement leftElement
              in  do diagCost <- M.unsafeRead mCost (i - 1, j - 1)
                     topCost  <- M.unsafeRead mCost (i - 1, j    )
                     pure $ minimum
                         [ ( alignCost + diagCost, DiagArrow)
                         , (insertCost +  topCost, UpArrow  )
                         ]

      -- Define how to compute the last cell of the first "rows - offest" rows.
      -- We need to ensure that there are only Left Arrow values in the directional matrix.
      -- We can also reduce the number of comparisons the first row makes from 3 to 1,
      -- since the diagonal and upward values are "out of bounds."
      let rightBoundary leftElement _insertCost i j = {-# SCC rightBoundary #-}
            let topElement = symbolAlignmentMedian $ longerTop ! (j - 1)
            -- Preserve the gap in the top (longer) string
            in  if topElement == gap
                then (\x -> (x, LeftArrow)) <$> M.unsafeRead mCost (i, j - 1)
                else let deleteCost = cost topElement    gap
                         alignCost  = cost topElement leftElement
                     in  do diagCost <- M.unsafeRead mCost (i - 1, j - 1)
                            leftCost <- M.unsafeRead mCost (i    , j - 1)
                            pure $ minimum
                                [ ( alignCost + diagCost, DiagArrow)
                                , (deleteCost + leftCost, LeftArrow)
                                ]

      let rightColumn = {-# SCC rightColumn #-} internalCell
      
      let computeCell leftElement insertCost i j = {-# SCC recomputeCell #-}
            let topElement = symbolAlignmentMedian $ longerTop ! (j - 1) 
                deleteCost = cost topElement    gap
                alignCost  = cost topElement leftElement
            in do
                  diagCost <- M.unsafeRead mCost (i - 1, j - 1)
                  topCost  <- M.unsafeRead mCost (i - 1, j    )
                  leftCost <- M.unsafeRead mCost (i    , j - 1)
                  oldCost  <- M.unsafeRead mCost (i    , j    )
                  let e@(c,_) = minimum $ traceShowId
                                  [ ( alignCost + diagCost, DiagArrow)
                                  , (deleteCost + leftCost, LeftArrow)
                                  , (insertCost +  topCost, UpArrow  )
                                  ]
                  write (i,j) e
                  let res = (c == oldCost, j+1)
                  trace (unlines [ "recomputeCell" <> show (i,j)
                                 , "  diagCost: " <> show diagCost
                                 , "   topCost: " <> show  topCost
                                 , "  leftCost: " <> show leftCost
                                 , ""
                                 , "  oldCost: " <> show oldCost
                                 , "  newCost: " <> show c
                                 , "  result:  " <> show res
                                 ]
                        ) $ pure ()
                  pure res
--                  pure (c /= oldCost, j+1)

      -- Define how to compute values to an entire row of the Ukkonen matrix.
      let extendRow i =
            -- Precomute some values that will be used for the whole row
            let start0 =  max 0          $ i - offset
                start3 =  min (cols - 1) $ i + width - offset - prevOffset
                goUpTo =  max 0          ( i - prevOffset) - 1
                stop   =  min (cols - 1) $ i + width - offset - 1
                leftElement = symbolAlignmentMedian $ lesserLeft ! (i - 1)
                insertCost  = cost gap leftElement
                firstCell
                  | i <= offset = leftColumn
                  | otherwise   = leftBoundary

                lastCell
                  | i <= cols - quasiDiagonalWidth - offset = rightBoundary
                  | otherwise = rightColumn

                continueRecomputing (same, j) = same || j >= stop - 1
                computeCell' ~(_,j) = computeCell leftElement insertCost i j
                internalCell' j = internalCell leftElement insertCost i j >>= write (i,j)
                recomputeUntilSame j = snd <$> iterateUntilM continueRecomputing computeCell' (False, j)
            in  do -- Get the starts from the previous iteration
                   start1 <- readSTRef headStop
                   start2 <- readSTRef tailStart

                   let showBounds = trace (unlines
                           [ "quasiDiagonalWidth: " <> show quasiDiagonalWidth
                           , "prevOffset: " <> show prevOffset
                           , "offset: " <> show offset
                           , "width:  " <> show width
                           , "start0: " <> show start0
                           , "start1: " <> show start1
                           , "start2: " <> show start2
                           , "start3: " <> show start3
                           , "goUpTo: " <> show goUpTo
                           , "stop:   " <> show stop
                           ])
                           $ pure ()

                   if i == 22
                   then showBounds
                   else pure ()

                   if   start2 > start3
                   then trace "!!! start2 > start 3 !!!" (pure ()) *> showBounds
                   else pure ()

                   -- Conditionally write to the first cell of the Ukkonen band
                   if   i > prevOffset
                   then firstCell leftElement insertCost i start0 >>= write (i, start0)
                   else pure ()

                   for_ [start0+1 .. goUpTo] internalCell'
                   leadStop  <- if (\x -> trace ("goUpTo < start0 = " <> show x) x) $ goUpTo < start0
                                then pure start1
                                else recomputeUntilSame $ goUpTo+1
                   headStop' <- if   leadStop >= start1
                                then pure leadStop
                                else recomputeUntilSame start1
                   tailStop' <- if start2 >= start3
                                then pure start3
                                else recomputeUntilSame start2
                   trace (unlines [ "headStop': " <> show headStop'
                                  , "tailStop': " <> show tailStop'
                                  ]
                         ) $ pure ()
                   for_ [max (tailStop') start3 .. stop-1] internalCell'

                   -- Conditionally write to the last cell of the Ukkonen band
                   if   tailStop' < stop
                   then lastCell leftElement insertCost i stop >>= write (i, stop)
                   else trace ("Skipping last cell for row " <> show i) $ pure ()

                   -- Update references for the next row
                   writeSTRef headStop headStop'
                   writeSTRef tailStart $ if tailStop' /= start2 then tailStop' else start3

      ---------------------------------------
      -- Compute all values of the matrix  --
      ---------------------------------------

      let start = quasiDiagonalWidth + prevOffset

      -- Extend the first row to seed subsequent rows.
      for_ [start .. min (cols - 1) (width - offset - 1)] $ \j ->
        let topElement    = symbolAlignmentMedian $ longerTop ! (j - 1)
        in  if topElement == gap
            then ((\x -> (x, LeftArrow)) <$> M.unsafeRead mCost (0, j - 1)) >>= write (0,j)
            else let firstCellCost = cost gap topElement
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
buildFullDirectionMatrix gap overlapFunction leftChar topChar = fullMatrix
  where
    med  x y   = fst $ overlapFunction x y
    cost x y   = snd $ overlapFunction x y
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
        in  if topElement == gap
            then ((\x -> (x, LeftArrow)) <$> V.unsafeRead vOne (j - 1)) >>= uncurry (write vOne (0,j))
            else let firstCellCost = cost gap topElement
                 in  do firstPrevCost <- V.unsafeRead vOne (j - 1)
                        write vOne (0,j) (firstCellCost + firstPrevCost) LeftArrow
{-
        let topElement    = symbolAlignmentMedian $ topChar ! (j - 1)
            firstCellCost = cost gap topElement
        in  do firstPrevCost <- V.unsafeRead vOne (j - 1)
               write vOne (0,j) (firstCellCost + firstPrevCost) LeftArrow
-}
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
                     alignCost   = cost topElement leftElement
                     insertCost  = cost gap        leftElement
                 in  do diagCost <- V.unsafeRead prev $ j - 1
                        topCost  <- V.unsafeRead prev   j
                        leftCost <- V.unsafeRead curr $ j - 1
                        let xs = [ ( alignCost + diagCost, DiagArrow)
                                 , (deleteCost + leftCost, LeftArrow)
                                 , (insertCost +  topCost, UpArrow  )
                                 ]
                        let (c,d) = minimum xs
--                        write curr (i,j) c d
                        if d == DiagArrow && gap == med topElement leftElement
                        then write curr (i,j) c LeftArrow
                        else write curr (i,j) c d

      let v | odd  rows = vOne
            | otherwise = vTwo
      c <- V.unsafeRead v (cols - 1)
      m <- unsafeFreeze mDir

      x <- unsafeFreeze =<< M.replicate (rows, cols) 0
      trace (renderCostMatrix gap leftChar topChar x m) $ pure ()

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

{-
  
  , deleteGaps
  , insertGaps

    let  (swapped, longerChar, shorterChar) = measureCharacters char1 char2
         (alignmentCost, traversalMatrix)   = matrixFunction longerChar shorterChar
         uncommutedContext = traceback gap overlapλ traversalMatrix longerChar shorterChar
         alignmentContext
           | swapped   = reverseContext <$> uncommutedContext
           | otherwise = uncommutedContext
    in   (alignmentCost, alignmentContext)

directOptimization overlapλ char1 char2 matrixFunction =
    let (swapped, gapsLesser, gapsLonger, shorterChar, longerChar) = measureAndUngapCharacters char1 char2
        (alignmentCost, ungappedAlignment) =
          if      olength shorterChar == 0
          then if olength  longerChar == 0
               -- Neither character was Missing, but both are empty when gaps are removed                                                                                                                          
               then (0, toMissing char1)
               -- Neither character was Missing, but one of them is empty when gaps are removed                                                                                                                    
               else let gap = getMedian $ gapOfStream char1
                        f x = let m = getMedian x in deleteElement (fst $ overlapλ m gap) m
                    in  (0, (\x -> trace ("One char all gaps, non-gapped char: " <> show x) x) $ omap f longerChar)
               -- Both have some non-gap elements, perform string alignment                                                                                                                                        
          else let traversalMatrix = matrixFunction overlapλ longerChar $ trace "Neither Empty" shorterChar
               in  traceback overlapλ traversalMatrix longerChar shorterChar
        transformation    = if swapped then omap swapContext else id
        regappedAlignment = insertGaps gapsLesser gapsLonger shorterChar longerChar ungappedAlignment
        alignmentContext  = transformation regappedAlignment
    in  handleMissingCharacter char1 char2 (alignmentCost, alignmentContext)
-}


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

    longerLen = tr "traceback:longerLen" $ length longerChar
    lesserLen = tr "traceback:lesserLen" $ length lesserChar
    rows      = lesserLen + 1
    cols      = longerLen + 1

    startPoint = trace "Traceback UNBOXED" (rows - 1, cols - 1)

    go !p@(~(!i, !j))
      | trace (show p) False = mempty
      | p == (0,0) = mempty
      | otherwise  =
        let previousSequence = go (row', col')

            directionArrow = unsafeIndex alignMatrix p

            (# !row', !col', !localContext #) =
                case directionArrow of
                  LeftArrow -> let j' = j - 1
                                   te = symbolAlignmentMedian $ longerChar ! j'
                                   e  = insertElement (f te gap) te
--                                   e  = deleteElement (f te gap) te
                               in (# i , j', e #)
                  UpArrow   -> let i' = i - 1
                                   le = symbolAlignmentMedian $ lesserChar ! i'
                                   e  = deleteElement (f gap le) le
--                                   e  = insertElement (f gap le) le
                               in (# i', j, e #)
                  DiagArrow -> let i' = i - 1
                                   j' = j - 1
                                   te = symbolAlignmentMedian $ longerChar ! j'
                                   le = symbolAlignmentMedian $ lesserChar ! i'
                                   e  = alignElement (f te le) te le
                               in (# i', j', e #)

        in  previousSequence `snoc` localContext


{--}
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
--    showCell (0,DiagArrow) = ""
    showCell (c,d)    = show c <> show d
    maxPrefixWidth    = maxLengthOf lesserTokens
    maxColumnWidth    = max (maxLengthOf longerTokens) . maxLengthOf $ fold matrixTokens
    maxLengthOf       = maximum . fmap length

    colCount = tr "renderCostMatrix:colCount" $ length longer + 1
    rowCount = tr "renderCostMatrix:rowCount" $ length lesser + 1

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
        renderRow k e vs = " " <> pad maxPrefixWidth e <> "┃ " <> concatMap (pad maxColumnWidth) vs

    renderContext (Align  x _ _) = if x == gapGroup then "—" else "α"
    renderContext (Delete x _  ) = if x == gapGroup then "—" else "δ"
    renderContext (Insert x   _) = if x == gapGroup then "—" else "ι"
    renderContext Gapping{}      = "—"

    pad :: Int -> String -> String
    pad n e = replicate (n - len) ' ' <> e <> " "
      where
        len = length e
{--}
