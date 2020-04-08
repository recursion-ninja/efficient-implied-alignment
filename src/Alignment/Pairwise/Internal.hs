-----------------------------------------------------------------------------
-- |
-- Module      :  Alignment.Pairwise.Internal
-- Copyright   :  (c) 2018 Alex Washburn
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-- Defines the primitive operations for standard Needleman-Wunsch and Ukkonen
-- algorithms for performing a direct optimization heuristic alignmnet between
-- two alignment context strings.
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE TypeFamilies          #-}

-- To add Indexable/Lookup instances for Matrix
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Alignment.Pairwise.Internal
  ( Cost
  , Direction(..)
  , MatrixConstraint
  , MatrixFunction
  , NeedlemanWunchMatrix
  -- * Direct Optimization primitive construction functions
  , directOptimization
  , measureCharacters
  , needlemanWunschDefinition
  , renderCostMatrix
  ) where


import           Alignment.Pairwise.Ukkonen.Matrix (UkkonenMethodMatrix)
import           Control.Monad.State.Strict
import           Data.Foldable
import           Data.Key
import           Data.Matrix                       (Matrix, dim, unsafeIndex)
import           Data.Maybe                        (fromMaybe)
import           Data.Ord
import           Data.Semigroup
import           Data.SymbolString
import           Data.TCM
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import           Data.Vector.NonEmpty
import qualified Data.Vector.Primitive       as P
import qualified Data.Vector.Unboxed         as U
import           Data.Word                   (Word8)
import           Numeric.Extended.Natural
import           Prelude                           hiding (lookup, reverse, zipWith)

--import Debug.Trace
trace = const id


-- |
-- Which direction to align the character at a given matrix point.
--
-- It should be noted that the ordering of the three arrow types are important,
-- as it guarantees that the derived 'Ord' instance will have the following
-- property:
--
-- DiagArrow < LeftArrow < UpArrow
--
-- This means:
--
--   - DiagArrow has highest precedence when one or more costs are equal
--
--   - LeftArrow has second highest precedence when one or more costs are equal
--
--   -   UpArrow has lowest precedence when one or more costs are equal
--
-- Using this 'Ord' instance, we can resolve ambiguous transformations in a
-- deterministic way. Without loss of generality in determining the ordering,
-- we choose the same biasing as the C code called from the FFI for consistency.
data Direction = DiagArrow | LeftArrow | UpArrow
  deriving (Eq, Ord)


data instance U.MVector s Direction = MV_Direction (P.MVector s Word8)


data instance U.Vector   Direction  = V_Direction  (P.Vector    Word8)


instance U.Unbox Direction


instance M.MVector U.MVector Direction where

    {-# INLINE basicLength #-}
    basicLength (MV_Direction v) = M.basicLength v

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n (MV_Direction v) = MV_Direction $ M.basicUnsafeSlice i n v

    {-# INLINE basicOverlaps #-}
    basicOverlaps (MV_Direction v1) (MV_Direction v2) = M.basicOverlaps v1 v2

    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n = MV_Direction `liftM` M.basicUnsafeNew n

    {-# INLINE basicInitialize #-}
    basicInitialize (MV_Direction v) = M.basicInitialize v

    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeReplicate n x = MV_Direction `liftM` M.basicUnsafeReplicate n (fromDirection x)

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MV_Direction v) i = toDirection `liftM` M.basicUnsafeRead v i

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MV_Direction v) i x = M.basicUnsafeWrite v i (fromDirection x)

    {-# INLINE basicClear #-}
    basicClear (MV_Direction v) = M.basicClear v

    {-# INLINE basicSet #-}
    basicSet (MV_Direction v) x = M.basicSet v (fromDirection x)

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MV_Direction v1) (MV_Direction v2) = M.basicUnsafeCopy v1 v2

    basicUnsafeMove (MV_Direction v1) (MV_Direction v2) = M.basicUnsafeMove v1 v2

    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeGrow (MV_Direction v) n = MV_Direction `liftM` M.basicUnsafeGrow v n


instance G.Vector U.Vector Direction where

    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MV_Direction v) = V_Direction `liftM` G.basicUnsafeFreeze v

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (V_Direction v) = MV_Direction `liftM` G.basicUnsafeThaw v

    {-# INLINE basicLength #-}
    basicLength (V_Direction v) = G.basicLength v

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n (V_Direction v) = V_Direction $ G.basicUnsafeSlice i n v

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (V_Direction v) i = toDirection `liftM` G.basicUnsafeIndexM v i

    basicUnsafeCopy (MV_Direction mv) (V_Direction v) = G.basicUnsafeCopy mv v

    {-# INLINE elemseq #-}
    elemseq _ = seq


instance Show Direction where

    show DiagArrow = "↖"
    show LeftArrow = "←"
    show UpArrow   = "↑"


-- |
-- This internal type used for computing the alignment cost. This type has an
-- "infinity" value that is conveniently used for the barrier costs. The cost is
-- strictly non-negative, and possibly infinite.
type Cost = ExtendedNatural


-- |
-- A representation of an alignment matrix for DO.
-- The matrix itself stores tuples of the cost and direction at that position.
-- We also store a vector of characters that are generated.
type NeedlemanWunchMatrix e = Matrix (Cost, Direction, e)


-- |
-- Constraints on the type of structure a "matrix" exposes to be used in rendering
-- and traceback functions.
type MatrixConstraint m = (Indexable m, Key m ~ (Int, Int))


-- |
-- A parameterized function to generate an alignment matrix.
type MatrixFunction m f
    =  TransitionCostMatrix
    -> f SymbolContext
    -> f SymbolContext
    -> m (Cost, Direction, SymbolAmbiguityGroup)


type instance Key Matrix = (Int, Int)


instance Indexable Matrix where

    index = unsafeIndex


instance Lookup Matrix where

    lookup p@(i,j) m
      | i < 0 || r <= i = Nothing
      | j < 0 || c <= j = Nothing
      | otherwise       = Just $ unsafeIndex m p
      where
        (r,c) = dim m

  
-- |
-- Wraps the primitive operations in this module to a cohesive operation that is
-- parameterized by an 'TransitionCostMatrix'.
--
-- Reused internally by different implementations.
{-# INLINEABLE directOptimization #-}
{-# SPECIALIZE directOptimization :: TransitionCostMatrix -> (Vector SymbolContext -> Vector SymbolContext -> Matrix              (Cost, Direction, SymbolAmbiguityGroup) -> String) -> MatrixFunction Matrix              Vector -> Vector  SymbolContext -> Vector SymbolContext -> (Word, Vector SymbolContext) #-}
{-# SPECIALIZE directOptimization :: TransitionCostMatrix -> (Vector SymbolContext -> Vector SymbolContext -> UkkonenMethodMatrix (Cost, Direction, SymbolAmbiguityGroup) -> String) -> MatrixFunction UkkonenMethodMatrix Vector -> Vector  SymbolContext -> Vector SymbolContext -> (Word, Vector SymbolContext) #-}
directOptimization
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     , Key m ~ (Int, Int)
     , MatrixConstraint m
     )
  => TransitionCostMatrix
  -> (f SymbolContext -> f SymbolContext -> m (Cost, Direction, SymbolAmbiguityGroup) -> String)
  -> MatrixFunction m f
  -> f SymbolContext
  -> f SymbolContext
  -> (Word, Vector SymbolContext)
directOptimization overlapFunction _renderingFunction matrixFunction lhs rhs = trace (_renderingFunction lhs rhs traversalMatrix) (alignmentCost, alignmentContext)
  where
    (swapped, longerInput, shorterInput) = measureCharacters lhs rhs
    traversalMatrix                      = matrixFunction overlapFunction longerInput shorterInput
    (alignmentCost, uncommutedContext)   = traceback      traversalMatrix longerInput shorterInput
    alignmentContext
      | swapped   = reverseContext <$> uncommutedContext
      | otherwise = uncommutedContext


-- |
-- Strips the gap symbols from the supplied string.
--filterGaps :: NonEmpty (SymbolContext s) -> NonEmpty (SymbolContext s)
--filterGaps = NE.fromList . NE.filter (/= gap)


-- |
-- /O(1)/ for input strings of differing lengths
--
-- /O(k)/ for input strings of equal length, where /k/ is the shared prefix of
-- both characters.
--
-- Returns the string that is longer first, shorter second, and notes whether or
-- not the inputs were swapped to place the strings in this ordering.
--
-- Handles equal length characters by considering the lexicographically larger
-- string as longer.
--
-- Handles equality of inputs by /not/ swapping.
{-# INLINEABLE measureCharacters #-}
{-# SPECIALISE measureCharacters :: Vector SymbolContext -> Vector SymbolContext -> (Bool, Vector SymbolContext, Vector SymbolContext) #-}
measureCharacters
  :: Foldable f
  => f SymbolContext
  -> f SymbolContext
  -> (Bool, f SymbolContext, f SymbolContext)
measureCharacters lhs rhs
  | lhsOrdering == LT = ( True, rhs, lhs)
  | otherwise         = (False, lhs, rhs)
  where
    lhsOrdering =
        case comparing length lhs rhs of
          EQ -> let f = fmap symbolAlignmentMedian . toList
                in  f lhs `compare` f rhs
          x  -> x


-- |
-- Internal generator function for the matrices based on the Needleman-Wunsch
-- definition described in their paper.
{-# INLINEABLE needlemanWunschDefinition #-}
{-# SPECIALIZE needlemanWunschDefinition :: SymbolAmbiguityGroup -> TransitionCostMatrix -> Vector SymbolContext -> Vector SymbolContext -> Matrix              (Cost, Direction, SymbolAmbiguityGroup) -> (Int, Int) -> (Cost, Direction, SymbolAmbiguityGroup) #-}
{-# SPECIALIZE needlemanWunschDefinition :: SymbolAmbiguityGroup -> TransitionCostMatrix -> Vector SymbolContext -> Vector SymbolContext -> UkkonenMethodMatrix (Cost, Direction, SymbolAmbiguityGroup) -> (Int, Int) -> (Cost, Direction, SymbolAmbiguityGroup) #-}
needlemanWunschDefinition
  :: ( Lookup f
     , Indexable m
     , Key f ~ Int
     , Key m ~ (Int, Int)
     )
  => SymbolAmbiguityGroup
  -> TransitionCostMatrix
  -> f SymbolContext
  -> f SymbolContext
  -> m (Cost, Direction, SymbolAmbiguityGroup)
  -> (Int, Int)
  -> (Cost, Direction, SymbolAmbiguityGroup)
needlemanWunschDefinition gapGroup overlapFunction topChar leftChar memo p@(row, col)
  |  p == (0,0)                          = (            0, DiagArrow, gapGroup)
  |  col /= 0 &&  topElement == gapGroup = (leftwardValue, LeftArrow, gapGroup)
  |  row /= 0 && leftElement == gapGroup = (  upwardValue,   UpArrow, gapGroup)
  |  row /= 0 && col /= 0 && isDiagGap   = (      minCost, LeftArrow, gapGroup)
  |  otherwise                           = (      minCost,    minDir, minState)
  where
    -- | Lookup with a default value of infinite cost.
    {-# INLINE (!?) #-}
    (!?) m k = fromMaybe (infinity, DiagArrow, gapGroup) $ k `lookup` m

    isDiagGap = (minDir, minState) == (DiagArrow, gapGroup)

    topContext                    = (col - 1) `lookup`  topChar
    leftContext                   = (row - 1) `lookup` leftChar
    topElement                    = maybe gapGroup symbolAlignmentMedian  topContext
    leftElement                   = maybe gapGroup symbolAlignmentMedian leftContext
    (leftwardValue, _leftwardArrow, _leftWardState) = memo !? (row    , col - 1)
    (  upwardValue,   _upwardArrow,   _upwardState) = memo !? (row - 1, col    )
    (diagonalValue, _, _)         = memo !? (row - 1, col - 1)
    (rightChar, rightOverlapCost) = fromFinite <$> overlapFunction topElement gapGroup
    ( diagChar,  diagOverlapCost) = fromFinite <$> overlapFunction topElement leftElement
    ( downChar,  downOverlapCost) = fromFinite <$> overlapFunction gapGroup   leftElement
    rightCost                     = rightOverlapCost + leftwardValue
    diagCost                      =  diagOverlapCost + diagonalValue
    downCost                      =  downOverlapCost +   upwardValue
    (minCost, minState, minDir)   = getMinimalCostDirection gapGroup
                                      ( diagCost,  diagChar)
                                      (rightCost, rightChar)
                                      ( downCost,  downChar)


-- |
-- Serializes an alignment matrix to a 'String'. Uses input characters for row
-- and column labelings.
--
-- Useful for debugging purposes.
{-# INLINEABLE renderCostMatrix #-}
--{-# SPECIALIZE renderCostMatrix :: SymbolAmbiguityGroup -> Vector SymbolContext -> Vector SymbolContext -> Matrix              (Cost, Direction, SymbolAmbiguityGroup) -> String #-}
{-# SPECIALIZE renderCostMatrix :: SymbolAmbiguityGroup -> Vector SymbolContext -> Vector SymbolContext -> UkkonenMethodMatrix (Cost, Direction, SymbolAmbiguityGroup) -> String #-}
renderCostMatrix
  :: ( Foldable f
     , Foldable m
     , Functor m
     , Indexable m
     , Key m ~ (Int, Int)
     )
  => SymbolAmbiguityGroup
  -> f SymbolContext
  -> f SymbolContext
  -> m (Cost, Direction, SymbolAmbiguityGroup)
  -> String
renderCostMatrix gapGroup lhs rhs mtx = unlines
    [ dimensionPrefix
    , headerRow
    , barRow
    , renderedRows
    ]
  where
    (_,longer,lesser) = measureCharacters lhs rhs
    longerTokens      = toShownIntegers longer
    lesserTokens      = toShownIntegers lesser
    toShownIntegers   = fmap renderContext . toList
    matrixTokens      = showCell <$> mtx
    showCell (c,d,_)  = show c <> show d
    maxPrefixWidth    = maxLengthOf lesserTokens
    maxColumnWidth    = max (maxLengthOf longerTokens) . maxLengthOf $ toList matrixTokens
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

    renderedRows = unlines . zipWith renderRow ("⁎":lesserTokens) $ getRows matrixTokens
      where
        renderRow e vs = " " <> pad maxPrefixWidth e <> "┃ " <> concatMap (pad maxColumnWidth) vs

        getRows m = (`getRow'` m) <$> [0 .. rowCount - 1]
        getRow' i m = g <$> [0 .. colCount - 1]
          where
            g j = fromMaybe "" $ (i,j) `lookup` m

    renderContext (Align  x _ _) = if x == gapGroup then "—" else "α"
    renderContext (Delete x _  ) = if x == gapGroup then "—" else "δ"
    renderContext (Insert x   _) = if x == gapGroup then "—" else "ι"
    renderContext Gapping{}      = "—"

    pad :: Int -> String -> String
    pad n e = replicate (n - len) ' ' <> e <> " "
      where
        len = length e


-- |
-- Performs the traceback of an 'NeedlemanWunchMatrix'.
--
-- Takes in an 'NeedlemanWunchMatrix', two strings and returns an aligned string,
-- Essentially does the second step of Needleman-Wunsch, following the arrows
-- from the bottom right corner, accumulating the alignment context string as it
-- goes. The alignment *should* be biased toward insertions into the shorter of
-- the two strings.
{-# INLINEABLE traceback #-}
{-# SPECIALISE traceback :: Matrix              (Cost, Direction, SymbolAmbiguityGroup) -> Vector SymbolContext -> Vector SymbolContext -> (Word, Vector SymbolContext) #-}
{-# SPECIALISE traceback :: UkkonenMethodMatrix (Cost, Direction, SymbolAmbiguityGroup) -> Vector SymbolContext -> Vector SymbolContext -> (Word, Vector SymbolContext) #-}
traceback
  :: ( Foldable f
     , Indexable f
     , Indexable m
     , Key f ~ Int
     , Key m ~ (Int, Int)
     )
  => m (Cost, Direction, SymbolAmbiguityGroup)
  -> f SymbolContext
  -> f SymbolContext
  -> (Word, Vector SymbolContext)
traceback alignMatrix longerChar lesserChar = (unsafeToFinite cost, reverse $ unfoldr go lastCell)
  where
      lastCell     = (row, col)
      (cost, _, _) = alignMatrix ! lastCell

      col = length longerChar
      row = length lesserChar

      go currentCell@(!i, !j)
        | nextCell < (0,0) = (contextElement, Nothing)
        | otherwise        = (contextElement, Just nextCell)
        where
          (_, directionArrow, medianElement) = alignMatrix ! currentCell

          (nextCell, contextElement) =
              case directionArrow of
                LeftArrow -> ((i    , j - 1), Delete medianElement (symbolAlignmentMedian $ longerChar ! (j - 1)))
                UpArrow   -> ((i - 1, j    ), Insert medianElement (symbolAlignmentMedian $ lesserChar ! (i - 1)))
                DiagArrow -> ((i - 1, j - 1), Align  medianElement (symbolAlignmentMedian $ longerChar ! (j - 1)) (symbolAlignmentMedian $ lesserChar ! (i - 1)))


getMinimalCostDirection
  :: Ord c
  => SymbolAmbiguityGroup
  -> (c, SymbolAmbiguityGroup)
  -> (c, SymbolAmbiguityGroup)
  -> (c, SymbolAmbiguityGroup)
  -> (c, SymbolAmbiguityGroup, Direction)
getMinimalCostDirection gap (diagCost, diagChar) (rightCost, rightChar) (downCost, downChar) =
    minimumBy (comparing (\(c,_,d) -> (c,d))) xs
  where
    xs =
      [ (diagCost ,  diagChar       , DiagArrow)
      , (rightCost, rightChar <> gap, LeftArrow)
      , (downCost ,  downChar <> gap, UpArrow  )
      ]
      
{-# INLINE fromDirection #-}
fromDirection :: Direction -> Word8
fromDirection DiagArrow = 0
fromDirection LeftArrow = 1
fromDirection UpArrow   = 2


{-# INLINE toDirection #-}
toDirection :: Word8 -> Direction
toDirection 0 = DiagArrow
toDirection 1 = LeftArrow
toDirection _ = UpArrow
