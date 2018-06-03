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
-- Defines the primative operations for standard Needleman-Wunsch and Ukkonen
-- algorithms for performing a direct optimization heuristic alignmnet between
-- two alignment context strings.
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns, ConstraintKinds, FlexibleContexts, TypeFamilies #-}

module Alignment.Pairwise.Internal
  ( Cost
  , Direction()
  , MatrixConstraint
  , MatrixFunction
  , NeedlemanWunchMatrix
  -- * Direct Optimization primative construction functions
  , directOptimization
  , measureCharacters
  , needlemanWunschDefinition
  , renderCostMatrix
  ) where


import           Data.Foldable
import           Data.Key
import           Data.Matrix.ZeroIndexed  (Matrix)
import           Data.Maybe               (fromMaybe)
import           Data.Ord
import           Data.Semigroup
import           Data.SymbolString
import           Data.TCM
import           Data.Vector.NonEmpty
import           Numeric.Extended.Natural
import           Prelude            hiding (lookup, reverse, zipWith)

--import Debug.Trace


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


-- | (✔)
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

-- |
-- Wraps the primative operations in this module to a cohesive operation that is
-- parameterized by an 'TransitionCostMatrix'.
--
-- Reused internally by different implementations.
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
directOptimization overlapFunction _renderingFunction matrixFunction lhs rhs = {- trace (renderingFunction lhs rhs traversalMatrix) -} (alignmentCost, alignmentContext)
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
measureCharacters :: (Foldable f, Ord s) => f s -> f s -> (Bool, f s, f s)
measureCharacters lhs rhs
  | lhsOrdering == LT = ( True, rhs, lhs)
  | otherwise         = (False, lhs, rhs)
  where
    lhsOrdering =
        case comparing length lhs rhs of
          EQ -> toList lhs `compare` toList rhs
          x  -> x


-- |
-- Internal generator function for the matrices based on the Needleman-Wunsch
-- definition described in their paper.
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
  |  p == (0,0)              = (            0,  DiagArrow, gapGroup)
  |   topElement == gapGroup
  && leftElement == gapGroup
  && row /= 0
  && col /= 0                = (leftwardValue,  LeftArrow, gapGroup)
--  && col /= 0                = (diagonalValue,  DiagArrow, gapGroup)
  |   topElement == gapGroup
  && col /= 0                = (leftwardValue,  LeftArrow, gapGroup)
  |  leftElement == gapGroup
  && row /= 0                = (  upwardValue,    UpArrow, gapGroup)
  |  minDir == DiagArrow
  && minState == gapGroup
  && maybe False isInDel  topContext
  && maybe False isInDel leftContext = (leftwardValue,  LeftArrow, gapGroup)
  
--      if      isGapDim leftContext then (  upwardValue,   UpArrow, gapGroup)
--      else if isGapDim  topContext then (leftwardValue, LeftArrow, gapGroup)
--      else                              (diagonalValue, DiagArrow, gapGroup)

--      else if leftwardState == gapGroup  then (leftwardValue,  LeftArrow, gapGroup)
--      else if   upwardState == gapGroup  then (  upwardValue,    UpArrow, gapGroup)
{-
      else    error $ unlines [ "Cool corner case reached!"
                              , "Considering point: " <> show p
                              , "Top context:     " <> show ((col - 1) `lookup` topChar)
                              , "Left context:    " <> show ((row - 1) `lookup` leftChar)
                              , "  Upward point:  " <> show (memo !? (row - 1, col    ))
                              , "Diagonal point:  " <> show (memo !? (row - 1, col - 1))
                              , "Leftward point:  " <> show (memo !? (row    , col - 1))
                              ]
-}
  |  otherwise               = (      minCost,     minDir, minState)
  where
    -- | Lookup with a default value of infinite cost.
    {-# INLINE (!?) #-}
    (!?) m k = fromMaybe (infinity, DiagArrow, gapGroup) $ k `lookup` m

    isGapDim = maybe False (\x -> gapGroup == symbolAlignmentMedian x && isInDel x)

    isInDel Align {} = False
    isInDel _ = True
    
    topContext                    = (col - 1) `lookup` topChar
    leftContext                   = (row - 1) `lookup` leftChar
    topElement                    = maybe gapGroup symbolAlignmentMedian topContext
    leftElement                   = maybe gapGroup symbolAlignmentMedian leftContext
    (leftwardValue, leftwardArrow, leftWardState)         = memo !? (row    , col - 1)
    (  upwardValue,   upwardArrow,   upwardState)         = memo !? (row - 1, col    )
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
renderCostMatrix
  :: ( Foldable  f
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

    headerRow = mconcat
        [ " "
        , pad maxPrefixWidth "⊗"
        , "┃ "
        , pad maxColumnWidth "⁎"
        , concatMap (pad maxColumnWidth) longerTokens
        ]

    barRow    = mconcat
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
    minimumBy (comparing (\(c,_,d) -> (c,d)))
      [ (diagCost ,  diagChar       , DiagArrow)
      , (rightCost, rightChar <> gap, LeftArrow)
      , (downCost ,  downChar <> gap, UpArrow  )
      ]
