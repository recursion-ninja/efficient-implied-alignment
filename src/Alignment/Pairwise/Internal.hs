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
-- two dynamic characters.
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns, ConstraintKinds, FlexibleContexts, TypeFamilies #-}

module Alignment.Pairwise.Internal
 ( Cost
 , Direction(..)
 , DOCharConstraint
 , MatrixConstraint
 , MatrixFunction
 , NeedlemanWunchMatrix
 , OverlapFunction
 -- * Direct Optimization primative construction functions
 , directOptimization
 , measureCharacters
 , needlemanWunschDefinition
-- , renderCostMatrix
 , traceback
 -- * Probably removable
 , overlap
 , overlapConst
 , getOverlap
 , minimalChoice
 ) where


import           Control.Arrow            ((&&&))
import           Data.Bits
import           Data.DList               (snoc)
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty       (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Matrix.ZeroIndexed  (Matrix)
import           Data.Maybe               (fromMaybe)
import           Data.MonoTraversable
import           Data.Ord
import           Data.Pointed
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.SymbolString
import           Numeric.Extended.Natural
import           Prelude            hiding (lookup, zipWith)


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
-- Constraints on the input dynamic characters that direct optimization requires
-- to operate.
type DOCharConstraint s = NonEmpty (SymbolContext s)


-- |
-- Constraints on the type of structure a "matrix" exposes to be used in rendering
-- and traceback functions.
type MatrixConstraint m = (Indexable m, Key m ~ (Int, Int))


-- |
-- A parameterized function to generate an alignment matrix.
type MatrixFunction m f s
    =  OverlapFunction (SymbolAmbiguityGroup s)
    -> f (SymbolContext s)
    -> f (SymbolContext s)
    -> m (Cost, Direction, SymbolAmbiguityGroup s)


-- |
-- A generalized function representation: the "overlap" between dynamic character
-- elements, supplying the corresponding median and cost to align the two
-- characters.
type OverlapFunction e = e -> e -> (e, Word)


-- |
-- Wraps the primative operations in this module to a cohesive operation that is
-- parameterized by an 'OverlapFunction'.
--
-- Reused internally by different implementations.
directOptimization
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     , MatrixConstraint m
     , Ord s
     )
  => OverlapFunction (SymbolAmbiguityGroup s)
  -> MatrixFunction m f s
  -> f (SymbolContext s)
  -> f (SymbolContext s)
  -> (Word, NonEmpty (SymbolContext s))
directOptimization overlapFunction matrixFunction lhs rhs = (alignmentCost, alignmentContext)
  where
    (swapped, longerInput, shorterInput) = measureCharacters lhs rhs
    traversalMatrix                      = matrixFunction overlapFunction longerInput shorterInput
    (alignmentCost, uncommutedContext)   = traceback      traversalMatrix longerInput shorterInput
    alignmentContext 
      | swapped   = reverseContext <$> uncommutedContext
      | otherwise = uncommutedContext


-- |
-- Strips the gap elements from the supplied character.
--
-- If the character contains /only/ gaps, a missing character is returned.
--filterGaps :: NonEmpty (SymbolContext s) -> NonEmpty (SymbolContext s)
--filterGaps = NE.fromList . NE.filter (/= gap)


-- |
-- /O(1)/ for input characters of differing lengths
--
-- /O(k)/ for input characters of equal length, where /k/ is the shared prefix of
-- both characters.
--
-- Returns the dynamic character that is longer first, shorter second, and notes
-- whether or not the inputs were swapped to place the characters in this ordering.
--
-- Handles equal length characters by considering the lexicographically larger
-- character as longer.
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
     , Ord s
     )
  => s
  -> OverlapFunction (SymbolAmbiguityGroup s)
  -> f (SymbolContext s)
  -> f (SymbolContext s)
  -> m (Cost, Direction, SymbolAmbiguityGroup s)
  -> (Int, Int)
  -> (Cost, Direction, SymbolAmbiguityGroup s)
needlemanWunschDefinition gapValue overlapFunction topChar leftChar memo p@(row, col)
  | p == (0,0) = (      0, DiagArrow, gapGroup)
  | otherwise  = (minCost,    minDir, minState)
  where
    -- | Lookup with a default value of infinite cost.
    {-# INLINE (!?) #-}
    (!?) m k = fromMaybe (infinity, DiagArrow, gapGroup) $ k `lookup` m

    gapGroup                      = point gapValue
    topElement                    = maybe gapGroup symbolAlignmentMedian $ (col - 1) `lookup` topChar
    leftElement                   = maybe gapGroup symbolAlignmentMedian $ (row - 1) `lookup` leftChar
    (leftwardValue, _, _)         = memo !? (row    , col - 1)
    (diagonalValue, _, _)         = memo !? (row - 1, col - 1)
    (  upwardValue, _, _)         = memo !? (row - 1, col    )
    (rightChar, rightOverlapCost) = fromFinite <$> overlapFunction topElement gapGroup
    ( diagChar,  diagOverlapCost) = fromFinite <$> overlapFunction topElement leftElement
    ( downChar,  downOverlapCost) = fromFinite <$> overlapFunction gapGroup   leftElement
    rightCost                     = rightOverlapCost + leftwardValue
    diagCost                      =  diagOverlapCost + diagonalValue
    downCost                      =  downOverlapCost +   upwardValue
    (minCost, minState, minDir)   = getMinimalCostDirection gapValue
                                      ( diagCost,  diagChar)
                                      (rightCost, rightChar)
                                      ( downCost,  downChar)


-- |
-- Serializes an alignment matrix to a 'String'. Uses input characters for row
-- and column labelings.
--
-- Useful for debugging purposes.
{-
renderCostMatrix
  :: ( Enum (Element s)
     , Foldable f
     , Functor f
     , Indexable f
     , Key f ~ (Int, Int)
     , Ord s
     , Show a
     , Show b
     )
  => NonEmpty (SymbolContext s)
  -> NonEmpty (SymbolContext s)
  -> f (a, b, c) -- ^ The Needleman-Wunsch alignment matrix
  -> String
renderCostMatrix lhs rhs mtx = unlines
    [ dimensionPrefix
    , headerRow
    , barRow
    , renderedRows
    ]
  where
    (_,longer,lesser) = measureCharacters lhs rhs
    longerTokens      = toShownIntegers longer
    lesserTokens      = toShownIntegers lesser
    toShownIntegers   = fmap (show . fromEnum) . otoList
    matrixTokens      = showCell <$> mtx
    showCell (c,d,_)  = show c <> show d
    maxPrefixWidth    = maxLengthOf lesserTokens
    maxColumnWidth    = max (maxLengthOf longerTokens) . maxLengthOf $ toList matrixTokens
    maxLengthOf       = maximum . fmap length

    colCount = olength longer + 1
    rowCount = olength lesser + 1

    dimensionPrefix  = " " <> unwords
        [ "Dimensions:"
        , show rowCount
        , "X"
        , show colCount
        ]

    headerRow = mconcat
        [ " "
        , pad maxPrefixWidth "\\"
        , "| "
        , pad maxColumnWidth "*"
        , concatMap (pad maxColumnWidth) longerTokens
        ]

    barRow    = mconcat
        [ " "
        , bar maxPrefixWidth
        , "+"
        , concatMap (const (bar maxColumnWidth)) $ undefined : longerTokens
        ]
      where
        bar n = replicate (n+1) '-'

    renderedRows = unlines . zipWith renderRow ("*":lesserTokens) $ getRows matrixTokens
      where
        renderRow e vs = " " <> pad maxPrefixWidth e <> "| " <> concatMap (pad maxColumnWidth) vs

        getRows m = (`getRow'` m) <$> [0 .. rowCount - 1]
        getRow' i m = g <$> [0 .. colCount - 1]
          where
            g j = fromMaybe "" $ (i,j) `lookup` m


    pad :: Int -> String -> String
    pad n e = replicate (n - len) ' ' <> e <> " "
      where
        len = length e
-}


-- |
-- Performs the traceback of an 'NeedlemanWunchMatrix'.
--
-- Takes in an 'NeedlemanWunchMatrix', two 'EncodableDynamicCharacter's and returns an
-- aligned 'EncodableDynamicCharacter', as well as the aligned versions of the
-- two inputs. Essentially does the second step of Needleman-Wunsch, following
-- the arrows from the bottom right corner, accumulating the sequences as it goes,
-- but returns three alignments: the left character, the right character, and the
-- parent. The child alignments *should* be biased toward the shorter of the two
-- dynamic characters.
traceback
  :: ( Foldable f
     , Indexable f
     , Indexable m
     , Key f ~ Int
     , Key m ~ (Int, Int)
     )
  => m (Cost, Direction, SymbolAmbiguityGroup s)
  -> f (SymbolContext s)
  -> f (SymbolContext s)
  -> (Word, NonEmpty (SymbolContext s))
traceback alignMatrix longerChar lesserChar = (unsafeToFinite cost, NE.unfoldr go lastCell)
  where
      lastCell     = (row, col)
      (cost, _, _) = alignMatrix ! lastCell

      col = length longerChar
      row = length lesserChar

      go currentCell@(!i, !j)
        | nextCell == (0,0) = (contextElement, Nothing)
        | otherwise         = (contextElement, Just nextCell)
        where
          (cost, directionArrow, medianElement) = alignMatrix ! currentCell

          (nextCell, contextElement) =
              case directionArrow of
                LeftArrow -> ((i    , j - 1), Delete (unsafeToFinite cost) medianElement (symbolAlignmentMedian $ longerChar ! (j - 1)))
                UpArrow   -> ((i - 1, j    ), Insert (unsafeToFinite cost) medianElement (symbolAlignmentMedian $ lesserChar ! (i - 1)))
                DiagArrow -> ((i - 1, j - 1), Align  (unsafeToFinite cost) medianElement (symbolAlignmentMedian $ longerChar ! (j - 1)) (symbolAlignmentMedian $ lesserChar ! (i - 1)))


{--
 - Internal computations
 -}


-- |
-- Memoized wrapper of the overlap function
getOverlap
  :: ( Foldable1 f
     , Ord a
     )
  => f a
  -> SymbolAmbiguityGroup a
  -> SymbolAmbiguityGroup a
  -> (a -> a -> Word)
  -> (SymbolAmbiguityGroup a, Word)
getOverlap allSymbols lhs rhs costStruct = overlap allSymbols costStruct lhs rhs


-- |
-- Takes two 'EncodableStreamElement' and a symbol change cost function and
-- returns a tuple of a new character, along with the cost of obtaining that
-- character. The return character may be (or is even likely to be) ambiguous.
-- Will attempt to intersect the two characters, but will union them if that is
-- not possible, based on the symbol change cost function.
--
-- To clarify, the return character is an intersection of all possible least-cost
-- combinations, so for instance, if @ char1 == A,T @ and @ char2 == G,C @, and
-- the two (non-overlapping) least cost pairs are A,C and T,G, then the return
-- value is A,C,G,T.
overlap
  :: ( Foldable1 f
     , Ord a
     )
  => f a
  -> (a -> a -> Word)
  -> SymbolAmbiguityGroup a
  -> SymbolAmbiguityGroup a
  -> (SymbolAmbiguityGroup a, Word)
overlap allSymbols costStruct lhs rhs = 
    case lhs /\ rhs of
      Nothing -> minimalChoice $ symbolDistances allSymbols costStruct lhs rhs
      Just xs -> (xs, 0)


-- |
-- Given a structure of unambiguous character elements and costs, calculates the
-- least costly intersection of unambiguous character elements and the cost of
-- that intersection.
minimalChoice :: (Semigroup a, Foldable1 t, Ord a, Ord c) => t (a, c) -> (a, c)
minimalChoice = foldl1 f
  where
    f (!symbol1, !cost1) (!symbol2, !cost2) =
        case cost1 `compare` cost2 of
          EQ -> (symbol1 <> symbol2, cost1)
          LT -> (symbol1           , cost1)
          GT -> (symbol2           , cost2)


-- |
-- Finds the cost between all single, unambiguous symbols and two dynamic
-- character elements (ambiguity groups of symbols).
--
-- Takes in a symbol change cost function and two ambiguous elements of a dynamic
-- character and returns a list of tuples of all possible unambiguous pairings,
-- along with the cost of each pairing. The resulting elements each have exactly
-- two bits set.
symbolDistances
  :: Foldable1 f
  => f a
  -> (a -> a -> Word)
  -> SymbolAmbiguityGroup a
  -> SymbolAmbiguityGroup a
  -> NonEmpty (SymbolAmbiguityGroup a, Word)
symbolDistances allSymbols costStruct group1 group2 = foldMap1 costAndSymbol allSymbols
  where
    costAndSymbol i = pure (point i, cost1 + cost2)
      where
        cost1 = getDistance i group1
        cost2 = getDistance i group2

    getDistance i e = minimum $ costStruct i <$> toNonEmpty e


-- |
-- An overlap function that applies the discrete metric to aligning two elements.
overlapConst
  :: Ord a
  => SymbolAmbiguityGroup a
  -> SymbolAmbiguityGroup a
  -> (SymbolAmbiguityGroup a, Word)
overlapConst lhs rhs =
    case lhs /\ rhs of
      Nothing -> (lhs <> rhs, 1)
      Just xs -> (xs , 0)


getMinimalCostDirection
  :: ( Ord a
     , Ord c
     )
  => a
  -> (c, SymbolAmbiguityGroup a)
  -> (c, SymbolAmbiguityGroup a)
  -> (c, SymbolAmbiguityGroup a)
  -> (c, SymbolAmbiguityGroup a, Direction)
getMinimalCostDirection gap (diagCost, diagChar) (rightCost, rightChar) (downCost, downChar) =
    minimumBy (comparing (\(c,_,d) -> (c,d)))
      [ (diagCost ,  diagChar             , DiagArrow)
      , (rightCost, rightChar <> point gap, LeftArrow)
      , (downCost ,  downChar <> point gap, UpArrow  )
      ]
