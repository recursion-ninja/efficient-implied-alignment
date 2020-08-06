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
  , measureAndUngapCharacters
  , needlemanWunschDefinition
  , renderCostMatrix
  -- * Gap removal and reinsertion
  , deleteGaps
  , insertGaps
  ) where


import           Alignment.Pairwise.Ukkonen.Matrix (UkkonenMethodMatrix)
import           Control.DeepSeq
import           Control.Monad.Loops                   (whileM)
import           Control.Monad.State.Strict
import           Control.Monad.ST
import           Data.Bifunctor
import           Data.Coerce
import           Data.Foldable
import           Data.IntMap                           (IntMap)
import qualified Data.IntMap                           as IM
import           Data.Key
import           Data.Matrix                       (Matrix, dim, unsafeIndex)
import           Data.Maybe                        (fromJust, fromMaybe)
import           Data.Ord
import           Data.Semigroup
import           Data.STRef
import           Data.SymbolString
import           Data.TCM
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Primitive       as P
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector                           as EV
import qualified Data.Vector.Mutable                   as MV
import qualified Data.Vector.Unboxed.Mutable           as MUV
import           Data.Vector.NonEmpty                  (Vector)
import qualified Data.Vector.NonEmpty                  as V
import           Data.Word                   (Word8)
import           Numeric.Extended.Natural
import           Prelude                           hiding (lookup, zipWith)


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
    basicUnsafeNew n = MV_Direction <$> M.basicUnsafeNew n

    {-# INLINE basicInitialize #-}
    basicInitialize (MV_Direction v) = M.basicInitialize v

    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeReplicate n x = MV_Direction <$> M.basicUnsafeReplicate n (fromDirection x)

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MV_Direction v) i = toDirection <$> M.basicUnsafeRead v i

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
    basicUnsafeGrow (MV_Direction v) n = MV_Direction <$> M.basicUnsafeGrow v n


instance G.Vector U.Vector Direction where

    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MV_Direction v) = V_Direction <$> G.basicUnsafeFreeze v

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (V_Direction v) = MV_Direction <$> G.basicUnsafeThaw v

    {-# INLINE basicLength #-}
    basicLength (V_Direction v) = G.basicLength v

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i n (V_Direction v) = V_Direction $ G.basicUnsafeSlice i n v

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (V_Direction v) i = toDirection <$> G.basicUnsafeIndexM v i

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
type MatrixFunction m
    =  TransitionCostMatrix
    -> SymbolString
    -> SymbolString
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
-- {-# SPECIALIZE directOptimization :: SymbolAmbiguityGroup -> TransitionCostMatrix -> (Vector SymbolContext -> Vector SymbolContext -> Matrix              (Cost, Direction, SymbolAmbiguityGroup) -> String) -> MatrixFunction Matrix              -> Vector  SymbolContext -> Vector SymbolContext -> (Word, Vector SymbolContext) #-}
-- {-# SPECIALIZE directOptimization :: SymbolAmbiguityGroup -> TransitionCostMatrix -> (Vector SymbolContext -> Vector SymbolContext -> UkkonenMethodMatrix (Cost, Direction, SymbolAmbiguityGroup) -> String) -> MatrixFunction UkkonenMethodMatrix -> Vector  SymbolContext -> Vector SymbolContext -> (Word, Vector SymbolContext) #-}
directOptimization
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     , Key m ~ (Int, Int)
     , MatrixConstraint m
     )
  => SymbolAmbiguityGroup
  -> TransitionCostMatrix
  -> (f SymbolContext -> f SymbolContext -> m (Cost, Direction, SymbolAmbiguityGroup) -> String)
  -> MatrixFunction m
  -> f SymbolContext
  -> f SymbolContext
  -> (Word, Vector SymbolContext)
directOptimization gap overlapλ _renderingFunction matrixFunction lhs rhs =
    let (swapped, gapsLesser, gapsLonger, shorterChar, longerChar) = measureAndUngapCharacters gap lhs rhs
        (alignmentCost, ungappedAlignment) =
          case (shorterChar, longerChar) of
            (Nothing, Just ys) ->
                let f x = let m = symbolAlignmentMedian x in insertElement (fst $ overlapλ m gap) m
                in  (0, Just $ f <$> ys)
            (Just xs, Just ys) ->
                let traversalMatrix = matrixFunction overlapλ ys xs
                in  second Just $ traceback traversalMatrix ys xs
            _                  -> (0, Nothing)

        transformation    = if swapped then fmap reverseContext else id
        regappedAlignment = insertGaps gap gapsLesser gapsLonger ungappedAlignment
        alignmentContext  = transformation regappedAlignment
    in  (alignmentCost, alignmentContext)


-- |
-- Strips the gap symbols from the supplied string.
--filterGaps :: NonEmpty (SymbolContext s) -> NonEmpty (SymbolContext s)
--filterGaps = NE.fromList . NE.filter (/= gap)


{-# INLINEABLE measureCharacters #-}
{-# SPECIALISE measureCharacters :: Vector SymbolContext -> Vector SymbolContext -> (Bool, [SymbolContext], [SymbolContext]) #-}
measureCharacters
  :: Foldable f
  => f SymbolContext
  -> f SymbolContext
  -> (Bool, [SymbolContext], [SymbolContext])
measureCharacters lhs rhs =
    let f = maybe [] toList
        (b, x, y) = measureNullableCharacters (Just lhs) (Just rhs)
    in  (b, f x, f y)


-- |
-- /O(1)/ for input strings of differing lengths
--
-- /O(k)/ for input strings of equal length, where /k/ is the shared prefix of
-- both characters.
--
-- Returns the string that is shorter first, longer second, and notes whether or
-- not the inputs were swapped to place the strings in this ordering.
--
-- Handles equal length characters by considering the lexicographically larger
-- string as longer.
--
-- Handles equality of inputs by /not/ swapping.
{-# INLINEABLE measureNullableCharacters #-}
{-# SPECIALISE measureNullableCharacters :: Maybe (Vector SymbolContext) -> Maybe (Vector SymbolContext) -> (Bool, Maybe (Vector SymbolContext), Maybe (Vector SymbolContext)) #-}
measureNullableCharacters
  :: Foldable f
  => Maybe (f SymbolContext)
  -> Maybe (f SymbolContext)
  -> (Bool, Maybe (f SymbolContext), Maybe (f SymbolContext))
measureNullableCharacters lhs rhs
  | lhsOrdering == GT = ( True, rhs, lhs)
  | otherwise         = (False, lhs, rhs)
  where
    lhsOrdering =
        -- First, compare inputs by length.
        case comparing (maybe 0 length) lhs rhs of
          -- If the inputs are equal length,
          -- Then compare by the (arbitary) lexicographical ordering of the median states.
          EQ -> let x = maybe [] toList lhs
                    y = maybe [] toList rhs
                    f = fmap symbolAlignmentMedian
                in  case f x `compare` f y of
                      -- If the input median states have the same ordering,
                      -- Lastly, we compare by the lexicographic ordering of the "tagged triples."
                      --
                      -- If they are equal after this step,
                      -- Then the inputs are representationally equal.
                      -- Actually, honest to goodness 100% equal!
                      EQ -> x `compare` y
                      v  -> v
          v  -> v


-- |
-- /O(1)/ for input characters of differing lengths
--
-- /O(k)/ for input characters of equal length, where /k/ is the shared prefix of
-- both characters.
--
-- Considers the median values of the characters, ignores the left/right tagging.
--
-- First remove the gaps from the input characters.
--
-- If both "ungapped" inputs are empty, we measure the original "gapped" inputs to
-- determine if the inputs need to be swapped. This is requried to ensure comutativity
-- of subsequent operations which use this method.
--
-- Returns the "ungapped" dynamic character that is "shorter" first, "longer" second,
-- the removed gap mappings (in the same order), and notes whether or not the inputs
-- were swapped to place the characters in this ordering.
--
-- Handles equal length characters by considering the lexicographically larger
-- character as longer.
--
-- Handles equality of inputs by /not/ swapping.
{-# INLINE measureAndUngapCharacters #-}
{-# SPECIALISE measureAndUngapCharacters :: SymbolAmbiguityGroup -> Vector SymbolContext -> Vector SymbolContext -> (Bool, IntMap Word, IntMap Word, Maybe (Vector SymbolContext), Maybe (Vector SymbolContext)) #-}
measureAndUngapCharacters
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     )
  => SymbolAmbiguityGroup
  -> f SymbolContext
  -> f SymbolContext
  -> (Bool, IntMap Word, IntMap Word, Maybe SymbolString, Maybe SymbolString)
measureAndUngapCharacters gap char1 char2
  | swapInputs = (True , gapsChar2, gapsChar1, ungappedChar2, ungappedChar1)
  | otherwise  = (False, gapsChar1, gapsChar2, ungappedChar1, ungappedChar2)
  where
    (gapsChar1, ungappedChar1) = deleteGaps gap char1
    (gapsChar2, ungappedChar2) = deleteGaps gap char2
    swapInputs =
      let needToSwap (x,_,_) = x
          ungappedLen1 = maybe 0 length ungappedChar1
          ungappedLen2 = maybe 0 length ungappedChar2
      in  case ungappedLen1 `compare` ungappedLen2 of
            EQ | ungappedLen1 == 0 -> needToSwap $ measureNullableCharacters (Just char1)  (Just char2)
            _                      -> needToSwap $ measureNullableCharacters ungappedChar1 ungappedChar2


-- |
-- Strips the gap elements from the supplied character.
--
-- Remembers the locations of the gap characters that were deleted
--
-- If the character contains /only/ gaps, a missing character is returned.
{-# INLINEABLE deleteGaps #-}
deleteGaps
  :: ( Foldable f
     , Indexable f
     , Key f ~ Int
     )
  => SymbolAmbiguityGroup
  -> f SymbolContext
  -> (IntMap Word, Maybe SymbolString)
deleteGaps gap bvs
      | null gaps   = (gaps,         Just . V.generate (length bvs) $ \i -> bvs ! i)
      | newLen == 0 = (gaps,                 mempty)
      | otherwise   = (gaps, force $ Just newVector)
      where
        newVector = runST $ do
            j <- newSTRef 0
            let isGapAtJ = do
                  j' <- readSTRef j
                  pure $ j' < charLen && (symbolAlignmentMedian (bvs ! j') == gap)

            let g = do
                  void $ whileM isGapAtJ (modifySTRef j succ)
                  j' <- readSTRef j
                  modifySTRef j succ
                  pure $ bvs ! j'
                  
            V.generateM newLen $ const g

        gapCount = fromEnum . getSum $ foldMap Sum gaps
        charLen  = length bvs
        newLen   = charLen - gapCount

        gaps = IM.fromDistinctAscList $ reverse refs

        refs :: [(Int, Word)]
        refs = runST $ do
            nonGaps <- newSTRef 0
            prevGap <- newSTRef False
            gapLen  <- newSTRef 0
            gapRefs <- newSTRef []

            for_ [0 .. charLen - 1] $ \i ->
              if symbolAlignmentMedian (bvs ! i)  == gap
              then modifySTRef gapLen succ *> writeSTRef prevGap True
              else do gapBefore <- readSTRef prevGap
                      when gapBefore $ do
                        j <- readSTRef nonGaps
                        g <- readSTRef gapLen
                        modifySTRef gapRefs ( (j,g): )
                        writeSTRef  gapLen 0
                        writeSTRef prevGap False
                      modifySTRef nonGaps succ
            
            gapBefore <- readSTRef prevGap
            when gapBefore $ do
              j <- readSTRef nonGaps
              g <- readSTRef gapLen
              modifySTRef gapRefs ( (j,g): )
            readSTRef gapRefs


-- |
-- Adds gaps elements to the supplied character.
insertGaps
  :: ( Enum a
     , MUV.Unbox a
     , Num a
     , Eq a
     )
  => SymbolAmbiguityGroup
  -> IntMap a
  -> IntMap a
  -> Maybe SymbolString
  -> SymbolString
insertGaps gap lGaps rGaps meds
      | null lGaps && null rGaps = fromJust meds -- No work needed
      | otherwise                = force . coerce $ newVector
      where
        totalGaps = fromEnum . getSum . foldMap Sum
        gapVecLen = maybe 0 (succ . fst) . IM.lookupMax
        mLength   = maybe 0 length meds
        lGapCount = totalGaps lGaps
        rGapCount = totalGaps rGaps
        newLength = lGapCount + rGapCount + mLength

        xs !> i = maybe (error "Tried to index an empty alignment context when reinserting gaps") (!i) xs
        
        newVector = EV.create $ do
          mVec <- MV.unsafeNew newLength
          lVec <- MUV.replicate (gapVecLen lGaps) 0
          rVec <- MUV.replicate (gapVecLen rGaps) 0
          lGap <- newSTRef 0
          mPtr <- newSTRef 0
          rGap <- newSTRef 0

          -- Write out to the mutable vectors
          for_ (IM.toAscList lGaps) $ uncurry (MUV.unsafeWrite lVec)
          for_ (IM.toAscList rGaps) $ uncurry (MUV.unsafeWrite rVec)

          let align i = do
                    m <- readSTRef mPtr
                    let e = meds !> m
                    let v = coerce e
                    MV.unsafeWrite mVec i v
                    modifySTRef mPtr succ
                    when (isAlign e || isDelete e) $
                      modifySTRef lGap succ
                    when (isAlign e || isInsert e) $
                      modifySTRef rGap succ

          let checkRightGapReinsertion i = do
                rg <- readSTRef rGap
                v  <- if rg >= MUV.length rVec then pure 0 else MUV.unsafeRead rVec rg
                if   v == 0
                then align i
                else do MV.unsafeWrite mVec i $ insertElement gap gap
                        MUV.unsafeWrite rVec rg $ v - 1

          for_ [0 .. newLength - 1] $ \i -> do
            -- Check if we need to insert a gap from the left char
            lg <- readSTRef lGap
            v  <- if lg >= MUV.length lVec then pure 0 else MUV.unsafeRead lVec lg
            if   v == 0
            then checkRightGapReinsertion i
            else do MV.unsafeWrite mVec i $ deleteElement gap gap
                    MUV.unsafeWrite lVec lg $ v - 1

          pure mVec


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
--  |  col /= 0 &&  topElement == gapGroup = (leftwardValue, LeftArrow, gapGroup)
--  |  row /= 0 && leftElement == gapGroup = (  upwardValue,   UpArrow, gapGroup)
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
    rightCost                     = rightOverlapCost +  leftwardValue
    diagCost                      =  diagOverlapCost +  diagonalValue
    downCost                      =  downOverlapCost +    upwardValue
    (minCost, minState, minDir)   = getMinimalCostDirection gapGroup
                                      ( diagCost,  diagChar)
                                      (rightCost, rightChar)
                                      ( downCost,  downChar)


{--}
-- |
-- Serializes an alignment matrix to a 'String'. Uses input characters for row
-- and column labelings.
--
-- Useful for debugging purposes.
{-# INLINEABLE renderCostMatrix #-}
--{-# SPECIALIZE renderCostMatrix :: SymbolAmbiguityGroup -> Vector SymbolContext -> Vector SymbolContext -> Matrix              (Cost, Direction, SymbolAmbiguityGroup) -> String #-}
--{-# SPECIALIZE renderCostMatrix :: SymbolAmbiguityGroup -> Vector SymbolContext -> Vector SymbolContext -> UkkonenMethodMatrix (Cost, Direction, SymbolAmbiguityGroup) -> String #-}
renderCostMatrix
  :: ( Foldable f
     , Lookup m
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
    (_,lesser,longer) = measureCharacters lhs rhs
    longerTokens      = toShownIntegers longer
    lesserTokens      = toShownIntegers lesser
    toShownIntegers   = fmap renderContext . toList
    matrixTokens      = [ (\j -> maybe "" showCell $ (i,j) `lookup` mtx)
                           <$> [ 0 .. colCount - 1 ]

                        | i <- [ 0 .. rowCount - 1 ]
                        ]                                
    showCell (c,d,_)  = show c <> show d
    maxPrefixWidth    = maxLengthOf lesserTokens + 1
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

    renderedRows = unlines
                 . zipWithKey renderRow ("⁎":lesserTokens)
                 $ matrixTokens
      where
        renderRow k e cs = fold [" ", pad maxPrefixWidth (e <> show (k `mod` 10)), "┃ ", fold $ pad maxColumnWidth <$> cs ]

    renderContext (Align  x _ _) = if x == gapGroup then "—" else "α"
    renderContext (Delete x _  ) = if x == gapGroup then "—" else "δ"
    renderContext (Insert x   _) = if x == gapGroup then "—" else "ι"
    renderContext Gapping{}      = "—"

    pad :: Int -> String -> String
    pad n e = replicate (n - len) ' ' <> e <> " "
      where
        len = length e
{--}


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
traceback alignMatrix longerChar lesserChar = force (unsafeToFinite cost, V.reverse $ V.unfoldr go lastCell)
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
                LeftArrow -> ((i    , j - 1), Insert medianElement (symbolAlignmentMedian $ longerChar ! (j - 1)))
                UpArrow   -> ((i - 1, j    ), Delete medianElement (symbolAlignmentMedian $ lesserChar ! (i - 1)))
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
