{-# Language BangPatterns #-}
{-# Language ImportQualifiedPost #-}

module Data.TCM
    ( SymbolChangeMatrix
    , TransitionCostMatrix
      -- * Construction
    , buildSymbolChangeMatrix
    , buildTransitionCostMatrix
      -- * Querries
    , overlap
    , overlap'
    , renderTCM
    ) where

import Control.DeepSeq
import Data.Alphabet
import Data.Bits
import Data.Foldable
import Data.Key
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Matrix (Matrix, unsafeIndex)
import Data.Matrix qualified as M
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.SymbolString


-- |
-- /O(1)/ for practical purposes, technically \( \mathcal{O} \left( \log_16 \left( a \right) \right) \)
--
-- A generalized function representationing cost to change between two symbols.
type SymbolChangeMatrix k = k -> k -> Word


-- |
-- /O(a^2)/
--
-- A generalized function representationing transition between two
-- 'SymbolAmbiguityGroup's, returning the corresponding median
-- 'SymbolAmbiguityGroup' and transition cost.
type TransitionCostMatrix = SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> (SymbolAmbiguityGroup, Word)


-- |
-- /O(a^2)/
--
-- Build a 'SymbolChangeMatrix' from an 'Alphabet' and a square 'Matrix'. It is
-- assumed that @rows matrix == length alphabet@ and
-- @cols matrix == length alphabet@ and that the index of each row & column
-- corresponds to that index of the alphabet.
buildSymbolChangeMatrix :: Matrix Word -> SymbolChangeMatrix Int
buildSymbolChangeMatrix matrix = let m = force matrix in curry (unsafeIndex m)
--                                 in  (\x y -> getElem x y m)


renderTCM :: Alphabet Char -> TransitionCostMatrix -> String
renderTCM alphabet tcm = unlines . (headerRow :) $ fmap (foldMap render) listOfRows
    where
        len        = 1 `shiftL` length alphabet
        listOfRows = [ [ tcm (toEnum i) (toEnum j) | j <- [0, len - 1] ] | i <- [0, len - 1] ]
        headerRow =
            foldMap (\x -> fold ["|", renderMonospacedGroup alphabet (toEnum x), "|   "]) [1 .. len - 1]

        render :: Show a => (SymbolAmbiguityGroup, a) -> String
        render (x, y) = fold ["(", renderMonospacedGroup alphabet x, ",", show y, ") "]


-- |
-- /O(1)/
--
-- Build a 'TransitionCostMatrix' from an 'Alphabet' and a 'SymbolChangeMatrix'.
{-# INLINABLE buildTransitionCostMatrix #-}
buildTransitionCostMatrix :: Alphabet k -> SymbolChangeMatrix Int -> TransitionCostMatrix
buildTransitionCostMatrix alphabet scm =
    let len = 1 `shiftL` length alphabet
        m   = M.fromList
            (len, len)
            [ overlap alphabet scm (toEnum i) (toEnum j) | i <- [0 .. len - 1], j <- [0 .. len - 1] ]
    in  \i j -> unsafeIndex m (fromEnum i, fromEnum j)


-- |
-- Takes two 'SymbolAmbiguityGroup's and a symbol change cost function and
-- returns a tuple of a 'SymbolAmbiguityGroup', along with the cost of
-- obtaining that 'SymbolAmbiguityGroup'. The return 'SymbolAmbiguityGroup'
-- may be (or is even likely to be) ambiguous. Will attempt to intersect the
-- two 'SymbolAmbiguityGroup', but will union them if that is not possible,
-- based on the symbol change cost function.
--
-- To clarify, the return 'SymbolAmbiguityGroup' is an intersection of all
-- possible least-costly combinations, so for instance, if @ char1 == A,T @ and
-- @ char2 == G,C @, and the two (non-overlapping) least cost pairs are @ A,C @
-- and @ T,G @, then the return value is @ A,C,G,T @sy.
{-# INLINABLE overlap #-}
{-# SPECIALIZE overlap :: Alphabet SymbolAmbiguityGroup -> SymbolChangeMatrix Int -> SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> (SymbolAmbiguityGroup, Word) #-}
overlap
    :: Foldable1 f
    => f a
    -> SymbolChangeMatrix Int
    -> SymbolAmbiguityGroup
    -> SymbolAmbiguityGroup
    -> (SymbolAmbiguityGroup, Word)
overlap allSymbols costStruct lhs rhs
    | zeroBits == lhs || zeroBits == rhs = (zeroBits, 0)
    | otherwise = case lhs /\ rhs of
        Nothing -> minimalChoice $ symbolDistances allSymbols costStruct lhs rhs
        Just xs -> (xs, 0)


-- |
-- Given a structure of unambiguous symbols and costs, calculates the least
-- costly intersection of unambiguous character elements and the cost of that
-- intersection.
{-# INLINE minimalChoice #-}
{-# SPECIALIZE minimalChoice :: Foldable1 f => f (SymbolAmbiguityGroup, Word) -> (SymbolAmbiguityGroup, Word) #-}
{-# SPECIALIZE minimalChoice ::         NonEmpty (SymbolAmbiguityGroup, Word) -> (SymbolAmbiguityGroup, Word) #-}
minimalChoice :: (Semigroup a, Foldable1 t, Ord c) => t (a, c) -> (a, c)
minimalChoice = foldl1 f
    where
        f :: (Ord c, Semigroup a) => (a, c) -> (a, c) -> (a, c)
        f (!symbol1, !cost1) (!symbol2, !cost2) = case cost1 `compare` cost2 of
            EQ -> (symbol1 <> symbol2, cost1)
            LT -> (symbol1, cost1)
            GT -> (symbol2, cost2)


-- |
-- Finds the cost between all unambiguous symbols and two 'SymbolAmbiguityGroup'
-- (ambiguity groups of symbols).
--
-- Takes in a symbol change cost function and two ambiguous symbol sets and
-- returns a list of tuples of all possible unambiguous pairings, along with the
-- cost of each pairing. The resulting elements each have exactly two symbols.
symbolDistances
    :: Foldable1 f
    => f a
    -> SymbolChangeMatrix Int
    -> SymbolAmbiguityGroup
    -> SymbolAmbiguityGroup
    -> NonEmpty (SymbolAmbiguityGroup, Word)
symbolDistances allSymbols costStruct group1 group2 = foldMap1 costAndSymbol allKeys
    where
        allKeys :: NonEmpty Int
        allKeys = mapWithKey const $ toNonEmpty allSymbols

        costAndSymbol :: Int -> NonEmpty (SymbolAmbiguityGroup, Word)
        costAndSymbol i = pure (bit i, cost1 + cost2)
            where
                cost1 = getDistance i group1
                cost2 = getDistance i group2

        getDistance :: Int -> SymbolAmbiguityGroup -> Word
        getDistance i e = minimum $ costStruct i <$> NE.filter (e `testBit`) allKeys


-- |
-- Takes one or more elements of 'FiniteBits' and a symbol change cost function
-- and returns a tuple of a new character, along with the cost of obtaining that
-- character. The return character may be (or is even likely to be) ambiguous.
-- Will attempt to intersect the two characters, but will union them if that is
-- not possible, based on the symbol change cost function.
--
-- To clarify, the return character is an intersection of all possible least-cost
-- combinations, so for instance, if @ char1 == A,T @ and @ char2 == G,C @, and
-- the two (non-overlapping) least cost pairs are A,C and T,G, then the return
-- value is A,C,G,T.
{-# INLINE overlap' #-}
{-# SPECIALISE overlap' :: FiniteBits e => Int -> (Word -> Word -> Word) -> NonEmpty e -> (e, Word) #-}
{-# SPECIALISE overlap' :: Int -> (Word -> Word -> Word) -> NonEmpty SymbolAmbiguityGroup -> (SymbolAmbiguityGroup, Word) #-}
overlap'
    :: (FiniteBits e, Foldable1 f, Functor f)
    => Int                   -- ^ Alphabet size
    -> (Word -> Word -> Word) -- ^ Symbol change matrix (SCM) to determin cost
    -> f e                    -- ^ List of elements for of which to find the k-median and cost
    -> (e, Word)              -- ^ K-median and cost
overlap' size sigma xs = go size maxBound zero
    where
        zero = let wlog = getFirst $ foldMap1 First xs in wlog `xor` wlog

    --    go :: Int -> Word -> e -> (e, Word)
        go 0 theCost bits = (bits, theCost)
        go i oldCost bits =
            let i'               = i - 1
                newCost          = sum $ getDistance (toEnum i') <$> xs
                (minCost, bits') = case oldCost `compare` newCost of
                    EQ -> (oldCost, bits `setBit` i')
                    LT -> (oldCost, bits)
                    GT -> (newCost, zero `setBit` i')
            in  go i' minCost bits'

        getDistance :: Bits b => Word -> b -> Word
        getDistance i b = go' size (maxBound :: Word)
            where
                go' :: Int -> Word -> Word
                go' 0 a = a
                go' j a =
                    let j' = j - 1
                        a' = if b `testBit` j' then min a $ sigma i (toEnum j') else a
                    in  go' j' a'
