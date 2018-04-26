{-# LANGUAGE BangPatterns #-}

module Data.TCM
  ( SymbolChangeMatrix
  , TransitionCostMatrix
  -- * Construction
  , buildSymbolChangeMatrix
  , buildTransitionCostMatrix
  -- * Querries
  , overlap
  ) where

import Data.Alphabet
import Data.Foldable
import Data.Hashable
import Data.HashMap.Lazy hiding ((!))
import Data.Key
import Data.List.NonEmpty       (NonEmpty)
import Data.Matrix.ZeroIndexed  (Matrix)
import Data.Pointed
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.SymbolString


-- |
-- A generalized function representationing cost to change between two symbols.
type SymbolChangeMatrix k = k -> k -> Word


-- |
-- A generalized function representationing transition between two
-- 'SymbolAmbiguityGroup's, returning the corresponding median
-- 'SymbolAmbiguityGroup' and transition cost.
type TransitionCostMatrix k
     =  SymbolAmbiguityGroup k
     -> SymbolAmbiguityGroup k
     -> (SymbolAmbiguityGroup k, Word)


buildSymbolChangeMatrix
  :: (Eq k, Hashable k)
  => Alphabet k
  -> Matrix Word
  -> SymbolChangeMatrix k
buildSymbolChangeMatrix alphabet matrix x y = (memoizedStructure ! x) ! y
  where
    memoizedStructure = fromList $ foldMapWithKey buildRow alphabet
      where
        buildRow i rowSymbol = [(rowSymbol, fromList $ foldMapWithKey buildCell alphabet)]
          where
            buildCell j colSymbol = [(colSymbol, matrix ! (i, j))]
                  

buildTransitionCostMatrix
  :: (Ord k, Hashable k)
  => Alphabet k
  -> SymbolChangeMatrix k
  -> TransitionCostMatrix k
buildTransitionCostMatrix alphabet scm = overlap alphabet scm 


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
overlap
  :: ( Foldable1 f
     , Ord a
     )
  => f a
  -> SymbolChangeMatrix a
  -> SymbolAmbiguityGroup a
  -> SymbolAmbiguityGroup a
  -> (SymbolAmbiguityGroup a, Word)
overlap allSymbols costStruct lhs rhs = 
    case lhs /\ rhs of
      Nothing -> minimalChoice $ symbolDistances allSymbols costStruct lhs rhs
      Just xs -> (xs, 0)


-- |
-- Given a structure of unambiguous symbols and costs, calculates the least 
-- costly intersection of unambiguous character elements and the cost of that
-- intersection.
minimalChoice :: (Semigroup a, Foldable1 t, Ord a, Ord c) => t (a, c) -> (a, c)
minimalChoice = foldl1 f
  where
    f (!symbol1, !cost1) (!symbol2, !cost2) =
        case cost1 `compare` cost2 of
          EQ -> (symbol1 <> symbol2, cost1)
          LT -> (symbol1           , cost1)
          GT -> (symbol2           , cost2)


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
  -> SymbolChangeMatrix a
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

