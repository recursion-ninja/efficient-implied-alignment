-----------------------------------------------------------------------------
-- |
-- Module      :  Alignment.Pairwise.Test
-- Copyright   :  (c) 2018 Alex Washburn
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-- Test suite for general analysis operations
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Alignment.Pairwise.Test where


import Alignment.Pairwise
--import Alignment.Pairwise.Internal
--import Alignment.Pairwise.NeedlemanWunsch
--import Alignment.Pairwise.Ukkonen
import Data.Alphabet
import Data.Foldable
import Data.List.NonEmpty          (NonEmpty(..))
--import Data.MonoTraversable
import Data.Semigroup
import Data.SymbolString    hiding (filterGaps)
import Data.TCM
import Data.Vector.NonEmpty hiding (filter, reverse)
import Test.NucleotideSequence
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Test.Tasty.SmallCheck as SC


type ResultType = (Word, [SymbolAmbiguityGroup], [SymbolAmbiguityGroup], [SymbolAmbiguityGroup], [SymbolAmbiguityGroup])


testSuite :: TestTree
testSuite = testGroup "Pariwise alignment tests"
    [ testSuiteNaiveDO
    , testSuiteMemoizedDO
    , testSuiteUkkonnenDO
    , constistentImplementation
    ]


toOtherReturnContext
  :: (Word, Vector SymbolContext)
  -> ResultType
toOtherReturnContext (cost, contextVector) =
    let (a, b, c) = unzip3 $ f <$> toList contextVector
    in (cost, filterGaps a, a, b, c)
  where
    f (Align  x y z) = (x,   y,   z)
    f (Delete x y  ) = (x,   y, gap)
    f (Insert x   z) = (x, gap,   z)


filterGaps :: [SymbolAmbiguityGroup] -> [SymbolAmbiguityGroup]
filterGaps = filter (/=gap)


constistentImplementation = testGroup "All implementations return same states"
    [ consistentResults "Consistenty over discrete metric" discreteMetric
    , consistentResults "Consistenty over L1 norm" l1Norm
    , consistentResults "Consistenty over prefer substitution metric (1:2)" preferSubMetric
    , consistentResults "Consistenty over prefer insertion/deletion metric (2:1)" preferGapMetric
    ]


consistentResults :: String -> (Int -> Int -> Word) -> TestTree
consistentResults label metric = SC.testProperty label $ SC.forAll checkConsistency
  where
    tcm = buildTransitionCostMatrix alphabet metric

    f :: SymbolAmbiguityGroup -> SymbolString
    f = fromNonEmpty . (:|[]) . (\x -> Align x x x)

    checkConsistency :: (NucleotideBase, NucleotideBase) -> Bool
    checkConsistency (NB x, NB y) = naiveResult == memoedResult && naiveResult == ukkonenResult
      where
        naiveResult   = naiveDO     alphabet metric (f x) (f y)
        memoedResult  = naiveDOMemo alphabet tcm    (f x) (f y)
        ukkonenResult = ukkonenDO   alphabet tcm    (f x) (f y)


testSuiteNaiveDO = testGroup "Naive DO"
    [ isValidPairwiseAlignment "Naive DO over discrete metric"
       $ \x y -> toOtherReturnContext (naiveDO alphabet discreteMetric x y)
    , isValidPairwiseAlignment "Naive DO over L1 norm"
       $ \x y -> toOtherReturnContext (naiveDO alphabet l1Norm x y)
    , isValidPairwiseAlignment "Naive DO over prefer substitution metric (1:2)"
       $ \x y -> toOtherReturnContext (naiveDO alphabet preferSubMetric x y)
    , isValidPairwiseAlignment "Naive DO over prefer insertion/deletion metric (2:1)"
       $ \x y -> toOtherReturnContext (naiveDO alphabet preferGapMetric x y)
    ]


testSuiteMemoizedDO = testGroup "Memoized DO"
    [ isValidPairwiseAlignment "Memoized DO over discrete metric"
       $ \x y -> toOtherReturnContext (naiveDOMemo alphabet (buildTransitionCostMatrix alphabet discreteMetric) x y)
    , isValidPairwiseAlignment "Memoized DO over L1 norm"
       $ \x y -> toOtherReturnContext (naiveDOMemo alphabet (buildTransitionCostMatrix alphabet l1Norm) x y)
    , isValidPairwiseAlignment "Memoized DO over prefer substitution metric (1:2)"
       $ \x y -> toOtherReturnContext (naiveDOMemo alphabet (buildTransitionCostMatrix alphabet preferSubMetric) x y)
    , isValidPairwiseAlignment "Memoized DO over prefer insertion/deletion metric (2:1)"
       $ \x y -> toOtherReturnContext (naiveDOMemo alphabet (buildTransitionCostMatrix alphabet preferGapMetric) x y)
    ]


testSuiteUkkonnenDO = testGroup "Ukkonnen DO"
    [ isValidPairwiseAlignment "Ukkonnen DO over discrete metric"
       $ \x y -> toOtherReturnContext (ukkonenDO alphabet (buildTransitionCostMatrix alphabet discreteMetric) x y)
    , isValidPairwiseAlignment "Ukkonnen DO over L1 norm"
       $ \x y -> toOtherReturnContext (ukkonenDO alphabet (buildTransitionCostMatrix alphabet l1Norm) x y)
    , isValidPairwiseAlignment "Ukkonnen DO over prefer substitution metric (1:2)"
       $ \x y -> toOtherReturnContext (ukkonenDO alphabet (buildTransitionCostMatrix alphabet preferSubMetric) x y)
    , isValidPairwiseAlignment "Ukkonnen DO over prefer insertion/deletion metric (2:1)"
       $ \x y -> toOtherReturnContext (ukkonenDO alphabet (buildTransitionCostMatrix alphabet preferGapMetric) x y)
    ]



isValidPairwiseAlignment
  :: String
  -> (SymbolString -> SymbolString -> ResultType)
  -> TestTree
isValidPairwiseAlignment label alignmentFunction = testGroup label
    [ testProperty "alignment function is commutative"               commutivity
    , testProperty "aligned results are all equal length"            resultsAreEqualLength
    , testProperty "output length is >= input length"                greaterThanOrEqualToInputLength
    , testProperty "alignment length is =< sum of input lengths"     greaterThanOrEqualToInputLength
    , testProperty "output alignments were not erroneously swapped"  outputsCorrespondToInputs
    , testProperty "output alignments were not erroneously reversed" outputsAreNotReversed
    , testProperty "output alignments only contain new gaps"         filterGapsEqualsInput
    , testProperty "ungapped output contains no gaps"                ungappedHasNogaps
    ]
  where
    commutivity :: (NucleotideSequence, NucleotideSequence) -> Property
    commutivity (NS lhs, NS rhs) =
        a === x .&&. b === z .&&. c === y
      where
        (_, _, a, b, c) = alignmentFunction lhs rhs
        (_, _, x, y, z) = alignmentFunction rhs lhs

    resultsAreEqualLength :: (NucleotideSequence, NucleotideSequence) -> Property
    resultsAreEqualLength (NS lhs, NS rhs) =
        length med === length lhs' .&&. length lhs' === length rhs'
      where
        (_, _, med, lhs', rhs') = alignmentFunction lhs rhs

    greaterThanOrEqualToInputLength :: (NucleotideSequence, NucleotideSequence) -> Bool
    greaterThanOrEqualToInputLength (NS lhs, NS rhs) =
        length lhs' >= length lhs && length rhs' >= length rhs
      where
        (_, _, _, lhs', rhs') = alignmentFunction lhs rhs

    totalAlignmentLengthLessThanOrEqualToSumOfLengths :: (NucleotideSequence, NucleotideSequence) -> Property
    totalAlignmentLengthLessThanOrEqualToSumOfLengths (NS lhs, NS rhs) =
        counterexample shownCounterexample $ medLen <= lhsLen + rhsLen
      where
        (_, _, med, _, _) = alignmentFunction lhs rhs
        medLen = length med
        lhsLen = length lhs
        rhsLen = length rhs
        shownCounterexample = unwords [ show medLen, ">", show lhsLen, "+", show rhsLen ]

    outputsCorrespondToInputs :: (NucleotideSequence, NucleotideSequence) -> Property
    outputsCorrespondToInputs (NS lhs, NS rhs) =
        medianList lhs /= medianList rhs ==>
            counterexample "lhs' === rhs" (filterGaps lhs' /= filterGaps (medianList rhs)) .&&.
            counterexample "rhs' === lhs" (filterGaps rhs' /= filterGaps (medianList lhs))
      where
        (_, _, _, lhs', rhs') = alignmentFunction lhs rhs

    outputsAreNotReversed :: (NucleotideSequence, NucleotideSequence) -> Property
    outputsAreNotReversed (NS lhs, NS rhs) =
        counterexample (show lhs <> show lhs') (isNotPalindrome (medianList lhs) ==> isNotReversed (filterGaps lhs') (medianList lhs)) .&&.
        counterexample (show rhs <> show rhs') (isNotPalindrome (medianList rhs) ==> isNotReversed (filterGaps rhs') (medianList rhs))
      where
        (_, _, _, lhs', rhs') = alignmentFunction lhs rhs
        isNotReversed x y = reverse (toList x) /= toList y
        isNotPalindrome x = reverse (toList x) /= toList x

    filterGapsEqualsInput :: (NucleotideSequence, NucleotideSequence) -> Property
    filterGapsEqualsInput (NS lhs, NS rhs) =
        filterGaps lhs' === filterGaps (medianList lhs) .&&. filterGaps rhs' === filterGaps (medianList rhs)
      where
        (_, _, _, lhs', rhs') = alignmentFunction lhs rhs

    ungappedHasNogaps :: (NucleotideSequence, NucleotideSequence) -> Property
    ungappedHasNogaps (NS lhs, NS rhs) =
        filterGaps unGap === unGap
      where
        (_, unGap, _, _, _) = alignmentFunction lhs rhs


medianList :: SymbolString -> [SymbolAmbiguityGroup]
medianList = fmap symbolAlignmentMedian . toList


alphabet :: Alphabet String
alphabet = fromSymbols ["A","C","G","T"] 


gap :: SymbolAmbiguityGroup
gap = encodeAmbiguityGroup alphabet . (:|[]) $ gapSymbol alphabet


discreteMetric :: (Num a, Num b, Ord a, Ord b) => a -> a -> b
discreteMetric i j = if i /= j then 1 else 0


l1Norm :: Int -> Int -> Word
l1Norm i j = toEnum $ max i j - min i j


preferGapMetric :: (Num a, Num b, Ord a, Ord b) => a -> a -> b
preferGapMetric i j
  | i == j    = 0
  | i == 4    = 1
  | j == 4    = 1
  | otherwise = 2


preferSubMetric :: (Num a, Num b, Ord a, Ord b) => a -> a -> b
preferSubMetric i j
  | i == j    = 0
  | i == 4    = 2
  | j == 4    = 2
  | otherwise = 1
