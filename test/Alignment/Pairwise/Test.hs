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

{-# Language FlexibleContexts #-}
{-# Language ImportQualifiedPost #-}
{-# Language TypeFamilies #-}

module Alignment.Pairwise.Test
    ( testSuite
    ) where

import Alignment.Pairwise
import Data.Alphabet
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.SymbolString hiding (filterGaps)
import Data.TCM
import Data.Vector.NonEmpty hiding (filter, reverse)
import Test.NucleotideSequence
import Test.Tasty
import Test.Tasty.QuickCheck (Property, counterexample, testProperty, (.&&.), (===), (==>))
import Test.Tasty.SmallCheck qualified as SC


type ResultType
    = (Word, [SymbolAmbiguityGroup], [SymbolAmbiguityGroup], [SymbolAmbiguityGroup], [SymbolAmbiguityGroup])


testSuite :: TestTree
testSuite = testGroup
    "Pairwise alignment tests"
    [testSuiteNaiveDO, testSuiteNeedlemanWunsch, testSuiteUkkonenDO, constistentImplementation]


toOtherReturnContext :: (Word, Vector SymbolContext) -> ResultType
toOtherReturnContext (cost, contextVector) =
    let (a, b, c) = unzip3 $ f <$> toList contextVector in (cost, filterGaps a, a, b, c)
    where
        f (Align x y z) = (x, y, z)
        f (Delete x y ) = (x, gap, y)
        f (Insert x z ) = (x, z, gap)
        f (Gapping _  ) = (gap, gap, gap)


filterGaps :: [SymbolAmbiguityGroup] -> [SymbolAmbiguityGroup]
filterGaps = filter (/= gap)


constistentImplementation :: TestTree
constistentImplementation = testGroup
    "All implementations return same states"
    [ consistentResults "Consistenty over discrete metric"                        discreteMetric
    , consistentResults "Consistenty over L1 norm"                                l1Norm
    , consistentResults "Consistenty over prefer substitution metric (1:2)"       preferSubMetric
    , consistentResults "Consistenty over prefer insertion/deletion metric (2:1)" preferGapMetric
    ]


consistentResults :: String -> (Int -> Int -> Word) -> TestTree
consistentResults label metric = SC.testProperty label $ SC.forAll checkConsistency
    where
        tcm = buildTransitionCostMatrix alphabet metric

        f :: SymbolAmbiguityGroup -> SymbolString
        f = fromNonEmpty . (:| []) . (\x -> Align x x x)

        checkConsistency :: (NucleotideBase, NucleotideBase) -> Either String String
        checkConsistency inputs@(NB x, NB y)
            | resultsMatch = Right $ show inputs
            | otherwise    = Left contextRendering
            where
                naiveResult      = alignNaively alphabet metric (f x) (f y)
                memoedResult     = alignNeedlemanWunsch alphabet tcm (f x) (f y)
                ukkonenResult    = alignUkkonen alphabet tcm (f x) (f y)

                resultsMatch     = all (naiveResult ==) [memoedResult, ukkonenResult]

                contextRendering = renderContexts tcm inputs contexts

                contexts =
                    [ ("Naive"            , naiveResult)
                    , ("Needleman-Wunsch" , memoedResult)
                    , ("Ukkonen (Unboxed)", ukkonenResult)
                    ]


renderContexts :: (Show b, Show c, Show d, Foldable t) => a -> b -> t (String, (c, d)) -> String
renderContexts _tcm inputs xs =
    let f :: (Show a, Show b) => (String, (a, b)) -> String
        f (s, c) = s <> "\n" <> renderResult c

        renderResult :: (Show a, Show b) => (a, b) -> String
        renderResult (cost, aligned) =
            unlines ["  Cost     : " <> show cost, "  Alignment: " <> show aligned]
    in  unlines . ([show inputs] <>) . fmap f $ toList xs


testSuiteNaiveDO :: TestTree
testSuiteNaiveDO = testGroup
    "Naive DO"
    [ isValidPairwiseAlignment "Naive DO over discrete metric"
        $ \x y -> toOtherReturnContext (alignNaively alphabet discreteMetric x y)
    , isValidPairwiseAlignment "Naive DO over L1 norm"
        $ \x y -> toOtherReturnContext (alignNaively alphabet l1Norm x y)
    , isValidPairwiseAlignment "Naive DO over prefer substitution metric (1:2)"
        $ \x y -> toOtherReturnContext (alignNaively alphabet preferSubMetric x y)
    , isValidPairwiseAlignment "Naive DO over prefer insertion/deletion metric (2:1)"
        $ \x y -> toOtherReturnContext (alignNaively alphabet preferGapMetric x y)
    ]


testSuiteNeedlemanWunsch :: TestTree
testSuiteNeedlemanWunsch = testGroup
    "Memoized DO"
    [ isValidPairwiseAlignment "Memoized DO over discrete metric" $ \x y -> toOtherReturnContext
        (alignNeedlemanWunsch alphabet (buildTransitionCostMatrix alphabet discreteMetric) x y)
    , isValidPairwiseAlignment "Memoized DO over L1 norm" $ \x y ->
        toOtherReturnContext (alignNeedlemanWunsch alphabet (buildTransitionCostMatrix alphabet l1Norm) x y)
    , isValidPairwiseAlignment "Memoized DO over prefer substitution metric (1:2)" $ \x y ->
        toOtherReturnContext
            (alignNeedlemanWunsch alphabet (buildTransitionCostMatrix alphabet preferSubMetric) x y)
    , isValidPairwiseAlignment "Memoized DO over prefer insertion/deletion metric (2:1)" $ \x y ->
        toOtherReturnContext
            (alignNeedlemanWunsch alphabet (buildTransitionCostMatrix alphabet preferGapMetric) x y)
    ]


testSuiteUkkonenDO :: TestTree
testSuiteUkkonenDO = testGroup
    "Unboxed Ukkonen DO"
    [ isValidPairwiseAlignment "Unboxed Ukkonen DO over discrete metric" $ \x y ->
        toOtherReturnContext (alignUkkonen alphabet (buildTransitionCostMatrix alphabet discreteMetric) x y)
    , isValidPairwiseAlignment "Unboxed Ukkonen DO over L1 norm" $ \x y ->
        toOtherReturnContext (alignUkkonen alphabet (buildTransitionCostMatrix alphabet l1Norm) x y)
    , isValidPairwiseAlignment "Unboxed Ukkonen DO over prefer substitution metric (1:2)" $ \x y ->
        toOtherReturnContext (alignUkkonen alphabet (buildTransitionCostMatrix alphabet preferSubMetric) x y)
    , isValidPairwiseAlignment "Unboxed Ukkonen DO over prefer insertion/deletion metric (2:1)" $ \x y ->
        toOtherReturnContext (alignUkkonen alphabet (buildTransitionCostMatrix alphabet preferGapMetric) x y)
    ]


isValidPairwiseAlignment :: String -> (SymbolString -> SymbolString -> ResultType) -> TestTree
isValidPairwiseAlignment label alignmentFunction = testGroup
    label
    [ testProperty "alignment function is commutative"    commutivity
    , testProperty "aligned results are all equal length" resultsAreEqualLength
    , testProperty "output length is ≥ input length"      greaterThanOrEqualToInputLength
    , testProperty
        "alignment length is ≤ sum of input lengths"
        totalAlignmentLengthLessThanOrEqualToSumOfLengths
    , testProperty "output alignments were not erroneously swapped"  outputsCorrespondToInputs
    , testProperty "output alignments were not erroneously reversed" outputsAreNotReversed
    , testProperty "output alignments only contain new gaps"         filterGapsEqualsInput
    , testProperty "ungapped output contains no gaps"                ungappedHasNogaps
    ]
    where
        commutivity :: (NucleotideSequence, NucleotideSequence) -> Property
        commutivity (NS lhs, NS rhs) = a === x .&&. b === z .&&. c === y
            where
                (_, _, a, b, c) = alignmentFunction lhs rhs
                (_, _, x, y, z) = alignmentFunction rhs lhs

        resultsAreEqualLength :: (NucleotideSequence, NucleotideSequence) -> Property
        resultsAreEqualLength (NS lhs, NS rhs) = length med === length lhs' .&&. length lhs' === length rhs'
            where (_, _, med, lhs', rhs') = alignmentFunction lhs rhs

        greaterThanOrEqualToInputLength :: (NucleotideSequence, NucleotideSequence) -> Bool
        greaterThanOrEqualToInputLength (NS lhs, NS rhs) =
            length lhs' >= length lhs && length rhs' >= length rhs
            where (_, _, _, lhs', rhs') = alignmentFunction lhs rhs

        totalAlignmentLengthLessThanOrEqualToSumOfLengths
            :: (NucleotideSequence, NucleotideSequence) -> Property
        totalAlignmentLengthLessThanOrEqualToSumOfLengths (NS lhs, NS rhs) =
            counterexample shownCounterexample $ medLen <= lhsLen + rhsLen
            where
                (_, _, med, _, _)   = alignmentFunction lhs rhs
                medLen              = length med
                lhsLen              = length lhs
                rhsLen              = length rhs
                shownCounterexample = unwords [show medLen, ">", show lhsLen, "+", show rhsLen]

        outputsCorrespondToInputs :: (NucleotideSequence, NucleotideSequence) -> Property
        outputsCorrespondToInputs (NS lhs, NS rhs) =
            medianList lhs
                /=   medianList rhs
                ==>  counterexample counterExample1 (filterGaps lhs' /= filterGaps (medianList rhs))
                .&&. counterexample counterExample2 (filterGaps rhs' /= filterGaps (medianList lhs))
            where
                counterExample1 = unlines
                    [ "lhs' === rhs !?!?"
                    , "filterGaps lhs' /= filterGaps (medianList rhs)"
                    , "lhs  " <> show lhs
                    , "lhs' " <> show lhs' <> " ~/~ " <> show (filterGaps lhs')
                    , "rhs  " <> show rhs <> " ~/~ " <> show (filterGaps (medianList rhs))
                    , "rhs' " <> show rhs'
                    ]
                counterExample2 = unlines
                    [ "rhs' === lhs !?!?"
                    , "filterGaps rhs' /= filterGaps (medianList lhs)"
                    , "rhs  " <> show rhs
                    , "rhs' " <> show rhs' <> " ~/~ " <> show (filterGaps rhs')
                    , "lhs  " <> show lhs <> " ~/~ " <> show (filterGaps (medianList lhs))
                    , "lhs' " <> show lhs'
                    ]
                (_, _, _, lhs', rhs') = alignmentFunction lhs rhs

        outputsAreNotReversed :: (NucleotideSequence, NucleotideSequence) -> Property
        outputsAreNotReversed (NS lhs, NS rhs) =
            counterexample
                    (show lhs <> show lhs')
                    (isNotPalindrome (medianList lhs) ==> isNotReversed (filterGaps lhs') (medianList lhs))
                .&&. counterexample
                        (show rhs <> show rhs')
                        (isNotPalindrome (medianList rhs) ==> isNotReversed (filterGaps rhs') (medianList rhs)
                        )
            where
                (_, _, _, lhs', rhs') = alignmentFunction lhs rhs

                isNotReversed :: [SymbolAmbiguityGroup] -> [SymbolAmbiguityGroup] -> Bool
                isNotReversed x y = reverse (toList x) /= toList y

                isNotPalindrome :: [SymbolAmbiguityGroup] -> Bool
                isNotPalindrome x = isNotReversed x x

        filterGapsEqualsInput :: (NucleotideSequence, NucleotideSequence) -> Property
        filterGapsEqualsInput (NS lhs, NS rhs) =
            counterexample context
                $    filterGaps lhs'
                ===  filterGaps (medianList lhs)
                .&&. filterGaps rhs'
                ===  filterGaps (medianList rhs)
            where
                context = unlines
                    [ "lhs' = " <> show lhs'
                    , "lhs  = " <> show lhs
                    , "rhs' = " <> show rhs'
                    , "rhs  = " <> show rhs
                    , unwords
                        [ "filterGaps lhs' === filterGaps (medianList lhs) ="
                        , show (filterGaps lhs')
                        , "==="
                        , show (filterGaps (medianList lhs))
                        ]
                    , unwords
                        [ "filterGaps rhs' === filterGaps (medianList rhs) ="
                        , show (filterGaps rhs')
                        , "==="
                        , show (filterGaps (medianList rhs))
                        ]
                    ]
                (_, _, _, lhs', rhs') = alignmentFunction lhs rhs

        ungappedHasNogaps :: (NucleotideSequence, NucleotideSequence) -> Property
        ungappedHasNogaps (NS lhs, NS rhs) = filterGaps unGap === unGap
            where (_, unGap, _, _, _) = alignmentFunction lhs rhs


medianList :: SymbolString -> [SymbolAmbiguityGroup]
medianList = fmap symbolAlignmentMedian . toList


alphabet :: Alphabet String
alphabet = fromSymbols ["A", "C", "G", "T"]


gap :: SymbolAmbiguityGroup
gap = encodeAmbiguityGroup alphabet . (:| []) $ gapSymbol alphabet


discreteMetric :: (Num b, Ord a) => a -> a -> b
discreteMetric i j = if i /= j then 1 else 0


l1Norm :: Int -> Int -> Word
l1Norm i j = toEnum $ max i j - min i j


preferGapMetric :: (Num a, Num b, Ord a) => a -> a -> b
preferGapMetric i j
    | i == j    = 0
    | i == 4    = 1
    | j == 4    = 1
    | otherwise = 2


preferSubMetric :: (Num a, Num b, Ord a) => a -> a -> b
preferSubMetric i j
    | i == j    = 0
    | i == 4    = 2
    | j == 4    = 2
    | otherwise = 1
