{-# Language FlexibleContexts #-}
{-# Language ImportQualifiedPost #-}
{-# Language TypeFamilies #-}

module Main
    ( main
    , testTrees
    ) where

import Alignment
import Alignment.Pairwise (alignUkkonen)
import Control.DeepSeq
import Control.Lens ((^.))
import Data.BTree
import Data.Char
import Data.Decoration
import Data.Foldable
import Data.Key
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Ord
import Data.SymbolString
import Data.TCM (TransitionCostMatrix)
import Data.Text (Text, unpack)
import Data.Validation
import Data.Vector.Unboxed.NonEmpty (Vector)
import File.Input
import Prelude hiding (zip)
import SampleData
import Test.Tasty
import Test.Tasty.HUnit


stringAligner :: TransitionCostMatrix -> PreliminaryNode -> PreliminaryNode -> PreliminaryNode
stringAligner tcm = postorderLogic $ alignUkkonen defaultAlphabet tcm


main :: IO ()
main = testTrees


testTrees :: IO ()
testTrees = defaultMain . testGroup "Test Trees" $ runTest <$> sampleDataSets


runTest
    :: ( TestName
       , Map Text (NonEmpty (Vector Char), NonEmpty (NonEmpty (Vector Char)))
       , BTree b a
       , SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> (SymbolAmbiguityGroup, Word)
       )
    -> TestTree
runTest (dataSetLabel, leafData, treeData, op) = testCase dataSetLabel $ do
    inputTree' <- case toEither $ unifyInput defaultAlphabet (fst <$> leafData) treeData of
        Left  errors -> assertFailure . unlines $ show <$> toList errors
        Right tree   -> pure tree
    outputTrees <- case toEither $ gatherOutputTrees leafData treeData of
        Left  errors -> assertFailure . unlines $ show <$> toList errors
        Right tree   -> pure tree
    let result         = force . preorder' $ postorder' inputTree'
        alignedString' = renderPhylogeny leafRendererA result
        inputString'   = renderPhylogeny inputRenderer inputTree'
        outputStrings  = renderPhylogeny inputRenderer <$> outputTrees
        closestOutput  = minimumBy (comparing (strDistance alignedString')) outputStrings
        errorMsg       = unlines
            [ "Input Tree:"
            , inputString'
            , "Actual Aligned Tree:"
            , alignedString'
            , "Expected Aligned Tree:"
            , closestOutput
            ]
    assertBool errorMsg (alignedString' `elem` outputStrings)

    where
        postorder'           = postorder $ stringAligner op
        preorder'            = preorder preorderRootLogic medianStateFinalizer preorderLeafLogic

        medianStateFinalizer = preorderInternalLogic

        gatherOutputTrees
            :: Map Text (a, NonEmpty (NonEmpty (Vector Char)))
            -> BTree b c
            -> Validation (NonEmpty UnificationError) (NonEmpty (BTree b PreliminaryNode))
        gatherOutputTrees x y = traverse (\i -> f $ (! i) . snd <$> x) $ 0 :| [1 .. count - 1]
            where
                count = length . head . toList $ snd <$> x
                f z = unifyInput defaultAlphabet z y

        leafRendererA :: FinalizedNode -> Text -> String
        leafRendererA x i = fold [unpack i, ": ", renderSingleton defaultAlphabet $ x ^. alignedString]

        inputRenderer :: PreliminaryNode -> Text -> String
        inputRenderer x i = fold [unpack i, ": ", renderSingleton defaultAlphabet $ x ^. preliminaryString]

        strDistance :: Eq a => [a] -> [a] -> Int
        strDistance x y = length . filter (uncurry (/=)) $ zip x y
