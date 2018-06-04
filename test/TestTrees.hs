{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Main where

import           Alignment
import           Control.DeepSeq
import           Control.Lens
import           Data.Alphabet
import           Data.BTree
import           Data.Char
import           Data.Decoration
import           Data.Foldable
import           Data.Functor                 (($>))
import           Data.Key
import           Data.List.NonEmpty           (NonEmpty(..), intersperse)
import qualified Data.List.NonEmpty    as NE
import           Data.Matrix.ZeroIndexed      (matrix)
import           Data.Map                     (Map)
import qualified Data.Map              as M
import           Data.Ord
import           Data.Pointed
import           Data.Semigroup               ((<>))
import           Data.Semigroup.Foldable
import           Data.Set                     (Set)
import           Data.SymbolString
import           Data.TCM
import           Data.Validation
import           File.Input
import           Prelude               hiding (zip)
import           SampleData
import           System.IO
import           Test.Tasty
import           Test.Tasty.HUnit


main :: IO ()
main = runTests


runTests :: IO ()
runTests = defaultMain . testGroup "Test Trees" $ runTest <$> sampleDataSets


runTest (dataSetLabel, leafData, treeData, op) = testCase dataSetLabel $ do
    inputTree   <- case toEither $ unifyInput defaultAlphabet (fst <$> leafData) treeData of
                     Left  errors -> assertFailure . unlines $ show <$> toList errors
                     Right tree   -> pure tree
    outputTrees <- case toEither $ gatherOutputTrees leafData treeData of
                     Left  errors -> assertFailure . unlines $ show <$> toList errors
                     Right tree   -> pure tree
    let result = force . preorder' $ postorder' inputTree
        alignedString = renderPhylogeny leafRendererA result
        inputString   = renderPhylogeny inputRenderer inputTree
        outputStrings = renderPhylogeny inputRenderer <$> outputTrees
        closestOutput = minimumBy (comparing (strDistance alignedString)) outputStrings
        errorMsg      = unlines
            [ "Input Tree:"
            , inputString
            , "Actual Aligned Tree:"
            , alignedString
            , "Expected Aligned Tree:"
            , closestOutput
            ]
    assertBool errorMsg (any (== alignedString) outputStrings) 
    
  where
    postorder' = postorder stringAligner
    preorder'  = preorder preorderRootLogic medianStateFinalizer preorderLeafLogic

    medianStateFinalizer = preorderInternalLogic

    gatherOutputTrees x y = sequenceA $ (\i -> f $ ((! i) . snd) <$> x) <$> (0:|[1 .. count - 1])
      where
        count = length . head . toList $ snd <$> x
        f z = unifyInput defaultAlphabet z y
    
    stringAligner = postorderLogic (ukkonenDO defaultAlphabet op)
    leafRendererA x i = mconcat [ i, ": ", renderSingleton defaultAlphabet $ x ^. alignedString ]
    nodeRendererA x _ = mconcat [ "?: "  , renderSingleton defaultAlphabet $ x ^. alignedString ]
    inputRenderer x i = mconcat [ i, ": ", renderSingleton defaultAlphabet $ x ^. preliminaryString ]

    strDistance :: Eq a => [a] -> [a] -> Int
    strDistance x y = length . filter (uncurry (/=)) $ zip x y
