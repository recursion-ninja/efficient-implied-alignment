{-# Language FlexibleContexts #-}
{-# Language ImportQualifiedPost #-}
{-# Language LambdaCase #-}
{-# Language TypeFamilies #-}

module Main
    ( main
    , testPrint
    ) where

import Alignment
import Alignment.Pairwise (alignUkkonen)
import Control.DeepSeq
import Control.Lens (Getting, (^.))
import Data.BTree
import Data.Decoration
import Data.Foldable
import Data.Key
import Data.SymbolString
import Data.TCM
import Data.Text (Text, unpack)
import Data.Validation
import File.Input
import SampleData
import System.IO


main :: IO ()
main = testPrint


testPrint :: IO ()
testPrint = do
    hSetBuffering stdout NoBuffering
    let maxWidth = maximum $ (\(x, _, _, _) -> length x) <$> sampleDataSets
    mapWithKeyM_ (runAndReportDataSet maxWidth) sampleDataSets


runAndReportDataSet :: Int -> Int -> (String, StringValues, TreeInput, TransitionCostMatrix) -> IO ()
runAndReportDataSet width num (dataSetLabel, leafData, treeData, operation) =
    let centerWithin spanArea x =
            let (spacing, extra) = (spanArea - length x) `quotRem` 2
                spanPadding      = replicate spacing ' '
                spanOffest       = case extra of
                    0 -> ""
                    _ -> " "
            in  fold [spanPadding, x, spanPadding, spanOffest]

        postorder'           = postorder stringAligner
        preorder'            = preorder preorderRootLogic medianStateFinalizer preorderLeafLogic
        medianStateFinalizer = preorderInternalLogic
        stringAligner        = postorderLogic (alignUkkonen defaultAlphabet operation)

        generalRender :: s -> Getting SymbolString s SymbolString -> Maybe Text -> String
        generalRender x s = \case
            Nothing -> fold ["", "?: ", renderSingleton defaultAlphabet $ x ^. s]
            Just i  -> fold [unpack i, ": ", renderSingleton defaultAlphabet $ x ^. s]

        inputRenderer :: PreliminaryNode -> Text -> String
        inputRenderer x = generalRender x preliminaryString . Just

        leafRendererA :: FinalizedNode -> Text -> String
        leafRendererA x = generalRender x alignedString . Just

        leafRendererB :: FinalizedNode -> Text -> String
        leafRendererB x = generalRender x alignedString . Just

        leafRendererC :: FinalizedNode -> Text -> String
        leafRendererC x = generalRender x preliminaryString . Just

        nodeRendererA :: FinalizedNode -> p -> String
        nodeRendererA x = const $ generalRender x alignedString Nothing

        nodeRendererB :: FinalizedNode -> p -> String
        nodeRendererB x = const $ generalRender x alignedString Nothing

        nodeRendererC :: FinalizedNode -> p -> String
        nodeRendererC x = const $ generalRender x preliminaryString Nothing

        preamble :: IO ()
        preamble =
            let dataSetNumber = "Data Set Number: " <> show num
                midWidth      = max width $ length dataSetNumber
                header x = fold ["-=-=-=-=-=- ", centerWithin midWidth x, " -=-=-=-=-=-"]
            in  traverse_ putStrLn [header dataSetNumber, header dataSetLabel, ""]

        reporting :: IO ()
        reporting = case toEither $ unifyInput defaultAlphabet (fst <$> leafData) treeData of
            Left  errors -> mapM_ print $ toList errors
            Right tree   -> do
                putStrLn ""
                print defaultAlphabet
                putStrLn ""
                putStrLn "Input Strings:"
                putStrLn ""
                putStrLn $ renderPhylogeny inputRenderer tree
                putStrLn ""
                putStrLn "Output Alignment:"
                putStrLn ""
                let result = force . preorder' $ postorder' tree
                putStrLn $ renderAlignment nodeRendererA leafRendererA result
                putStrLn ""
                putStrLn $ renderAlignment nodeRendererB leafRendererB result
                putStrLn ""
                putStrLn $ renderAlignment nodeRendererC leafRendererC result
                putStrLn ""
    in  preamble *> reporting
