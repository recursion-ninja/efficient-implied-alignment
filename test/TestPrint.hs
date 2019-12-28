{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Main where

import           Alignment
import           Control.DeepSeq
import           Control.Lens
import           Data.Alphabet
import           Data.BTree
import           Data.Char
import           Data.Decoration
import           Data.Foldable
import           Data.Functor            (($>))
import           Data.Key
import           Data.List.NonEmpty      (NonEmpty (..), intersperse)
import qualified Data.List.NonEmpty      as NE
import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Matrix.ZeroIndexed (matrix)
import           Data.Pointed
import           Data.Semigroup          ((<>))
import           Data.Semigroup.Foldable
import           Data.Set                (Set)
import           Data.SymbolString
import           Data.TCM
import           Data.Validation
import           File.Input
import           SampleData
import           System.IO


main :: IO ()
main = runTests


runTests :: IO ()
runTests = do
    hSetBuffering stdout NoBuffering
    let maxWidth = maximum $ (\(x,_,_,_) -> length x) <$> sampleDataSets
    mapWithKeyM_ (runAndReportDataSet maxWidth) sampleDataSets
--    runAndReportDataSet maxWidth 0 $ sampleDataSets ! 7


runAndReportDataSet :: Int -> Int -> (String, StringValues, TreeInput, TransitionCostMatrix) -> IO ()
runAndReportDataSet width num (dataSetLabel, leafData, treeData, op) = do
    let dataSetNumber = "Data Set Number: " <> show num
    let width'        = max width $ length dataSetNumber
    putStrLn $ fold [ "-=-=-=-=-=- ", centerWithin width' dataSetNumber, " -=-=-=-=-=-" ]
    putStrLn $ fold [ "-=-=-=-=-=- ", centerWithin width' dataSetLabel , " -=-=-=-=-=-" ]
    putStrLn ""
    case toEither $ unifyInput defaultAlphabet (fst <$> leafData) treeData of
      Left  errors -> mapM_ print $ toList errors
      Right tree   ->  do
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
{--}
          putStrLn $ renderAlignment nodeRendererB leafRendererB result
          putStrLn ""
          putStrLn $ renderAlignment nodeRendererC leafRendererC result
          putStrLn ""
{--}
  where
    centerWithin width x = fold
        [ replicate pad ' '
        , x
        , replicate pad ' '
        , if extra == 1 then " " else ""
        ]
      where
        (pad, extra) = (width - length x) `quotRem` 2

    postorder' = postorder stringAligner
    preorder'  = preorder preorderRootLogic medianStateFinalizer preorderLeafLogic

    medianStateFinalizer = preorderInternalLogic -- (buildThreeWayCompare defaultAlphabet op)

    stringAligner = postorderLogic (ukkonenDO defaultAlphabet op)

    inputRenderer x i = fold
        [ i
        , ": "
        , renderSingleton defaultAlphabet $ x ^. preliminaryString
        ]

    leafRendererA x i = fold
        [ i
        , ": "
--        , renderSymbolString defaultAlphabet $ x ^. preliminaryString
--        , renderString       defaultAlphabet $ x ^.   finalizedString
--        , renderSymbolString defaultAlphabet $ x ^. alignedString
--        , renderAligns defaultAlphabet $ x ^. alignedString
        , renderSingleton defaultAlphabet $ x ^. alignedString
        ]

    nodeRendererA x _ = fold
        [ "?: "
--        , renderSymbolString defaultAlphabet $ x ^. preliminaryString
--        , renderString       defaultAlphabet $ x ^.   finalizedString
--        , renderSymbolString defaultAlphabet $ x ^. alignedString
--        , renderAligns defaultAlphabet $ x ^. alignedString
        , renderSingleton defaultAlphabet $ x ^. alignedString
        ]

    leafRendererB x i = fold
        [ i
        , ": "
--        , renderSymbolString defaultAlphabet $ x ^. preliminaryString
--        , renderString       defaultAlphabet $ x ^.   finalizedString
        , renderSymbolString defaultAlphabet $ x ^. alignedString
--        , renderAligns defaultAlphabet $ x ^. alignedString
--        , renderSingleton defaultAlphabet $ x ^. alignedString
        ]

    nodeRendererB x _ = fold
        [ "?: "
--        , renderSymbolString defaultAlphabet $ x ^. preliminaryString
--        , renderString       defaultAlphabet $ x ^.   finalizedString
        , renderSymbolString defaultAlphabet $ x ^. alignedString
--        , renderAligns defaultAlphabet $ x ^. alignedString
--        , renderSingleton defaultAlphabet $ x ^. alignedString
        ]

    leafRendererC x i = fold
        [ i
        , ": "
        , renderSymbolString defaultAlphabet $ x ^. preliminaryString
--        , renderString       defaultAlphabet $ x ^.   finalizedString
--        , renderSymbolString defaultAlphabet $ x ^. alignedString
--        , renderAligns defaultAlphabet $ x ^. alignedString
--        , renderSingleton defaultAlphabet $ x ^. alignedString
        ]

    nodeRendererC x _ = fold
        [ "?: "
        , renderSymbolString defaultAlphabet $ x ^. preliminaryString
--        , renderString       defaultAlphabet $ x ^.   finalizedString
--        , renderSymbolString defaultAlphabet $ x ^. alignedString
--        , renderAligns defaultAlphabet $ x ^. alignedString
--        , renderSingleton defaultAlphabet $ x ^. alignedString
        ]


pad :: Int -> String -> String
pad i str = str <> replicate (i - length str) ' '


renderAlphabet :: Alphabet Char -> String
renderAlphabet = (\x -> "Alphabet: { "<>x<>" }") . fold1 . intersperse ", " . fmap pure . toNonEmpty
