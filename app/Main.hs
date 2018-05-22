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
import           Data.Pointed
import           Data.Semigroup               ((<>))
import           Data.Semigroup.Foldable
import           Data.Set                     (Set)
import           Data.SymbolString
import           Data.TCM
import           Data.Validation
import           FileInput
import           Options.Applicative
import           UserInput
import           System.IO

import SampleData


main :: IO ()
main = runInput2


runTests :: IO ()
runTests = do
    hSetBuffering stdout NoBuffering
    let maxWidth = maximum $ (\(x,_,_,_) -> length x) <$> sampleDataSets
    mapWithKeyM_ (runAndReportDataSet maxWidth) sampleDataSets
--    runAndReportDataSet maxWidth 0 $ sampleDataSets ! 7


runInput :: IO ()
runInput = do
    opts <- force <$> parseUserInput
    vals <- parseFileInput opts
    case vals of
      Left  errs -> putStrLn errs
      Right (alphabet, tcm, tree) ->
        let maxLabelLen = succ . maximum $ foldMapWithKey (\k _ -> [length k]) tree
            inputRenderer x i = mconcat [ pad maxLabelLen (i<>":"), " ", renderSingleton defaultAlphabet $ x ^. preliminaryString ]
            leafRenderer  x i = mconcat [ pad maxLabelLen (i<>":"), " ", renderSingleton defaultAlphabet $ x ^. alignedString     ]
            nodeRenderer  x _ = mconcat [ pad maxLabelLen     "?:", " ", renderSingleton defaultAlphabet $ x ^. alignedString     ]

            postorder'  = postorder stringAligner
            preorder'   = preorder preorderRootLogic medianStateFinalizer preorderLeafLogic
            medianStateFinalizer = preorderInternalLogic (buildThreeWayCompare defaultAlphabet tcm)
            stringAligner = postorderLogic (ukkonenDO defaultAlphabet tcm)
        in  do
          putStrLn ""
          putStrLn $ renderAlphabet alphabet
          putStrLn ""
          putStrLn "Input Strings:"
          putStrLn ""
          putStrLn $ renderPhylogeny inputRenderer tree
          putStrLn ""
          let postResult    = force $ postorder' tree
              alignmentCost = getNodeDatum postResult ^. subtreeCost
          putStrLn "Post-order complete"
          putStrLn ""
          putStrLn $ "Alignment Cost: " <> show alignmentCost
          let preResult  = force $ preorder' postResult
          putStrLn ""
          putStrLn "Pre-order complete"
          putStrLn ""
          putStrLn "Output Alignment:"
          putStrLn ""
--          putStrLn $ renderPhylogeny inputRenderer postResult
          putStrLn $ renderAlignment nodeRenderer leafRenderer preResult


runInput2 :: IO ()
runInput2 = do
    hSetBuffering stdout NoBuffering
    opts <- force <$> parseUserInput
    vals <- parseFileInput opts
    case vals of
      Left  errs -> putStrLn errs
      Right (alphabet, tcm, tree) ->
        let maxLabelLen = succ . maximum $ foldMapWithKey (\k _ -> [length k]) tree
            inputRenderer  x i = mconcat [ pad maxLabelLen (i<> ":"), " ", renderSingleton alphabet $ x ^. preliminaryString ]
            prelimRenderer x i = mconcat [ pad maxLabelLen     "?:" , " ", renderSingleton alphabet $ x ^. preliminaryString ]
            leafRenderer   x i = mconcat [ pad maxLabelLen (i<> ":"), " ", renderSingleton alphabet $ x ^. alignedString     ]
            nodeRenderer   x _ = mconcat [ pad maxLabelLen     "?:" , " ", renderSingleton alphabet $ x ^. alignedString     ]
            postorder'    = postorder stringAligner
            preorder'     = preorder preorderRootLogic medianStateFinalizer preorderLeafLogic
            medianStateFinalizer = preorderInternalLogic (buildThreeWayCompare defaultAlphabet tcm)
            stringAligner = postorderLogic (ukkonenDO alphabet tcm)
        in  do
          putStrLn ""
          putStrLn $ renderAlphabet alphabet
          putStrLn ""
          putStrLn "Input Strings:"
          putStrLn ""
          putStrLn $ renderPhylogeny inputRenderer tree
          putStrLn ""
          let postResult    = force $ postorder' tree
              alignmentCost = getNodeDatum postResult ^. subtreeCost
          putStrLn "Post-order complete"
          putStrLn ""
          putStrLn $ renderAlignment prelimRenderer inputRenderer postResult
          putStrLn ""
          putStrLn $ "Alignment Cost: " <> show alignmentCost
          let preResult  = force $ preorder' postResult
          putStrLn ""
          putStrLn "Pre-order complete"
          putStrLn ""
          putStrLn "Output Alignment:"
          putStrLn ""
          putStrLn $ renderPhylogeny leafRenderer preResult
          putStrLn ""
          putStrLn $ renderAlignment nodeRenderer leafRenderer preResult


runAndReportDataSet :: Int -> Int -> (String, LeafInput, TreeInput, TransitionCostMatrix Char) -> IO ()
runAndReportDataSet width num (dataSetLabel, leafData, treeData, op) = do
--    parseUserInput >>= print
    let dataSetNumber = "Data Set Number: " <> show num
    let width'        = max width $ length dataSetNumber
    putStrLn $ mconcat [ "-=-=-=-=-=- ", centerWithin width' dataSetNumber, " -=-=-=-=-=-" ]    
    putStrLn $ mconcat [ "-=-=-=-=-=- ", centerWithin width' dataSetLabel , " -=-=-=-=-=-" ]
    putStrLn ""
    case toEither $ unifyInput leafData treeData of
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
{--          
          putStrLn $ renderAlignment nodeRendererB leafRendererB result
          putStrLn ""
          putStrLn $ renderAlignment nodeRendererC leafRendererC result
          putStrLn ""
--}
  where
    centerWithin width x = mconcat
        [ replicate pad ' '
        , x
        , replicate pad ' '
        , if extra == 1 then " " else ""
        ]
      where
        (pad, extra) = (width - length x) `quotRem` 2
    
    postorder' = postorder stringAligner
    preorder'  = preorder preorderRootLogic medianStateFinalizer preorderLeafLogic

    medianStateFinalizer = preorderInternalLogic (buildThreeWayCompare defaultAlphabet op)
    
    stringAligner = postorderLogic (ukkonenDO defaultAlphabet op)

    inputRenderer x i = mconcat
        [ i
        , ": "
        , renderSingleton defaultAlphabet $ x ^. preliminaryString
        ]
    
    leafRendererA x i = mconcat
        [ i
        , ": "
--        , renderSymbolString defaultAlphabet $ x ^. preliminaryString
--        , renderString       defaultAlphabet $ x ^.   finalizedString
--        , renderSymbolString defaultAlphabet $ x ^. alignedString
--        , renderAligns defaultAlphabet $ x ^. alignedString
        , renderSingleton defaultAlphabet $ x ^. alignedString
        ]

    nodeRendererA x _ = mconcat
        [ "?: "
--        , renderSymbolString defaultAlphabet $ x ^. preliminaryString
--        , renderString       defaultAlphabet $ x ^.   finalizedString
--        , renderSymbolString defaultAlphabet $ x ^. alignedString
--        , renderAligns defaultAlphabet $ x ^. alignedString
        , renderSingleton defaultAlphabet $ x ^. alignedString
        ]

    leafRendererB x i = mconcat
        [ i
        , ": "
--        , renderSymbolString defaultAlphabet $ x ^. preliminaryString
--        , renderString       defaultAlphabet $ x ^.   finalizedString
        , renderSymbolString defaultAlphabet $ x ^. alignedString
--        , renderAligns defaultAlphabet $ x ^. alignedString
--        , renderSingleton defaultAlphabet $ x ^. alignedString
        ]

    nodeRendererB x _ = mconcat
        [ "?: "
--        , renderSymbolString defaultAlphabet $ x ^. preliminaryString
--        , renderString       defaultAlphabet $ x ^.   finalizedString
        , renderSymbolString defaultAlphabet $ x ^. alignedString
--        , renderAligns defaultAlphabet $ x ^. alignedString
--        , renderSingleton defaultAlphabet $ x ^. alignedString
        ]

    leafRendererC x i = mconcat
        [ i
        , ": "
        , renderSymbolString defaultAlphabet $ x ^. preliminaryString
--        , renderString       defaultAlphabet $ x ^.   finalizedString
--        , renderSymbolString defaultAlphabet $ x ^. alignedString
--        , renderAligns defaultAlphabet $ x ^. alignedString
--        , renderSingleton defaultAlphabet $ x ^. alignedString
        ]

    nodeRendererC x _ = mconcat
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
