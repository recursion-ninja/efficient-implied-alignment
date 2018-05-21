{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Main where

import Alignment
import Control.DeepSeq
import Control.Lens
import Data.Alphabet
import Data.BTree
import Data.Decoration
import Data.Key
import Data.List.NonEmpty      (intersperse)
import Data.Semigroup          ((<>))
import Data.Semigroup.Foldable
import Data.SymbolString
import File.Input
import InputParser
import System.IO


main :: IO ()
main = runInput


runInput :: IO ()
runInput = do
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
            preorder'     = preorder preorderRootLogic preorderInternalLogic preorderLeafLogic
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


pad :: Int -> String -> String
pad i str = str <> replicate (i - length str) ' '


renderAlphabet :: Alphabet Char -> String
renderAlphabet = (\x -> "Alphabet: { "<>x<>" }") . fold1 . intersperse ", " . fmap pure . toNonEmpty
