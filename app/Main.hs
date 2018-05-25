{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Main where

import Alignment
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Data.Alphabet
import Data.BTree
import Data.Decoration
import Data.Key
import Data.List.NonEmpty      (intersperse)
import Data.Semigroup          ((<>))
import Data.Semigroup.Foldable
import Data.SymbolString
import File.Input
import File.Output
import InputParser
import System.IO
import System.Timing


main :: IO ()
main = runInput


runInput :: IO ()
runInput = do
    hSetBuffering stdout NoBuffering
    opts <- force <$> parseUserInput
    vals <- parseFileInput opts
    case vals of
      Left  errs -> putStrLn errs
      Right fileInput -> 
        let alphabet = inputAlphabet fileInput
            tcm      = inputTCM      fileInput
            tree     = inputTree     fileInput

            maxLabelLen        = succ . maximum $ foldMapWithKey (\k _ -> [length k]) tree
            inputRenderer  x i = mconcat [ padR maxLabelLen (i<> ":"), " ", renderSingleton alphabet $ x ^. preliminaryString ]
--            prelimRenderer x _ = mconcat [ padR maxLabelLen     "?:" , " ", renderSingleton alphabet $ x ^. preliminaryString ]
            leafRenderer   x i = mconcat [ padR maxLabelLen (i<> ":"), " ", renderSingleton alphabet $ x ^. alignedString     ]
            nodeRenderer   x _ = mconcat [ padR maxLabelLen     "?:" , " ", renderSingleton alphabet $ x ^. alignedString     ]

            postorder'    = postorder stringAligner
            preorder'     = preorder preorderRootLogic preorderInternalLogic preorderLeafLogic
            stringAligner = postorderLogic (ukkonenDO alphabet tcm)
        in  do
          when (verbose opts) $ mapM_ putStrLn
              [ ""
              , renderAlphabet alphabet
              , ""
              , "Input Strings:"
              , renderPhylogeny inputRenderer tree
              , ""
              ]

          (postorderTime, postorderResult) <- timeOp $ do
                                                  let x = force $ postorder' tree
                                                  pure x

          let alignmentCost = getNodeDatum postorderResult ^. subtreeCost

          ( preorderTime,  preorderResult) <- timeOp $ do
                                                  let x = force $ preorder' postorderResult
                                                  pure x

          when (verbose opts) $ do
              let shownParseTime  = show $ parseTime      fileInput
                  shownUnifyTime  = show $ unifyTime      fileInput
                  shownPreCompute = show $ precomputeTime fileInput
                  shownPostorder  = show postorderTime
                  shownPreorder   = show preorderTime
                  dPad = maximum $ length <$>
                      [ shownParseTime
                      , shownUnifyTime
                      , shownPreCompute
                      , shownPostorder
                      , shownPreorder
                      ]

              mapM_ putStrLn
                [ "Output Alignment:"
                , ""
--                , renderPhylogeny inputRenderer postResult
                , renderAlignment nodeRenderer leafRenderer preorderResult
                , ""
                , "Alignment Cost: " <> show alignmentCost
                , ""
                , "Diagnostics:"
                , "  Parse Files: " <> padL dPad shownParseTime
                , "  Unify Input: " <> padL dPad shownUnifyTime
                , "  Setup TCM:   " <> padL dPad shownPreCompute
                , "  Postorder:   " <> padL dPad shownPostorder
                , "  Preorder:    " <> padL dPad shownPreorder
                ]
          writeFastaFile alphabet preorderResult $ outputFile opts 


padL :: Int -> String -> String
padL i str = replicate (i - length str) ' ' <> str

padR :: Int -> String -> String
padR i str = str <> replicate (i - length str) ' '


renderAlphabet :: Alphabet Char -> String
renderAlphabet = (\x -> "Alphabet: { "<>x<>" }") . fold1 . intersperse ", " . fmap pure . toNonEmpty
