
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module File.Format.Fasta.Test
  ( testSuite
  ) where

import Control.Arrow            (first,second)
import Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty as NE (fromList)
import Data.Vector              (Vector,fromList)
import File.Format.Fasta.Test   (validTaxonLines)
import File.Format.Fasta.Parser
import Test.Custom.Parse        (parseEquals)
import Test.Tasty               (TestTree,testGroup)
import Test.Tasty.HUnit


testSuite :: TestTree
testSuite = testGroup "Fasta Format"
    [ testGroup "Fasta Parser"
        [fastaSymbolSequence',fastaTaxonSequenceDefinition',fastaStreamParser']
    ]


fastaSymbolSequence' :: TestTree
fastaSymbolSequence' = testGroup "fastaSymbolSequence" [valid]
  where
    f (res,str) = testCase (show str) $ parseEquals fastaSymbolSequence str res
    valid       = testGroup "Valid sequences" $ f <$> validSequences


validSequences :: [(Vector (NonEmpty String), String)]
validSequences = first (fromList . fmap NE.fromList) <$>
    [ ([["wow"]]                                           , "wow\n"                         )
    , ([["wow"],["such"]]                                  , "wow such\n"                    )
    , ([["wow"],["such"],["very"]]                         , " wow such very \n"             )
    , ([["wow"],["such","very","much"],["success"]]        , "wow such | very | much success\n")
    , ([["wow"],["such","very"],["success"],["many","much"]
       ,["compile"],["so","amaze"],["parse"],["wow"]]      , "wow such|very success many|much compile so|amaze parse wow\n")
    ]


fastaTaxonSequenceDefinition' :: TestTree
fastaTaxonSequenceDefinition' = testGroup "fastaTaxonSequenceDefinition" [valid]
  where
    f (res,str) = testCase (show str) $ parseEquals fastaTaxonSequenceDefinition str res
    valid               = testGroup "Valid sequences" $ f <$> validTaxonSequences


validTaxonSequences :: [(FastaSequence,String)]
validTaxonSequences = zipWith f validTaxonLines validSequences
  where
    f (x,str) (y,seq')  = (FastaSequence x y, concat [str,"\n",seq'])


fastaStreamParser' :: TestTree
fastaStreamParser' = testGroup "fastaStreamParser" [testGroup "Valid stream" [validStream]]
  where
    validStream = testCase "Fasta concatenated stream" $ parseEquals fastaStreamParser str (NE.fromList res)
    (res,str)   = second concat $ unzip validTaxonSequences

