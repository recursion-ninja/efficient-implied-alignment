{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module TimingParameters
  ( TimingParameters(..)
  , validateTimingParameters
  ) where

import Control.DeepSeq
import Data.Maybe
import System.Exit


data  TimingParameters
    = TimingParameters
    { dataFile      :: FilePath
    , treeFile      :: FilePath
    , tcmFile       :: FilePath
    , leafSetSizes  :: [Word]
    , stringLengths :: [Rational]
    , noGenerate    :: Bool
    } deriving (Show)


instance NFData TimingParameters where

    rnf (TimingParameters a b c d e f) =
      rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f


validateTimingParameters :: TimingParameters -> IO TimingParameters
validateTimingParameters tp =
    case errors of
      []  -> pure tp
      [x] -> die (x <> "\n")
      xs  -> die . unlines $ "Encountered the following errors: ":xs
  where
    errors = catMaybes
      [ dataFileEmpty
      , treeFileEmpty
      ,  tcmFileEmpty
      , leafSetSizesEmpty
      , stringLengthsEmpty
      , stringLengthsOutOfRange
      ]
      
    dataFileEmpty
      | null $ dataFile tp = Just "The data file path was empty"
      | otherwise          = Nothing

    treeFileEmpty
      | null $ treeFile tp = Just "The tree file path was empty"
      | otherwise          = Nothing

    tcmFileEmpty
      | null $ tcmFile tp  = Just "The tcm file path was empty"
      | otherwise          = Nothing

    leafSetSizesEmpty
      | null $ leafSetSizes tp = Just "The list of leaf set sizes was empty"
      | otherwise              = Nothing

    stringLengthsEmpty
      | null $ stringLengths tp = Just "The list of string length fractions sizes was empty"
      | otherwise               = Nothing

    stringLengthsOutOfRange =
        case filter f $ stringLengths tp of
          [] -> Nothing
          xs  -> Just $ unlines
              [ "One or more rationals were outside the rang (0, 1]"
              , "  " <> show xs
              ]
      where
        f x = x > 1 || x <= 0
