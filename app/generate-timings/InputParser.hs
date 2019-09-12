{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module InputParser
  ( TimingParameters(..)
  , parseTimingParameters
  ) where

import Data.Char
import Data.Foldable
import Data.Semigroup               ((<>))
import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen (string)
import TimingParameters


parseTimingParameters :: IO TimingParameters
parseTimingParameters = customExecParser preferences $ info (helper <*> timingParameters) description
  where
    timingParameters =
        TimingParameters
          <$> fileSpec 'd' "data"        "FASTA data file"
          <*> fileSpec 't' "tree"        "Newick tree file"
          <*> fileSpec 'm' "tcm"         "Transition Cost Matrix file with symbol alphabet"
          <*> fileSpec 'o' "output"      "File path prefix of CSV prorder and postorder timings"
          <*>  argSpec 'n' "leaves"      "Leaf set lengths"
          <*>  argSpec 'k' "lengths"     "String lengths in range [0, 1]"
          <*> switch  (fold [long "no-generate", help "Do NOT generate files, assume they exist"])


    fileSpec c s h = strOption   $ fold [short c, long s, help h, metavar (toUpper <$> s <> "FILE")]
    argSpec  c s h = option auto $ fold [short c, long s, help h]

    description = mconcat
        [ fullDesc
        , headerDoc . Just $ string "\n  Generate timing results for tree-align"
        , footerDoc $ Just mempty
        ]

    preferences = prefs $ mconcat [showHelpOnError, showHelpOnEmpty]
