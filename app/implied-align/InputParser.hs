{-# Language FlexibleContexts #-}
{-# Language Safe #-}
{-# Language TypeFamilies #-}

module InputParser
    ( UserInput (..)
    , parseUserInput
    ) where

import Data.Char
import Data.Foldable
import Data.String (IsString)
import Data.UserInput
import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen (string)


parseUserInput :: IO UserInput
parseUserInput = customExecParser preferences $ info (helper <*> userInput) description
    where
        userInput =
            UserInput
                <$> argSpec 'd' "data"   "FASTA data file"
                <*> argSpec 't' "tree"   "Newick tree file"
                <*> argSpec 'm' "tcm"    "Transition Cost Matrix file with symbol alphabet"
                <*> argSpec 'o' "output" "Output file for alignments"
                <*> switch (fold [short 'v', long "verbose", help "Display alignment & timing information"])
                <*> switch (fold [long "timing", help "Display timing information"])
                <*> (   flag' DNA (fold [long "dna", help "Use DNA IUPAC codes for file input & output"])
                    <|> flag
                            Standard
                            RNA
                            (fold [long "rna", help "Use RNA IUPAC codes for file input & output"])
                    )

        argSpec :: IsString s => Char -> String -> String -> Parser s
        argSpec c s h = strOption $ fold [short c, long s, help h, metavar (toUpper <$> s <> "FILE")]

        description :: InfoMod a
        description = fold
            [ fullDesc
            , headerDoc . Just $ string "\n  Tree-based multiple string alignment program"
            , footerDoc $ Just mempty
            ]

        preferences = prefs $ fold [showHelpOnError, showHelpOnEmpty]
