{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module InputParser
  ( UserInput(..)
  , parseUserInput
  ) where

import           Data.Char
import           Data.Foldable
import           Data.Semigroup               ((<>))
import           Data.UserInput
import           Options.Applicative
import           Text.PrettyPrint.ANSI.Leijen (string)


parseUserInput :: IO UserInput
parseUserInput = customExecParser preferences $ info (helper <*> userInput) description
  where
    userInput =
        UserInput
          <$> argSpec 'd' "data"   "FASTA data file"
          <*> argSpec 't' "tree"   "Newick tree file"
          <*> argSpec 'm' "tcm"    "Transition Cost Matrix file with symbol alphabet"
          <*> argSpec 'o' "output" "Output file for alignments"
          <*> switch  (fold [short 'v', long "verbose", help "Display alignment & timing informaion"])
          <*> switch  (fold [           long "timing" , help "Display timing informaion"])
          <*> ( flag'          DNA (fold [long "dna", help "Use DNA IUPAC codes for file input & output"]) <|>
                flag  Standard RNA (fold [long "rna", help "Use RNA IUPAC codes for file input & output"])
              )
--          <*> commandHelperDescriptions

    argSpec c s h = strOption $ fold [short c, long s, help h, metavar (toUpper <$> s <> "FILE")]

    description = fold
        [ fullDesc
        , headerDoc . Just $ string "\n  Tree-based multiple string alignment program"
        , footerDoc $ Just mempty
        ]

    preferences = prefs $ fold [showHelpOnError, showHelpOnEmpty]

{-
    commandHelperDescriptions = hsubparser $ fold
        [ command "DATAFILE" $ fileFormatHelp DataFileRequest
            [ "The DATAFILE provided should be in FASTC format."
            , "The FASTC file is similar to the FASTA file format in structure."
            , "There are one or more ordered pairs of leaf identifier lines and string definition lines."
            , "A leaf identifier line begins with a '>' symbol followed by an identifier for the leaf node on the same line."
            , "An identifier for a leaf node is defined as one or more characters (excluding '>') that are not separated by whitespace."
            , "The string definition line is the the next non-whitespace line."
            , "The string definition line contains one or more symbols and separated by in-line whitespace."
            , "A symbol is defined as one or more characters (excluding '>') that are not separated by whitespace."
            , "The leaf identifier line and the string definition line form a pair of leaf identifier and the corresponding string."
            , "Pairs of leaf identifier lines and string definition lines alternate to enumerate the leafset and link each leaf to it's corresponding data."
            , "Note the following:"
            , "Each leaf identifier in the file must be unique!"
            , "There is no way to represent the empty string! It is assumed that all strings are non-empty and finite."
            , "A leaf set may be created by collecting the union of leaf identifiers that occurs in the FASTC file. This union should be an exact macth of the leaf set provided in the TREEFILE."
            , "An 'observed symbol set' may be created by collecting the union of symbols that occurs in the FASTC file. This union should be a subset of the alphabet provided in the TCMFILE."
            ]
        , command "TREEFILE" $ fileFormatHelp TreeFileRequest ["Say goodbye"]
        , command  "TCMFILE" $ fileFormatHelp  TcmFileRequest ["Silly option"]
        ]

    fileFormatHelp :: ExampleFileRequest -> [String] -> ParserInfo ExampleFileRequest
    fileFormatHelp val = info (pure val) . progDesc . unlines
-}
