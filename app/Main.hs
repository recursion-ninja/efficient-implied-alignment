module Main where


import Data.Char
import Data.Semigroup ((<>))
import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen (string)


data UserInput =
     UserInput
     { treeFile   :: String
     , dataFile   :: String
     ,  tcmFile   :: String
     , outputFile :: String
     , verbose    :: Bool
     } deriving (Show)


main :: IO ()
main = parseUserInput >>= print


parseUserInput = customExecParser preferences $ info userInput description
  where
    userInput =
        UserInput
          <$> argSpec 't' "tree"   "Newick tree file"
          <*> argSpec 'd' "data"   "FASTC data file"
          <*> argSpec 'm' "tcm"    "Transition Cost Matrix file with symbol alphabet"
          <*> argSpec 'o' "output"  "Ouput file"
          <*> switch  (mconcat [short 'v', long "verbose", help "Display debugging informaion"])

    argSpec c s h = strOption $ mconcat [short c, long s, help h, metavar (toUpper <$> s <> "FILE")]

    description = mconcat
        [ fullDesc
        , headerDoc (Just (string "\n  Tree-based multiple string alignment program"))
        , footerDoc (Just mempty)
        ]

    preferences = prefs $ mconcat [showHelpOnError, showHelpOnEmpty]
