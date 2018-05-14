{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies #-}

module Main where

import           Alignment
import           Control.Lens
import           Data.Alphabet
import           Data.BTree
import           Data.Char
import           Data.Decoration
import           Data.Foldable
import           Data.Functor                 (($>))
import           Data.Key
import           Data.List.NonEmpty           (NonEmpty(..))
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
import           Options.Applicative
import           Prelude               hiding (lookup)
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen (string)


data  UserInput
    = UserInput
    { dataFile    :: String
    , treeFile    :: String
    , tcmFile     :: String
    , outputFile  :: String
    , verbose     :: Bool
    , commandHelp :: ExampleFileRequest
    } deriving (Show)


data  ExampleFileRequest
    = DataFileRequest
    | TreeFileRequest
    | TcmFileRequest
    | NoFileRequest
    deriving (Eq, Show)


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
--    parseUserInput >>= print
    case toEither $ unifyInput dataSetA topologyA of
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
          putStrLn . renderAlignment nodeRenderer leafRenderer . preorder' $ postorder' tree
          putStrLn ""
  where 
    postorder' = postorder stringAligner
    preorder'  = preorder preorderRootLogic (preorderInternalLogic defaultTripleCompare) preorderLeafLogic
    
    stringAligner = postorderLogic (ukkonenDO defaultAlphabet defaultTCM)

    inputRenderer x i = mconcat
        [ i
        , ": "
        , renderSingleton defaultAlphabet $ x ^. preliminaryString
        ]
    
    leafRenderer x i = mconcat
        [ i
        , ": "
--        , renderSymbolString defaultAlphabet $ x ^. preliminaryString
--        , renderString       defaultAlphabet $ x ^.   finalizedString
--        , renderSymbolString defaultAlphabet $ x ^. alignedString
--        , renderAligns defaultAlphabet $ x ^. alignedString
        , renderSingleton defaultAlphabet $ x ^. alignedString
        ]
    nodeRenderer x _ = mconcat
        [ "?: "
--        , renderSymbolString defaultAlphabet $ x ^. preliminaryString
--        , renderString       defaultAlphabet $ x ^.   finalizedString
--        , renderSymbolString defaultAlphabet $ x ^. alignedString
--        , renderAligns defaultAlphabet $ x ^. alignedString
        , renderSingleton defaultAlphabet $ x ^. alignedString
        ]


parseUserInput = customExecParser preferences $ info (helper <*> userInput) description
  where
    userInput =
        UserInput
          <$> argSpec 'd' "data"   "FASTC data file"
          <*> argSpec 't' "tree"   "Newick tree file"
          <*> argSpec 'm' "tcm"    "Transition Cost Matrix file with symbol alphabet"
          <*> argSpec 'o' "output" "Output file for alignments"
          <*> switch  (mconcat [short 'v', long "verbose", help "Display debugging informaion"])
          <*> commandHelperDescriptions

    argSpec c s h = strOption $ mconcat [short c, long s, help h, metavar (toUpper <$> s <> "FILE")]

    description = mconcat
        [ fullDesc
        , headerDoc . Just $ string "\n  Tree-based multiple string alignment program"
        , footerDoc $ Just mempty
        ]

    preferences = prefs $ mconcat [showHelpOnError, showHelpOnEmpty]

    commandHelperDescriptions = hsubparser $ mconcat
        [ command "DATAFILE" $ fileFormatHelp DataFileRequest
            [ "The DATAFILE provided should be in FASTC format."
            , "The FASTC file is similar to the FASTA file format in structure."
            , "There are one or more ordered pairs of leaf identifier lines and string definition lines."
            , "A leaf identifier line begins with a '>' symbol followed by an identifier for the leaf node on the same line."
            , "An identifier for a leaf node is defined as one or more characters (excluding '>') that are not seperated by whitespace."
            , "The string definition line is the the next non-whitespace line."
            , "The string definition line contains one or more symbols and seperated by in-line whitespace."
            , "A symbol is defined as one or more characters (excluding '>') that are not seperated by whitespace."
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


defaultAlphabet :: Alphabet String
defaultAlphabet = fromSymbols $ pure <$> "ACGT-"


defaultTripleCompare :: ThreewayCompare String
defaultTripleCompare = buildThreeWayCompare defaultAlphabet defaultTCM


defaultTCM :: TransitionCostMatrix String
defaultTCM = tcm
  where
    tcm = buildTransitionCostMatrix defaultAlphabet scm
    scm = buildSymbolChangeMatrix   defaultAlphabet fakeParseInput
    fakeParseInput = matrix 5 5 (\(i,j) -> if i == j then 0 else 1)


dataSetA :: Map String (NonEmpty (NonEmpty String))
dataSetA = M.fromList
    [ ("A", toNonEmpties $ 'A':|"CGT")
    , ("B", toNonEmpties $ 'A':|"CG")
    , ("C", toNonEmpties $ 'A':|"C")
    , ("D", toNonEmpties $ 'A':|"")
    ]
  where
    toNonEmpties = foldMap1 (pure . pure . pure) 


topologyA :: BTree () ()
topologyA =
    Internal blank
    ( Leaf (NodeDatum "A" ()) )
    ( Internal blank
      ( Leaf (NodeDatum "B" ()) )
      ( Internal blank
        ( Leaf (NodeDatum "C" ()) )
        ( Leaf (NodeDatum "D" ()) )
      )
    )
  where
    blank = NodeDatum "" ()


dataSetB :: Map String (NonEmpty (NonEmpty String))
dataSetB = M.fromList
    [ ("A", toNonEmpties $ 'A':|"")
    , ("B", toNonEmpties $ 'A':|"C")
    , ("C", toNonEmpties $ 'A':|"CG")
    , ("D", toNonEmpties $ 'A':|"CGT")
    ]
  where
    toNonEmpties = foldMap1 (pure . pure . pure) 


topologyB :: BTree () ()
topologyB =
    Internal blank
    ( Leaf (NodeDatum "A" ()) )
    ( Internal blank
      ( Leaf (NodeDatum "B" ()) )
      ( Internal blank
        ( Leaf (NodeDatum "C" ()) )
        ( Leaf (NodeDatum "D" ()) )
      )
    )
  where
    blank = NodeDatum "" ()


dataSetC :: Map String (NonEmpty (NonEmpty String))
dataSetC = M.fromList
    [ ("A", toNonEmpties $ 'T':|"GCA")
    , ("B", toNonEmpties $ 'G':|"CA")
    , ("C", toNonEmpties $ 'C':|"A")
    , ("D", toNonEmpties $ 'A':|"")
    ]
  where
    toNonEmpties = foldMap1 (pure . pure . pure) 


topologyC :: BTree () ()
topologyC =
    Internal blank
    ( Leaf (NodeDatum "A" ()) )
    ( Internal blank
      ( Leaf (NodeDatum "B" ()) )
      ( Internal blank
        ( Leaf (NodeDatum "C" ()) )
        ( Leaf (NodeDatum "D" ()) )
      )
    )
  where
    blank = NodeDatum "" ()


dataSetD :: Map String (NonEmpty (NonEmpty String))
dataSetD = M.fromList
    [ ("A", toNonEmpties $ 'A':|"A")
    , ("B", toNonEmpties $ 'A':|"A")
    , ("C", toNonEmpties $ 'A':|"TA")
    , ("D", toNonEmpties $ 'A':|"TA")
    , ("E", toNonEmpties $ 'A':|"TA")
    , ("F", toNonEmpties $ 'A':|"A")
    , ("G", toNonEmpties $ 'A':|"A")
    , ("H", toNonEmpties $ 'A':|"A")
    , ("I", toNonEmpties $ 'A':|"TA")
    , ("J", toNonEmpties $ 'A':|"TA")
    ]
  where
    toNonEmpties = foldMap1 (pure . pure . pure) 


topologyD :: BTree () ()
topologyD =
    Internal blank
    ( Leaf (NodeDatum "A" ()) )
    ( Internal blank
      ( Leaf (NodeDatum "B" ()) )
      ( Internal blank
        ( Leaf (NodeDatum "C" ()) )
        ( Internal blank
          ( Leaf (NodeDatum "D" ()) )
          ( Internal blank
            ( Leaf (NodeDatum "E" ()) )
            ( Internal blank
              ( Leaf (NodeDatum "F" ()) )
              ( Internal blank
                ( Leaf (NodeDatum "G" ()) )
                ( Internal blank
                  ( Leaf (NodeDatum "H" ()) )
                  ( Internal blank
                    ( Leaf (NodeDatum "I" ()) )
                    ( Leaf (NodeDatum "J" ()) )
                  )
                )
              )
            )
          )
        )
      )
    )
  where
    blank = NodeDatum "" ()


dataSetE :: Map String (NonEmpty (NonEmpty String))
dataSetE = M.fromList
    [ ("A", toNonEmpties $ 'A':|"ATT")
    , ("B", toNonEmpties $ 'A':|"ATT")
    , ("C", toNonEmpties $ 'A':|"ATT")
    , ("D", toNonEmpties $ 'A':|"ACTT")
    , ("E", toNonEmpties $ 'A':|"ACTT")
    , ("F", toNonEmpties $ 'A':|"ACTT")
    , ("G", toNonEmpties $ 'A':|"ATT")
    , ("H", toNonEmpties $ 'A':|"ATT")
    ]
  where
    toNonEmpties = foldMap1 (pure . pure . pure) 


topologyE :: BTree () ()
topologyE =
    Internal blank
    ( Leaf (NodeDatum "A" ()) )
    ( Internal blank
      ( Leaf (NodeDatum "B" ()) )
      ( Internal blank
        ( Leaf (NodeDatum "C" ()) )
        ( Internal blank
          ( Leaf (NodeDatum "D" ()) )
          ( Internal blank
            ( Leaf (NodeDatum "E" ()) )
            ( Internal blank
              ( Leaf (NodeDatum "F" ()) )
              ( Internal blank
                ( Leaf (NodeDatum "G" ()) )
                ( Leaf (NodeDatum "H" ()) )
              )
            )
          )
        )
      )
    )
  where
    blank = NodeDatum "" ()


unifyInput
  :: ( Foldable  c
     , Foldable1 f
     , Foldable1 t
     , Key c ~ String
     , Keyed c
     , Lookup c
     , Traversable c
     )
  => c (f (t String))
  -> BTree b a
  -> Validation (NonEmpty UnificationError) (BTree b InitialInternalNode)
unifyInput dataCollection genericTree = validatedDataSet *> initializedTree
  where
    dataSetKeys   = mapWithKey const dataCollection
    leafTaggedTree = setLeafLabels genericTree
    leafTagSet :: Set String
    leafTagSet    = foldMap point leafTaggedTree
    
    validatedDataSet = traverse f dataSetKeys
      where
        f :: String -> Validation (NonEmpty UnificationError) ()
        f k = validate err (`elem` leafTagSet) k $> ()
          where
            err = pure $ DataLabelMissingInLeafSet k

    initializedTree = traverse f leafTaggedTree
      where
        f :: String -> Validation (NonEmpty UnificationError) InitialInternalNode
        f k = validationNel $
            case k `lookup` dataCollection of
              Nothing -> Left $ LeafLabelMissingInDataSet k
              Just xs -> let !ss = buildSymbolString xs
                         in  Right $ InitialInternalNode 0 0 ss
          where
            buildSymbolString = foldMap1 (pure . buildAmbiguityGroup)
            buildAmbiguityGroup x =
                let !y = foldMap1 point x
                in  Align 0 y y y


data  UnificationError
    = LeafLabelMissingInDataSet String
    | DataLabelMissingInLeafSet String
    deriving (Eq, Show)
    
