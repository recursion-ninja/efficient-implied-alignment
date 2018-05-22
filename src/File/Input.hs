{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies #-}

module File.Input
  ( FileInput(..)
  , parseFileInput
  , unifyInput
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Alphabet
import           Data.Bifunctor
import           Data.BTree
import           Data.Char
import           Data.Decoration
import           Data.Foldable
import           Data.Functor                 (($>))
import           Data.Key
import           Data.List.NonEmpty           (NonEmpty(..), intersperse)
import qualified Data.List.NonEmpty    as NE
import           Data.Maybe
import           Data.Map                     (Map)
import qualified Data.Map              as M
import           Data.Matrix.ZeroIndexed      (Matrix)
import           Data.Pointed
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Set                     (Set)
import           Data.SymbolString
import           Data.TCM
import           Data.Validation
import           Data.UserInput
import           Data.Void
import           File.Format.Fasta
import           File.Format.Newick
import           File.Format.TransitionCostMatrix
import           Prelude               hiding (lookup)
import           System.Timing
import           Text.Megaparsec


data  FileInput
    = FileInput
    { inputAlphabet  :: Alphabet Char
    , inputTCM       :: TransitionCostMatrix
    , inputTree      :: BTree () InitialInternalNode
    , parseTime      :: CPUTime
    , unifyTime      :: CPUTime
    , precomputeTime :: CPUTime
    }


parseFileInput
  :: UserInput
  -> IO (Either String FileInput)
parseFileInput input = do
    (parseTime', parseResults) <- runFileParsers input
    case toEither parseResults of
      Left  pErr -> pure . Left . fold1 $ intersperse "\n\n" pErr
      Right parseData -> do
          (unifyTime', unifyResults)  <- runUnification parseData
          case toEither unifyResults of
            Left  uErr -> pure . Left . fold1 . intersperse "\n" $ show <$> uErr
            Right (alphabet, matrix, tree) -> do
                (tcmTime, tcm) <- precomputeTCM alphabet matrix
                pure . Right $ FileInput
                    { inputAlphabet  = alphabet
                    , inputTCM       = tcm
                    , inputTree      = tree
                    , parseTime      = parseTime'
                    , unifyTime      = unifyTime'
                    , precomputeTime = tcmTime
                    }


runFileParsers
  :: UserInput
  -> IO (CPUTime, Validation (NonEmpty String) (FastaParseResult, BTree () (), TCM))
runFileParsers input = timeOp $ do
    dataResult <- readAndParse  fastaStreamParser $ dataFile input
    treeResult <- readAndParse newickStreamParser $ treeFile input
    tcmResult  <- readAndParse    tcmStreamParser $  tcmFile input
    pure $ (,,) <$> dataResult <*> treeResult <*> tcmResult
  where
    readAndParse
      :: Parsec Void String a
      -> FilePath
      -> IO (Validation (NonEmpty String) a)
    readAndParse parser filePath = do
        stream <- readFile filePath
        pure . first (pure . parseErrorPretty' stream). fromEither $ parse parser filePath stream


runUnification
  :: MonadIO m
  => (FastaParseResult, BTree b a, TCM)
  -> m (CPUTime, Validation (NonEmpty String) (Alphabet Char, Matrix Word, BTree b InitialInternalNode))
runUnification (dataVal, treeVal, tcmVal) = timeOp $ do
    let TCM symbolList matrix = tcmVal
        alphabet    = fromSymbols symbolList
        leafDataMap = fastaToMap dataVal
        badSymbols  = validateSymbolsAndAlphabet tcmVal leafDataMap
        badLinking  = first (fmap show) $ unifyInput alphabet leafDataMap treeVal
    pure $ (\x -> (alphabet, matrix, x)) <$> (badLinking <* badSymbols)
  

precomputeTCM :: Alphabet a -> Matrix Word -> IO (CPUTime, TransitionCostMatrix)
precomputeTCM alphabet matrix = timeOp $ do
    let scm  = force $ buildSymbolChangeMatrix matrix
    pure . force $ buildTransitionCostMatrix alphabet scm 


fastaToMap :: FastaParseResult -> Map String CharacterSequence
fastaToMap = foldMap (M.singleton <$> fastaLabel <*> fastaSymbols) 


validateSymbolsAndAlphabet :: TCM -> Map Identifier CharacterSequence -> Validation (NonEmpty String) ()
validateSymbolsAndAlphabet (TCM symbolList _) m = fromEither $
    case foldMapWithKey f m of
      []   -> Right ()
      x:xs -> Left $ x:|xs
  where
    symbolSet :: Set Char
    symbolSet = foldMap point symbolList

    f :: Identifier -> CharacterSequence -> [String]
    f i s =
        case catMaybes . toList $ mapWithKey g s of
          []   -> []
          x:xs -> [preamble <> unlines (x:xs)]
      where
        preamble = "For leaf " <> i <> ", the following symbols were found but not specified in the alphabet: "
        
        g :: Int -> NonEmpty Char -> Maybe String
        g k v =
          case NE.filter (`notElem` symbolSet) v of
            []   -> Nothing
            x:xs -> Just $ mconcat
                [ "  at index "
                , show k
                , " unrecognized symbols: { "
                , fold1 . intersperse ", " . fmap pure $ x:|xs
                , " }"
                ]


unifyInput
  :: ( Foldable1 f
     , Foldable1 t
     , Key c ~ String
     , Keyed c
     , Lookup c
     , Traversable c
     )
  => Alphabet Char
  -> c (f (t Char))
  -> BTree b a
  -> Validation (NonEmpty UnificationError) (BTree b InitialInternalNode)
unifyInput alphabet dataCollection genericTree = validatedDataSet *> initializedTree
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
                let !y = encodeAmbiguityGroup alphabet x
                in  Align y y y


data  UnificationError
    = LeafLabelMissingInDataSet String
    | DataLabelMissingInLeafSet String
    deriving (Eq, Show)
    
