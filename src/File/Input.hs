{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module File.Input
  ( FileInput(..)
  , parseFileInput
  , unifyInput
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Alphabet
import           Data.Alphabet.IUPAC
import           Data.Bifunctor
import           Data.BTree
import           Data.Char
import           Data.Decoration
import           Data.Foldable
import           Data.Functor                     (($>))
import           Data.Key
import           Data.List.NonEmpty               (NonEmpty (..), intersperse)
import           Data.Map                         (Map)
import qualified Data.Map                         as M
import           Data.Matrix                      (Matrix)
import           Data.Maybe
import           Data.Pointed
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Set                         (Set)
import           Data.SymbolString
import           Data.TCM
import           Data.Text.Short                  (ShortText, toString)
import           Data.UserInput
import           Data.Validation
import qualified Data.Vector.NonEmpty             as V
import qualified Data.Vector.Unboxed.NonEmpty     as VU
import           Data.Void
import           File.Format.Fasta
import           File.Format.Newick
import           File.Format.TransitionCostMatrix
import           Prelude                          hiding (lookup)
import           System.Timing
import           Text.Megaparsec


type CharacterSequence = V.Vector (VU.Vector Char)


data  FileInput
    = FileInput
    { inputAlphabet  :: Alphabet Char
    , inputTCM       :: TransitionCostMatrix
    , inputTree      :: BTree () PreliminaryNode
    , parseTime      :: CPUTime
    , unifyTime      :: CPUTime
    , precomputeTime :: CPUTime
    }


data  UnificationError
    = LeafLabelMissingInDataSet String
    | DataLabelMissingInLeafSet String
    deriving (Eq, Show)


parseFileInput
  :: UserInput
  -> IO (Either String FileInput)
parseFileInput input = do
    (parseTime', parseResults) <- runFileParsers input
    case toEither parseResults of
      Left  pErr -> pure . Left . fold1 $ intersperse "\n\n" pErr
      Right parseData -> do
          (unifyTime', unifyResults)  <- runUnification (alphabetType input) parseData
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
        pure . first (pure . errorBundlePretty). fromEither $ parse parser filePath stream


runUnification
  :: MonadIO m
  => AlphabetType
  -> (FastaParseResult, BTree b a, TCM)
  -> m (CPUTime, Validation (NonEmpty String) (Alphabet Char, Matrix Word, BTree b PreliminaryNode))
runUnification alphaType (dataVal, treeVal, tcmVal) = timeOp $ do
    let TCM symbolList matrix = tcmVal
        alphabet    = fromSymbols $ VU.toList symbolList
        alphaChange = case alphaType of
                        Standard -> id
                        DNA      -> decodeIUPAC iupacToDna
                        RNA      -> decodeIUPAC iupacToRna
        leafDataMap :: Map ShortText CharacterSequence
        leafDataMap = fmap VU.fromNonEmpty . alphaChange <$> fastaToMap dataVal
        badSymbols  = validateSymbolsAndAlphabet tcmVal leafDataMap
        badLinking  = first (fmap show) $ unifyInput alphabet leafDataMap treeVal
    pure $ (\x -> (alphabet, matrix, x)) <$> (badLinking <* badSymbols)


precomputeTCM :: Alphabet a -> Matrix Word -> IO (CPUTime, TransitionCostMatrix)
precomputeTCM alphabet matrix = timeOp $ do
    let scm  = force $ buildSymbolChangeMatrix matrix
    pure . force $ buildTransitionCostMatrix alphabet scm


fastaToMap :: FastaParseResult -> Map ShortText (V.Vector (NonEmpty Char))
fastaToMap = foldMap (M.singleton <$> fastaLabel <*> reformSequence . fastaSymbols)
  where
    reformSequence v =
      let g :: Int -> NonEmpty Char
          g i = (v VU.! i) :| []
      in  V.generate (VU.length v) g


validateSymbolsAndAlphabet :: TCM -> Map Identifier CharacterSequence -> Validation (NonEmpty String) ()
validateSymbolsAndAlphabet (TCM symbolList _) m = fromEither $
    case foldMapWithKey f m of
      []   -> Right ()
      x:xs -> Left $ x:|xs
  where
    symbolSet :: Set Char
    symbolSet = (point '-' <>) . foldMap point $ VU.toList symbolList

    f :: Identifier -> CharacterSequence -> [String]
    f i s =
        case catMaybes . toList $ mapWithKey g s of
          []   -> []
          x:xs -> [preamble <> unlines (x:xs)]
      where
        preamble = "For leaf " <> toString i <> ", the following symbols were found but not specified in the alphabet: "

        g :: Int -> VU.Vector Char -> Maybe String
        g k v =
          case filter (`notElem` symbolSet) $ VU.toList v of
            []   -> Nothing
            x:xs -> Just $ fold
                [ "  at index "
                , show k
                , " unrecognized symbols: { "
                , fold1 . intersperse ", " . fmap pure $ x:|xs
                , " }"
                ]


unifyInput
  :: ( Foldable1 f
     , Key c ~ ShortText
     , Keyed c
     , Lookup c
     , Traversable c
     )
  => Alphabet Char
  -> c (f (VU.Vector Char))
  -> BTree b a
  -> Validation (NonEmpty UnificationError) (BTree b PreliminaryNode)
unifyInput alphabet dataCollection genericTree = validatedDataSet *> initializedTree
  where
    leafTagSet :: Set ShortText
    leafTagSet     = foldMap point leafTaggedTree
    dataSetKeys    = mapWithKey const dataCollection
    leafTaggedTree = setLeafLabels genericTree

    validatedDataSet = traverse f dataSetKeys
      where
        f :: ShortText -> Validation (NonEmpty UnificationError) ()
        f k = validate err isInLeafSet k $> ()
          where
            err = pure . DataLabelMissingInLeafSet $ toString k

            isInLeafSet x
              | x `elem` leafTagSet = Just x
              | otherwise = Nothing

    initializedTree = traverse f leafTaggedTree
      where
        f :: ShortText -> Validation (NonEmpty UnificationError) PreliminaryNode
        f k = validationNel $
            case k `lookup` dataCollection of
              Nothing -> Left . LeafLabelMissingInDataSet $ toString k
              Just xs -> let !ss = buildSymbolString xs
                         in  Right $ PreliminaryNode 0 0 ss
          where
            buildSymbolString = foldMap1 (pure . buildAmbiguityGroup)
            buildAmbiguityGroup x =
                let !y = encodeAmbiguityGroup alphabet $ VU.toNonEmpty x
                in  Align y y y
