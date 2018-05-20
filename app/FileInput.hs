{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies #-}

module FileInput
  ( parseFileInput
  , unifyInput
  ) where

import           Alignment
import           Control.DeepSeq
import           Control.Lens
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
import           Data.Matrix.ZeroIndexed      (matrix)
import           Data.Maybe
import           Data.Map                     (Map)
import qualified Data.Map              as M
import           Data.Pointed
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Set                     (Set)
import           Data.SymbolString
import           Data.TCM
import           Data.Validation
import           Data.Void
import           File.Format.Fastc
import           File.Format.Newick
import           File.Format.TransitionCostMatrix
import           Options.Applicative
import           Prelude               hiding (lookup)
import           System.IO
import           Text.Megaparsec
import           UserInput


parseFileInput
  :: UserInput
  -> IO (Either String (Alphabet Char, TransitionCostMatrix Char, BTree () InitialInternalNode))
parseFileInput input = do 
    dataResult <- readAndParse  fastcStreamParser $ dataFile input
    treeResult <- readAndParse newickStreamParser $ treeFile input
    tcmResult  <- readAndParse    tcmStreamParser $  tcmFile input
    case toEither $ (,,) <$> dataResult <*> treeResult <*> tcmResult of
      Left  pErr -> pure . Left . fold1 $ intersperse "\n\n" pErr
      Right (dataVal, treeVal, tcmVal) ->
        let leafDataMap = fastcToMap dataVal
            badSymbols  = validateSymbolsAndAlphabet tcmVal leafDataMap
            badLinking  = first (fmap show) . unifyInput leafDataMap $ newickNodeToBTree treeVal
        in  case toEither $ badLinking <* badSymbols of
              Left  uErr -> pure . Left . fold1 $ intersperse "\n" $ show <$> uErr
              Right tree ->
                let TCM symbolList matrix = tcmVal
                    alphabet = fromSymbols symbolList
                    scm      = force $ buildSymbolChangeMatrix alphabet (truncate <$> matrix)
                    tcm      = force $ buildTransitionCostMatrix alphabet scm
                in  pure . Right $ force (alphabet, tcm, tree)
  where
    readAndParse
      :: Parsec Void String a
      -> FilePath
      -> IO (Validation (NonEmpty String) a)
    readAndParse parser filePath = do
        stream <- readFile filePath
        pure . first (pure . parseErrorPretty' stream). fromEither $ parse parser filePath stream


fastcToMap :: FastcParseResult -> Map String CharacterSequence
fastcToMap = foldMap (M.singleton <$> fastcLabel <*> fastcSymbols) 


newickNodeToBTree :: NewickNode -> BTree () ()
newickNodeToBTree node =
    case descendants node of
      []    -> Leaf $ NodeDatum (fromJust (newickLabel node)) ()
      [x]   -> newickNodeToBTree x
      x:y:_ -> let label = fromMaybe "" $ newickLabel node
               in  Internal
                     (NodeDatum label ())
                     (newickNodeToBTree x)
                     (newickNodeToBTree y)


validateSymbolsAndAlphabet :: TCM -> Map Identifier CharacterSequence -> Validation (NonEmpty String) ()
validateSymbolsAndAlphabet (TCM symbolList _) m = fromEither $
    case foldMapWithKey f m of
      []   -> Right {}
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
        
        g :: Int -> (NonEmpty Char) -> Maybe String
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
  :: ( Foldable  c
     , Foldable1 f
     , Foldable1 t
     , Key c ~ String
     , Keyed c
     , Lookup c
     , Traversable c
     )
  => c (f (t Char))
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
    
