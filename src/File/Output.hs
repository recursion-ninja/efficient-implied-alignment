{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module File.Output
  ( writeFastaFile
  ) where

import           Control.Lens
import           Data.Alphabet
import           Data.Alphabet.IUPAC
import           Data.BTree
import           Data.Decoration
import           Data.Foldable
import           Data.Key
import qualified Data.List.NonEmpty    as NE 
import           Data.Map                     (Map)
import qualified Data.Map              as M
import           Data.SymbolString
import           Data.UserInput               (AlphabetType(..))


writeFastaFile
  :: HasAlignedString a SymbolString
  => AlphabetType
  -> Alphabet Char
  -> BTree b a
  -> FilePath
  -> IO ()
writeFastaFile alphabetType alphabet tree path =
    writeFile path . renderAlignments alphabetType alphabet $ collectLeafAlignments tree


collectLeafAlignments
  :: HasAlignedString a SymbolString
  => BTree b a
  -> Map String SymbolString
collectLeafAlignments = foldMapWithKey (\k v -> M.singleton k $ v ^. alignedString)


renderAlignments :: AlphabetType -> Alphabet Char -> Map String SymbolString -> String
renderAlignments alphabetType alphabet = foldMapWithKey f
  where
    f k v = unlines
        [ "> " <> k
        , g v
        , ""
        ]

    g :: SymbolString -> String
    g = case alphabetType of
          Standard -> renderString alphabet  . fmap symbolAlignmentMedian
          DNA      -> toList . fmap NE.head . encodeIUPAC iupacToDna . fmap (decodeAmbiguityGroup alphabet . symbolAlignmentMedian)
          RNA      -> toList . fmap NE.head . encodeIUPAC iupacToRna . fmap (decodeAmbiguityGroup alphabet . symbolAlignmentMedian) 
