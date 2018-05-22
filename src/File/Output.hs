{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies #-}

module File.Output
  ( writeFastaFile
  ) where

import           Control.Lens
import           Data.Alphabet
import           Data.BTree
import           Data.Decoration
import           Data.Key
import           Data.Map                     (Map)
import qualified Data.Map              as M
import           Data.SymbolString


writeFastaFile
  :: HasAlignedString a SymbolString
  => Alphabet Char
  -> BTree b a
  -> FilePath
  -> IO ()
writeFastaFile alphabet tree path = writeFile path . renderAlignments alphabet
                                  $ collectLeafAlignments tree


collectLeafAlignments
  :: HasAlignedString a SymbolString
  => BTree b a
  -> Map String SymbolString
collectLeafAlignments = foldMapWithKey (\k v -> M.singleton k $ v ^. alignedString)


renderAlignments :: Alphabet Char -> Map String SymbolString -> String
renderAlignments alphabet = foldMapWithKey f
  where
    f k v = unlines
        [ "> " <> k
        , renderString alphabet $ symbolAlignmentMedian <$> v
        ]
