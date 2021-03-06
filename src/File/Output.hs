{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module File.Output
  ( writeFastaFile
  ) where

import           Control.Lens
import           Data.Alphabet
import           Data.BTree
import           Data.Decoration
import           Data.Key
import           Data.Map          (Map)
import qualified Data.Map          as M
import           Data.SymbolString
import           Data.Text.Short   (ShortText, toString)
import           Data.UserInput    (AlphabetType (..))


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
  -> Map ShortText SymbolString
collectLeafAlignments = foldMapWithKey (\k v -> M.singleton k $ v ^. alignedString)


renderAlignments :: AlphabetType -> Alphabet Char -> Map ShortText SymbolString -> String
renderAlignments alphabetType alphabet = foldMapWithKey f
  where
    f k v = unlines
        [ "> " <> toString k
        , g v
        , ""
        ]

    g :: SymbolString -> String
    g = case alphabetType of
          Standard -> renderString alphabet
          DNA      -> renderLikeDNA
          RNA      -> renderLikeDNA
