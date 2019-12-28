{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module File.Format.Newick
  ( NewickNode(branchLength, descendants, newickLabel)
  , isLeaf
  , newickNode
  , newickStreamParser
  , renderNewickForest
  ) where


import           Data.BTree
import           File.Format.Newick.Internal
import           File.Format.Newick.Parser
import           Text.Megaparsec


-- |
-- Parses an entire stream into a zero or more 'NewickForest's.
newickStreamParser :: (MonadParsec e s m, Token s ~ Char) => m (BTree () ())
newickStreamParser = newickStandardDefinition <* eof
