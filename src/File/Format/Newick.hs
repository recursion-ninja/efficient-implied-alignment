{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module File.Format.Newick
  ( NewickNode(branchLength, descendants, newickLabel)
  , isLeaf
  , newickNode
  , newickStreamParser
  , renderNewickForest
  ) where


import File.Format.Newick.Internal
import File.Format.Newick.Parser
import Text.Megaparsec


-- |
-- Parses an entire stream into a zero or more 'NewickForest's.
newickStreamParser :: (MonadParsec e s m, Token s ~ Char) => m NewickNode
newickStreamParser = newickStandardDefinition <* eof
