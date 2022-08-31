-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Fasta
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The Newick file format was developed by an informal committee meeting at
-- Newick's seafood restaurant. The grammar definition of the Newick format
-- was never formally specified, but Gary Olsen's interpretation of the
-- original newick format has been documented here:
-- http://evolution.genetics.washington.edu/phylip/newick_doc.html
--
-- After over two decades of informal usage, the Extended Newick file format
-- was proposed in a BCM publication which allowed node labels to be non-
-- unique and merged to a single node with shared ancestors and descendants.
-- This allowed for easy manual annotating of phylogentic trees.
--
-- Another half decade later, the Forest Extended Newick was proposed by
-- Professor Wheeler to model collections of disjoint phylogenetic trees.
-- This new format allowed grouping many Extended Newick trees into a
-- forest to be analyzed collectively.
--
-- This parser correctly parses the Newick file format, and the super set
-- Extended Newick filed format, and the new Forest Extended Newick format.
--
-----------------------------------------------------------------------------

{-# Language FlexibleContexts #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}

module File.Format.Newick
    ( NewickNode
    , branchLength
    , descendants
    , isLeaf
    , newickLabel
    , newickStreamParser
    ) where

import Data.BTree
import File.Format.Newick.Internal
import File.Format.Newick.Parser
import Text.Megaparsec


-- |
-- Parses an entire stream into zero or more 'NewickForest's.
{-# INLINABLE newickStreamParser #-}
newickStreamParser :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m (BTree () ())
newickStreamParser = newickStandardDefinition <* eof
