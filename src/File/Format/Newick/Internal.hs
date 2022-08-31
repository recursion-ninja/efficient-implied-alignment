-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Newick.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Utility functions and types for parsing Newick tree files into a topological tree structure.
--
-----------------------------------------------------------------------------

{-# Language DeriveAnyClass #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language ImportQualifiedPost #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}

module File.Format.Newick.Internal
    ( NewickForest
    , NewickNode (..)
    ) where

import Control.DeepSeq (NFData)
import Data.Data
import Data.Foldable hiding (null)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text, null, unpack)
import Data.Vector (Vector, fromListN)
import GHC.Generics (Generic)
import Prelude hiding (null)


{----
  - The Newick file format was developed by an informal committee meeting at
  - Newick's seafood restaurant. The grammar definition of the Newick format
  - was never formally specified, but Gary Olsen's interpretation of the
  - original newick format has been documented here:
  - http://evolution.genetics.washington.edu/phylip/newick_doc.html
  -
  - After over two decades of informal usage, the Extended Newick file format
  - was proposed in a BCM publication which allowed node labels to be non-
  - unique and merged to a single node with shared ancestors and descendants.
  - This allowed for easy manual annotating of phylogenetic trees.
  -
  - Another half decade later, the Forest Extended Newick was proposed by
  - Professor Wheeler to model collections of disjoint phylogenetic trees.
  - This new format allowed grouping many Extended Newick trees into a
  - forest to be analyzed collectively.
  -
  - This parser correctly parses both Newick file formats, and the super set
  - Extended Newick filed format.
  -}


-- |
-- One or more trees in a "Phylogenetic Forest".
type NewickForest = NonEmpty NewickNode


-- |
-- A node in a "Phylogenetic Forest"
data  NewickNode
    = NewickNode
    { childNodes     :: {-# UNPACK #-} !(Vector NewickNode)
    , internalName   :: {-# UNPACK #-} !Text
    , internalLength :: !(Maybe Double)
    }
    deriving stock (Data, Eq, Generic, Ord)
    deriving anyclass NFData


instance Semigroup NewickNode where

    {-# INLINABLE (<>) #-}
    lhs <> rhs =
        NewickNode { childNodes = fromListN 2 [lhs, rhs], internalName = "", internalLength = Nothing }


instance Show NewickNode where

    show (NewickNode d n b) = fold [name, len, " ", show d]
        where
            name :: String
            name
                | null n    = "Node"
                | otherwise = unpack n
            len :: String
            len = maybe "" ((':' :) . show) b
