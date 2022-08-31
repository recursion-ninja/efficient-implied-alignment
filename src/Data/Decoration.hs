-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Decoration
-- Copyright   :  (c) 2018 Alex Washburn
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Data.Decoration
    ( FinalizedInternalNode (FinalizedInternalNode)
    , FinalizedLeaf (FinalizedLeaf)
    , FinalizedNode (FinalizedNode)
    , HasAlignedString (..)
    , InitialInternalNode (InitialInternalNode)
    , InitialLeaf (InitialLeaf)
    , PreliminaryNode (PreliminaryNode)
    , finalizedString
    , inputString
    , isRoot
    , localCost
    , outputString
    , preliminaryString
    , subtreeCost
    ) where

import Data.Decoration.Class
import Data.Decoration.Internal
import Data.Decoration.Leaf
import Data.Decoration.Node
