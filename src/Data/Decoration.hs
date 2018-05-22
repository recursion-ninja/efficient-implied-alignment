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
  ( InitialInternalNode(InitialInternalNode)
  , InitialLeaf(InitialLeaf)
  , FinalizedInternalNode(FinalizedInternalNode)
  , FinalizedLeaf(FinalizedLeaf)
  , HasAlignedString(..)
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
