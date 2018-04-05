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
  ( InitialInternalNode()
  , InitialLeaf()
  , FinalizedInternalNode()
  , FinalizedLeaf()
  , alignedString
  , inputString
  , localCost
  , outputString
  , preliminaryString
  , subtreeCost
  ) where

import Data.Decoration.Class
import Data.Decoration.Internal
import Data.Decoration.Leaf
