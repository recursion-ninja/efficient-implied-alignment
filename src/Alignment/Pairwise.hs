-----------------------------------------------------------------------------
-- |
-- Module      :  Alignment.Pairwise
-- Copyright   :  (c) 2018 Alex Washburn
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-- Pairwise direct optimization alignment functions using a variety of techniques.
--
-----------------------------------------------------------------------------

module Alignment.Pairwise
  ( OverlapFunction
  , getOverlap
  , minimalChoice
  , naiveDO
  , naiveDOMemo
  , ukkonenDO
  ) where


import Alignment.Pairwise.Internal
import Alignment.Pairwise.NeedlemanWunsch
import Alignment.Pairwise.Ukkonen
