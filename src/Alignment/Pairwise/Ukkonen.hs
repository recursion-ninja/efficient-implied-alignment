-----------------------------------------------------------------------------
-- |
-- Module      :  Alignment.Pairwise.Ukkonen
-- Copyright   :  (c) 2018 Alex Washburn
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-- Direct optimization export of Ukkonen's space & time saving algorithm.
--
-----------------------------------------------------------------------------

module Alignment.Pairwise.Ukkonen
  ( ukkonenDO
  ) where

import Alignment.Pairwise.Ukkonen.Internal
