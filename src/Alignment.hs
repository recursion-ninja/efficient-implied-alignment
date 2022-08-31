-----------------------------------------------------------------------------
-- |
-- Module      :  Alignment
-- Copyright   :  (c) 2018 Alex Washburn
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# Language FlexibleContexts #-}

module Alignment
    ( PairwiseAlignment
    , postorderLogic
    , preorderInternalLogic
    , preorderLeafLogic
    , preorderRootLogic
    ) where

import Alignment.Internal
