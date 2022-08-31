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

{-# Language ConstraintKinds #-}
{-# Language FlexibleContexts #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}

module Alignment.Pairwise
    ( alignNaively
    , alignNeedlemanWunsch
    , alignUkkonen
    ) where

import Alignment.Pairwise.NeedlemanWunsch
import Alignment.Pairwise.Ukkonen
