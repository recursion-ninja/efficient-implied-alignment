-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Extended
-- Copyright   :  (c) 2018 Alex Washburn
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}

module Numeric.Extended
  ( ExtendedNatural()
  , ExtendedNumber(..)
  , ExtendedReal()
  , Finite
  ) where

import Numeric.Extended.Internal
import Numeric.Extended.Natural
import Numeric.Extended.Real
