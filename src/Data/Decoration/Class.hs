-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Decoration.Class
-- Copyright   :  (c) 2018 Alex Washburn
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

module Data.Decoration.Class where

import Control.Lens


class HasAlignedString s a | s -> a where

    alignedString :: Lens' s a
    {-# MINIMAL alignedString #-}


class HasFinalizedString s a | s -> a where

    finalizedString :: Lens' s a
    {-# MINIMAL finalizedString #-}


class HasInputString s a | s -> a where

    inputString :: Lens' s a
    {-# MINIMAL inputString #-}


class HasLocalCost s a | s -> a where

    localCost :: Lens' s a
    {-# MINIMAL localCost #-}


class HasOutputString s a | s -> a where

    outputString :: Lens' s a
    {-# MINIMAL outputString #-}


class HasPreliminaryString s a | s -> a where

    preliminaryString :: Lens' s a
    {-# MINIMAL preliminaryString #-}


class HasSubtreeCost s a | s -> a where

    subtreeCost :: Lens' s a
    {-# MINIMAL subtreeCost #-}
