-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Decoration.Node
-- Copyright   :  (c) 2018 Alex Washburn
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Data.Decoration.Node where


import Control.DeepSeq
import Control.Lens
import Data.Decoration.Class
import Data.SymbolString
import GHC.Generics


data PreliminaryNode
   = PreliminaryNode
   { _PN_SubtreeCost       :: {-# UNPACK #-} !Word
   , _PN_LocalCost         :: {-# UNPACK #-} !Word
   , _PN_PreliminaryString :: {-# UNPACK #-} !SymbolString
   } deriving (Eq, Generic)


data FinalizedNode
   = FinalizedNode
   { _F_SubtreeCost       :: {-# UNPACK #-} !Word
   , _F_LocalCost         :: {-# UNPACK #-} !Word
   , _F_PreliminaryString :: {-# UNPACK #-} !SymbolString
   , _F_AlignedString     :: {-# UNPACK #-} !SymbolString
   , _F_IsRoot            :: !Bool
   } deriving (Eq, Generic)


instance NFData PreliminaryNode


instance NFData FinalizedNode


instance HasSubtreeCost PreliminaryNode Word where

    subtreeCost = lens _PN_SubtreeCost (\e x -> e { _PN_SubtreeCost = x })


instance HasSubtreeCost FinalizedNode Word where

    subtreeCost = lens _F_SubtreeCost (\e x -> e { _F_SubtreeCost = x })


instance HasLocalCost PreliminaryNode Word where

    localCost = lens _PN_LocalCost (\e x -> e { _PN_LocalCost = x })


instance HasLocalCost FinalizedNode Word where

    localCost = lens _F_LocalCost (\e x -> e { _F_LocalCost = x })


instance HasPreliminaryString PreliminaryNode SymbolString where

    preliminaryString = lens _PN_PreliminaryString (\e x -> e { _PN_PreliminaryString = x })


instance HasPreliminaryString FinalizedNode SymbolString where

    preliminaryString = lens _F_PreliminaryString (\e x -> e { _F_PreliminaryString = x })


instance HasAlignedString FinalizedNode SymbolString where

    alignedString = lens _F_AlignedString (\e x -> e { _F_AlignedString = x })


instance HasIsRoot FinalizedNode Bool where

    isRoot = lens _F_IsRoot (\e x -> e { _F_IsRoot = x })
