-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Decoration.Internal
-- Copyright   :  (c) 2018 Alex Washburn
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Data.Decoration.Internal where

import Control.DeepSeq
import Control.Lens
import Data.Decoration.Class
import Data.SymbolString
import Data.Vector.NonEmpty
import GHC.Generics


data InitialInternalNode
   = InitialInternalNode
   { _IIN_SubtreeCost       :: Word
   , _IIN_LocalCost         :: Word
   , _IIN_PreliminaryString :: SymbolString
   } deriving (Eq, Generic)


data FinalizedInternalNode
   = FinalizedInternalNode
   { _FIN_SubtreeCost       :: Word
   , _FIN_LocalCost         :: Word
   , _FIN_PreliminaryString :: SymbolString
   , _FIN_FinalizedString   :: Vector (SymbolAmbiguityGroup String)
   , _FIN_AlignedString     :: SymbolString
   , _FIN_IsRoot            :: Bool
   } deriving (Eq, Generic)


instance NFData InitialInternalNode


instance NFData FinalizedInternalNode


instance HasSubtreeCost InitialInternalNode Word where

    subtreeCost = lens _IIN_SubtreeCost (\e x -> e { _IIN_SubtreeCost = x })


instance HasSubtreeCost FinalizedInternalNode Word where

    subtreeCost = lens _FIN_SubtreeCost (\e x -> e { _FIN_SubtreeCost = x })


instance HasLocalCost InitialInternalNode Word where

    localCost = lens _IIN_LocalCost (\e x -> e { _IIN_LocalCost = x })


instance HasLocalCost FinalizedInternalNode Word where

    localCost = lens _FIN_LocalCost (\e x -> e { _FIN_LocalCost = x })


instance HasPreliminaryString InitialInternalNode SymbolString where

    preliminaryString = lens _IIN_PreliminaryString (\e x -> e { _IIN_PreliminaryString = x })


instance HasPreliminaryString FinalizedInternalNode SymbolString where

    preliminaryString = lens _FIN_PreliminaryString (\e x -> e { _FIN_PreliminaryString = x })


instance HasFinalizedString FinalizedInternalNode (Vector (SymbolAmbiguityGroup String)) where

    finalizedString = lens _FIN_FinalizedString (\e x -> e { _FIN_FinalizedString = x })


instance HasAlignedString FinalizedInternalNode SymbolString where

    alignedString = lens _FIN_AlignedString (\e x -> e { _FIN_AlignedString = x })


instance HasIsRoot FinalizedInternalNode Bool where

    isRoot = lens _FIN_IsRoot (\e x -> e { _FIN_IsRoot = x })
