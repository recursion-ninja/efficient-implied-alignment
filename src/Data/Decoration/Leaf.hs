-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Decoration.Leaf
-- Copyright   :  (c) 2018 Alex Washburn
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Data.Decoration.Leaf where

import Control.DeepSeq
import Control.Lens
import Data.Decoration.Class
import Data.SymbolString
import GHC.Generics


data InitialLeaf
   = InitialLeaf
   { _IL_SubtreeCost       :: Word
   , _IL_LocalCost         :: Word
   , _IL_PreliminaryString :: SymbolString
   , _IL_InputString       :: SymbolString
   } deriving (Eq, Generic)


data FinalizedLeaf
   = FinalizedLeaf
   { _FL_SubtreeCost          :: Word
   , _FL_LocalCost            :: Word
   , _FL_PreliminaryString    :: SymbolString
   , _FL_AlignedString        :: SymbolString
   , _FL_InputString          :: SymbolString
   , _FL_OutputString         :: SymbolString
   } deriving (Eq, Generic)


instance NFData InitialLeaf


instance NFData FinalizedLeaf


instance HasSubtreeCost InitialLeaf Word where

    subtreeCost = lens _IL_SubtreeCost (\e x -> e { _IL_SubtreeCost = x })


instance HasSubtreeCost FinalizedLeaf Word where

    subtreeCost = lens _FL_SubtreeCost (\e x -> e { _FL_SubtreeCost = x })


instance HasLocalCost InitialLeaf Word where

    localCost = lens _IL_LocalCost (\e x -> e { _IL_LocalCost = x })


instance HasLocalCost FinalizedLeaf Word where

    localCost = lens _FL_LocalCost (\e x -> e { _FL_LocalCost = x })


instance HasPreliminaryString InitialLeaf SymbolString where

    preliminaryString = lens _IL_PreliminaryString (\e x -> e { _IL_PreliminaryString = x })


instance HasPreliminaryString FinalizedLeaf SymbolString where

    preliminaryString = lens _FL_PreliminaryString (\e x -> e { _FL_PreliminaryString = x })


instance HasInputString InitialLeaf SymbolString where

    inputString = lens _IL_InputString (\e x -> e { _IL_InputString = x })


instance HasInputString FinalizedLeaf SymbolString where

    inputString = lens _FL_InputString (\e x -> e { _FL_InputString = x })


instance HasAlignedString FinalizedLeaf SymbolString where

    alignedString = lens _FL_AlignedString (\e x -> e { _FL_AlignedString = x })


instance HasOutputString FinalizedLeaf SymbolString where

    outputString = lens _FL_OutputString (\e x -> e { _FL_OutputString = x })
