-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SymbolString
-- Copyright   :  (c) 2018 Alex Wahsburn
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveFoldable, GeneralizedNewtypeDeriving, TypeFamilies #-}

module Data.SymbolString
  ( SymbolAmbiguityGroup()
  , SymbolContext(..)
  , SymbolString()
  , symbolAlignmentCost
  , symbolAlignmentMedian
  ) where


import Data.Key
import Data.List.NonEmpty
import Data.Pointed
import Data.Set
import Data.Semigroup
import Data.Semigroup.Foldable


type SymbolString = NonEmpty (SymbolContext String)


data  SymbolContext a
    = Align  Word (SymbolAmbiguityGroup a) (SymbolAmbiguityGroup a) (SymbolAmbiguityGroup a)
    | Delete Word (SymbolAmbiguityGroup a) (SymbolAmbiguityGroup a)
    | Insert Word (SymbolAmbiguityGroup a)                          (SymbolAmbiguityGroup a)
    deriving (Eq, Ord)


-- |
-- A non-empty set of characters.
newtype SymbolAmbiguityGroup a = SAG (Set a)
    deriving (Eq, Ord, Pointed, Semigroup)


symbolAlignmentCost :: SymbolContext a -> Word
symbolAlignmentCost (Align  x _ _ _) = x
symbolAlignmentCost (Delete x _ _  ) = x
symbolAlignmentCost (Insert x _ _  ) = x


symbolAlignmentMedian :: SymbolContext a -> SymbolAmbiguityGroup a
symbolAlignmentMedian (Align  _ x _ _) = x
symbolAlignmentMedian (Delete _ x _  ) = x
symbolAlignmentMedian (Insert _ x _  ) = x
