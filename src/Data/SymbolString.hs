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

{-# LANGUAGE DeriveFoldable, DeriveGeneric, GeneralizedNewtypeDeriving, TypeFamilies #-}

module Data.SymbolString
  ( SymbolAmbiguityGroup()
  , SymbolContext(..)
  , SymbolString()
  , symbolAlignmentCost
  , symbolAlignmentMedian
  ) where


import Control.DeepSeq
import Data.Foldable
import Data.Key
import Data.List.NonEmpty (NonEmpty(..), intersperse)
import Data.Pointed
import Data.Set           (Set)
import Data.Semigroup
import Data.Semigroup.Foldable
import GHC.Generics


type SymbolString = NonEmpty (SymbolContext String)


data  SymbolContext a
    = Align  Word (SymbolAmbiguityGroup a) (SymbolAmbiguityGroup a) (SymbolAmbiguityGroup a)
    | Delete Word (SymbolAmbiguityGroup a) (SymbolAmbiguityGroup a)
    | Insert Word (SymbolAmbiguityGroup a)                          (SymbolAmbiguityGroup a)
    deriving (Eq, Generic, Ord)


-- |
-- A non-empty set of characters.
newtype SymbolAmbiguityGroup a = SAG (Set a)
    deriving (Eq, Foldable, Generic, Ord, Pointed, Semigroup)


instance Foldable1 SymbolAmbiguityGroup where

    foldMap1 f = foldMap1 f . toNonEmpty

    toNonEmpty (SAG x) =
        case toList x of
          x:xs -> x:|xs
          _    -> error "The impossible happened when calling toNonEmpty on a SymbolAmbiguityGroup"


instance NFData a => NFData (SymbolContext a)


instance NFData a => NFData (SymbolAmbiguityGroup a)


instance Show a => Show (SymbolAmbiguityGroup a) where

    show = (\x -> "{"<>x<>"}") . sconcat . intersperse ", " . fmap show . toNonEmpty


symbolAlignmentCost :: SymbolContext a -> Word
symbolAlignmentCost (Align  x _ _ _) = x
symbolAlignmentCost (Delete x _ _  ) = x
symbolAlignmentCost (Insert x _ _  ) = x


symbolAlignmentMedian :: SymbolContext a -> SymbolAmbiguityGroup a
symbolAlignmentMedian (Align  _ x _ _) = x
symbolAlignmentMedian (Delete _ x _  ) = x
symbolAlignmentMedian (Insert _ x _  ) = x
