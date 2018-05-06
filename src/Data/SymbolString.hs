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
  , (/\)
  , gap
  , reverseContext
  , symbolAlignmentCost
  , symbolAlignmentMedian
  ) where

import           Control.DeepSeq
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty (NonEmpty(..), intersperse)
import           Data.Pointed
import           Data.Set           (Set)
import qualified Data.Set      as S
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Vector.NonEmpty
import           GHC.Generics


type SymbolString = Vector (SymbolContext String)


data  SymbolContext a
    = Align  Word (SymbolAmbiguityGroup a) (SymbolAmbiguityGroup a) (SymbolAmbiguityGroup a)
    | Delete Word (SymbolAmbiguityGroup a) (SymbolAmbiguityGroup a)
    | Insert Word (SymbolAmbiguityGroup a)                          (SymbolAmbiguityGroup a)
    deriving (Eq, Generic, Ord, Show)


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


gap :: SymbolAmbiguityGroup String
gap = SAG $ S.singleton "-"


reverseContext :: SymbolContext a -> SymbolContext a
reverseContext (Align  cost med lhs rhs) = (Align  cost med rhs lhs) 
reverseContext (Delete cost med lhs    ) = (Insert cost med     lhs) 
reverseContext (Insert cost med     rhs) = (Insert cost med rhs    ) 


-- |
-- Attempt to take the intersection two 'SymbolAmbiguityGroup's. Returns a
-- @Nothing@ value is the 'SymbolAmbiguityGroup's are disjoint or the @Just@ the
-- intersection.
(/\)
  :: Ord a
  => SymbolAmbiguityGroup a
  -> SymbolAmbiguityGroup a
  -> Maybe (SymbolAmbiguityGroup a)
(/\) (SAG lhs) (SAG rhs)
  | null intersect = Nothing
  | otherwise      = Just $ SAG intersect
  where
    intersect = S.intersection lhs rhs
