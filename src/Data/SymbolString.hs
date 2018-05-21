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

{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, Strict, TypeFamilies, UnboxedSums #-}

module Data.SymbolString
  ( SymbolAmbiguityGroup()
  , SymbolContext(..)
  , SymbolString()
  , (/\)
  , encodeAmbiguityGroup
  , decodeAmbiguityGroup
--  , gap
  , reverseContext
--  , symbolAlignmentCost
  , symbolAlignmentMedian
  , renderAligns
  , renderSingleton
  , renderString
  , renderSymbolString
  ) where

import           Control.DeepSeq
import           Data.Alphabet
import           Data.Bits
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty       (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Semigroup.Foldable
import           Data.Vector.NonEmpty
import           Data.Word
import           GHC.Generics
import           Numeric


type SymbolString = Vector SymbolContext


data  SymbolContext
    = Align  {-# UNPACK #-} SymbolAmbiguityGroup
    | Delete {-# UNPACK #-} SymbolAmbiguityGroup {-# UNPACK #-} SymbolAmbiguityGroup
    | Insert {-# UNPACK #-} SymbolAmbiguityGroup {-# UNPACK #-} SymbolAmbiguityGroup
    deriving (Eq, Generic, Ord)


-- |
-- A non-empty set of characters.
newtype SymbolAmbiguityGroup = SAG Word32
    deriving (Bits, Eq, Enum, Generic, Ord)


encodeAmbiguityGroup :: (Eq a, Foldable1 f) => Alphabet a -> f a -> SymbolAmbiguityGroup
encodeAmbiguityGroup alphabet xs = force . SAG $ foldlWithKey f 0 alphabet
  where
    f a k e
      | e `elem` xs = a `setBit` k
      | otherwise   = a


decodeAmbiguityGroup :: Alphabet a -> SymbolAmbiguityGroup -> NonEmpty a
decodeAmbiguityGroup alphabet xs = NE.fromList $ foldMapWithKey f alphabet
  where
    f k e
      | xs `testBit` k = [e]
      | otherwise      = []

  
instance NFData SymbolContext


instance NFData SymbolAmbiguityGroup


instance Semigroup SymbolAmbiguityGroup where

    (<>) (SAG lhs) (SAG rhs) = SAG $ lhs .|. rhs


instance Show SymbolAmbiguityGroup where

    show (SAG v) = showHex v ""


instance Show SymbolContext where

    show (Align  x) = "A" <> show x
    show (Delete x _) = "D" <> show x
    show (Insert x _) = "I" <> show x


renderString :: Foldable1 f => Alphabet Char -> f SymbolAmbiguityGroup -> String
renderString alphabet = foldMap renderGroup . toNonEmpty
  where
    renderGroup grp =
      case decodeAmbiguityGroup alphabet grp of
        x:|[] -> [x]
        x:|xs -> mconcat ["[",[x],xs,"]"]


renderSymbolString :: Alphabet Char -> SymbolString -> String
renderSymbolString alphabet = (\s -> "[ "<>s<>" ]") . intercalate1 ", " . fmap renderContext . toNonEmpty
  where
    renderContext (Align  x  ) = mconcat ["α: ", renderGroup x ]
    renderContext (Delete x _) = mconcat ["δ: ", renderGroup x ]
    renderContext (Insert x _) = mconcat ["ι: ", renderGroup x ]

    renderGroup grp = foldMapWithKey f alphabet
      where
        f k v
          | grp `testBit` k = [v]
          | otherwise       = " "


renderAligns :: Alphabet Char -> SymbolString -> String
renderAligns alphabet = (\s -> "[ "<>s<>" ]") . intercalate1 ", " . fmap renderContext . toNonEmpty
  where
    renderContext (Align  x) = renderAmbiguityGroup x
    renderContext _          = renderAmbiguityGroup $ encodeAmbiguityGroup alphabet ('-':|[])


renderSingleton :: Alphabet Char -> SymbolString -> String
renderSingleton alphabet = foldMap renderContext . toNonEmpty
  where
    gap = gapSymbol alphabet
    renderContext (Align x) = toList $ decodeAmbiguityGroup alphabet x
    renderContext _  = [gap]


renderAmbiguityGroup :: SymbolAmbiguityGroup -> String
renderAmbiguityGroup = show


symbolAlignmentMedian :: SymbolContext -> SymbolAmbiguityGroup
symbolAlignmentMedian (Align  x)   = x
symbolAlignmentMedian (Delete x _) = x
symbolAlignmentMedian (Insert x _) = x


reverseContext :: SymbolContext -> SymbolContext
reverseContext (Delete med x) = Insert med x
reverseContext (Insert med x) = Delete med x
reverseContext e = e


-- |
-- Attempt to take the intersection two 'SymbolAmbiguityGroup's. Returns a
-- @Nothing@ value is the 'SymbolAmbiguityGroup's are disjoint or the @Just@ the
-- intersection.
(/\)
  :: SymbolAmbiguityGroup
  -> SymbolAmbiguityGroup
  -> Maybe SymbolAmbiguityGroup
(/\) (SAG lhs) (SAG rhs)
  | zeroBits == intersect = Nothing
  | otherwise             = Just $ SAG intersect
  where
    intersect = lhs .&. rhs
