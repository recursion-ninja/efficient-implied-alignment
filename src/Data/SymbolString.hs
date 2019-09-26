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
  , filterGaps
  , reverseContext
  , symbolAlignmentMedian
  , renderAligns
  , renderMonospacedGroup
  , renderSingleton
  , renderString
  , renderSymbolString
  ) where

import           Control.DeepSeq
import           Data.Alphabet
import           Data.Bits
import           Data.Key
import           Data.List.NonEmpty       (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Semigroup.Foldable
import           Data.Vector.NonEmpty
import           Data.Word
import           GHC.Generics
import           Prelude hiding (filter)


type SymbolString = Vector SymbolContext


data  SymbolContext
    = Align  {-# UNPACK #-} !SymbolAmbiguityGroup {-# UNPACK #-} !SymbolAmbiguityGroup {-# UNPACK #-} !SymbolAmbiguityGroup
    | Delete {-# UNPACK #-} !SymbolAmbiguityGroup {-# UNPACK #-} !SymbolAmbiguityGroup
    | Insert {-# UNPACK #-} !SymbolAmbiguityGroup                                      {-# UNPACK #-} !SymbolAmbiguityGroup
    deriving (Eq, Generic, Ord)


-- |
-- A non-empty set of characters.
newtype SymbolAmbiguityGroup = SAG Word16
    deriving (Bits, Eq, FiniteBits, Enum, Generic, Ord)


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

    show (SAG v) = show v


instance Show SymbolContext where

    show (Align  m x y) = mconcat ["A|", showPad m, "|", showPad x, "|", showPad y, "|"]
    show (Delete m x  ) = mconcat ["D|", showPad m, "|", showPad x, "|",   "     ", "|"]
    show (Insert m   y) = mconcat ["I|", showPad m, "|",   "     ", "|", showPad y, "|"]


-- We pad things to be exactly 5 characters long with leading space because a
-- Word16 can have at most five, base 10 digits
showPad :: SymbolAmbiguityGroup -> String
showPad (SAG x) = replicate (5 - length shown) ' ' <> shown
  where
    shown = show x 


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
    renderContext (Align  x _ _) = mconcat ["α: ", renderGroup x ]
    renderContext (Delete x _  ) = mconcat ["δ: ", renderGroup x ]
    renderContext (Insert x   _) = mconcat ["ι: ", renderGroup x ]

    renderGroup = renderMonospacedGroup alphabet


renderMonospacedGroup :: Alphabet Char -> SymbolAmbiguityGroup -> String
renderMonospacedGroup alphabet grp = foldMapWithKey f alphabet
  where
    f k v
      | grp `testBit` k = [v]
      | otherwise       = " "


renderAligns :: Alphabet Char -> SymbolString -> String
renderAligns alphabet = (\s -> "[ "<>s<>" ]") . intercalate1 ", " . fmap renderContext . toNonEmpty
  where
    renderContext (Align x _ _) = renderAmbiguityGroup x
    renderContext _             = renderAmbiguityGroup $ encodeAmbiguityGroup alphabet ('-':|[])


renderSingleton :: Alphabet Char -> SymbolString -> String
renderSingleton alphabet = foldMap renderContext . toNonEmpty
  where
    gap = gapSymbol alphabet
    renderContext (Align x _ _) = pure . NE.head $ decodeAmbiguityGroup alphabet x
    renderContext _ = [gap]


renderAmbiguityGroup :: SymbolAmbiguityGroup -> String
renderAmbiguityGroup = show


symbolAlignmentMedian :: SymbolContext -> SymbolAmbiguityGroup
symbolAlignmentMedian (Align  x _ _) = x
symbolAlignmentMedian (Delete x _  ) = x
symbolAlignmentMedian (Insert x   _) = x


reverseContext :: SymbolContext -> SymbolContext
reverseContext (Align  med x y) = Align  med y x
reverseContext (Delete med x  ) = Insert med   x
reverseContext (Insert med   y) = Delete med y


filterGaps :: SymbolString -> SymbolString
filterGaps = filter f 
  where
    f Align  {} = True
    f Delete {} = False
    f Insert {} = False


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
