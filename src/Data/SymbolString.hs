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
  , renderAligns
  , renderSingleton
  , renderString
  , renderSymbolString
  ) where

import           Control.DeepSeq
import           Data.Alphabet
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty       (NonEmpty(..), intersperse)
import qualified Data.List.NonEmpty as NE
import           Data.Pointed
import           Data.Map                  (Map)
import qualified Data.Map           as M
import           Data.Set                  (Set)
import qualified Data.Set           as S
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Vector.NonEmpty
import           GHC.Generics


type SymbolString = Vector (SymbolContext Char)


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


instance Show a => Show (SymbolContext a) where

    show (Align  _ x _ _) = "A" <> show x
    show (Delete _ x _  ) = "D" <> show x
    show (Insert _ x   _) = "I" <> show x


renderString :: Foldable1 f => Alphabet Char -> f (SymbolAmbiguityGroup Char) -> String
renderString alphabet = (\s -> "["<>s<>"]") . intercalate1 "," . fmap renderAmbiguityGroup . toNonEmpty
  where
    renderAmbiguityGroup :: SymbolAmbiguityGroup Char -> String
    renderAmbiguityGroup xs = foldMap f alphabet
      where
        f v
          | v `elem` xs = [v]
          | otherwise   = " "


renderSymbolString :: Alphabet Char -> SymbolString -> String
renderSymbolString alphabet = (\s -> "[ "<>s<>" ]") . intercalate1 ", " . fmap renderContext . toNonEmpty
  where
    renderContext (Align  _ x y z) = mconcat ["α:", renderAmbiguityGroup x, "|", renderAmbiguityGroup y, "|", renderAmbiguityGroup z]
    renderContext (Delete _ x y  ) = mconcat ["δ:", renderAmbiguityGroup x, "|", renderAmbiguityGroup y, "|", blankSpace            ]
    renderContext (Insert _ x   z) = mconcat ["ι:", renderAmbiguityGroup x, "|", blankSpace            , "|", renderAmbiguityGroup z]

    renderAmbiguityGroup :: SymbolAmbiguityGroup Char -> String
    renderAmbiguityGroup xs = foldMap f alphabet
      where
        f v
          | v `elem` xs = [v]
          | otherwise   = " "

    blankSpace   = replicate (length alphabet) ' ' 


renderAligns :: Alphabet Char -> SymbolString -> String
renderAligns alphabet = (\s -> "[ "<>s<>" ]") . intercalate1 ", " . fmap renderContext . toNonEmpty
  where
    renderContext (Align  _ x _ _) = renderAmbiguityGroup x
    renderContext (Delete _ x _  ) = renderAmbiguityGroup (point '-')
    renderContext (Insert _ x   _) = renderAmbiguityGroup (point '-')

    renderAmbiguityGroup :: SymbolAmbiguityGroup Char -> String
    renderAmbiguityGroup xs = foldMap f alphabet
      where
        f v
          | v `elem` xs = [v]
          | otherwise   = " "


renderSingleton :: Alphabet Char -> SymbolString -> String
renderSingleton alphabet = foldMap renderContext . toNonEmpty
  where
    gap = gapSymbol alphabet
    renderContext (Align  _ x _ _) = pure . NE.head $ toNonEmpty x
    renderContext _  = [gap]


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
reverseContext (Align  cost med lhs rhs) = Align  cost med rhs lhs
reverseContext (Delete cost med lhs    ) = Insert cost med     lhs 
reverseContext (Insert cost med     rhs) = Delete cost med rhs     


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
