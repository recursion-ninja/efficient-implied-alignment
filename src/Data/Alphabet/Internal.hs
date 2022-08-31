-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Alphabet.Internal
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- We must ensure that missing and gap are appropriately
-- code as "-" & "?", respectively, before this module is used, i.e., as output
-- from either parsers or in unification step.
--
-----------------------------------------------------------------------------

{-# Language DeriveDataTypeable #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language ImportQualifiedPost #-}
{-# Language OverloadedStrings #-}
{-# Language TypeFamilies #-}


-- We do this because we added an orphan instance IsString Char
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Alphabet.Internal
    ( Alphabet ()
    , AmbiguityGroup
    , fromSymbols
    , gapSymbol
    ) where

import Control.DeepSeq (NFData)
import Control.Monad.State.Strict
import Data.Data
import Data.Foldable
import Data.Key
import Data.List (intercalate, sort)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Monoid
import Data.Semigroup.Foldable
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Data.Vector.NonEmpty (Vector)
import Data.Vector.NonEmpty qualified as NEV
import GHC.Generics (Generic)
import Prelude hiding (lookup, unzip, zip)
import Test.QuickCheck


-- |
-- A non empty collection of symbols from an 'Alphabet'.
type AmbiguityGroup a = NonEmpty a


-- |
-- A collection of symbols and optional corresponding state names.
data  Alphabet a
    = Alphabet
    { symbolVector :: {-# UNPACK #-} !(Vector a)
    , stateNames   :: [a]
    }
    deriving stock (Data, Functor, Generic)


type instance Key Alphabet = Int


-- Newtypes for coercing and consolidation of alphabet input processing logic
newtype AlphabetInputSingle a
    = ASI { toSingle :: a }
    deriving stock (Eq, Ord)


-- |
-- -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
-- -   Supporting code and data structures:
-- -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
--


newtype UnnamedSymbol a
    = Unnamed a
    deriving stock (Generic)


newtype NamedSymbol a
    = Named (a, a)
    deriving stock (Generic)


class InternalClass a where

    gapSymbol'        :: a
    isGapSymboled     :: a -> Bool
    isMissingSymboled :: a -> Bool


-- |
-- \( \mathcal{O} \left( n * \log_2 n \right) \)
alphabetPreprocessing :: (Ord a, InternalClass a, Foldable t) => t a -> NonEmpty a
alphabetPreprocessing = appendGapSymbol . sort . removeSpecialSymbolsAndDuplicates . toList
    where
        appendGapSymbol :: InternalClass a => [a] -> NonEmpty a
        appendGapSymbol xs = case xs of
            []     -> gapSymbol' :| []
            y : ys -> y :| (ys <> [gapSymbol'])

        removeSpecialSymbolsAndDuplicates = (`evalState` mempty) . filterM f
            where
                f :: (InternalClass a, MonadState (Set a) f, Ord a) => a -> f Bool
                f x
                    | isGapSymboled x = pure False
                    | isMissingSymboled x = pure False
                    | otherwise = do
                        seenSet <- get
                        _       <- put $ x `Set.insert` seenSet
                        pure $ x `notElem` seenSet


fromSingle :: a -> AlphabetInputSingle a
fromSingle = ASI


-- |
-- \( \mathcal{O} \left( n * \log_2 n \right) \)
--
-- Constructs an 'Alphabet' from a 'Foldable' structure of symbols which are
-- 'IsString' values.
fromSymbols :: (Ord a, IsString a, Foldable t) => t a -> Alphabet a
fromSymbols inputSymbols =
    let symbols =
            NEV.fromNonEmpty . fmap toSingle . alphabetPreprocessing . fmap fromSingle $ toList inputSymbols
    in  Alphabet symbols []


-- |
-- \( \mathcal{O} \left( 1 \right) \)
--
-- Retrieves the "gap character" from the alphabet.
gapSymbol :: Alphabet a -> a
gapSymbol alphabet = alphabet ! (length alphabet - 1)


instance (Ord a, IsString a) => Arbitrary (Alphabet a) where

    arbitrary =
        let -- We do this to simplify Alphabet generation, ensuring that there is at least one non gap symbol.
            buildPrefix = fromSymbols . flip take symbolSpace
            randomCount = (arbitrary :: Gen Int) `suchThat` (\x -> 0 < x && x <= symbolCount)
            symbolSpace = fromString . pure <$> ['0' .. '9'] <> ['A' .. 'Z'] <> ['a' .. 'z'] <> "?-"
            symbolCount = length symbolSpace
        in  buildPrefix <$> randomCount


-- |
-- \( \mathcal{O} \left( n * \log_2 n \right) \)
instance Ord a => Eq (Alphabet a) where

    lhs == rhs = length lhs == length rhs && sort (toList lhs) == sort (toList rhs)


instance Foldable Alphabet where

    {-# INLINE toList #-}
    toList = toList . symbolVector

    {-# INLINE foldMap #-}
    foldMap f = foldMap f . symbolVector

    {-# INLINE foldr #-}
    foldr f e = foldr f e . symbolVector

    {-# INLINE foldl #-}
    foldl f e = foldl f e . symbolVector

    {-# INLINE foldr1 #-}
    foldr1 f = foldr1 f . symbolVector

    {-# INLINE foldl1 #-}
    foldl1 f = foldl1 f . symbolVector

    {-# INLINE length #-}
    length = length . symbolVector


instance Foldable1 Alphabet where

    {-# INLINE fold1 #-}
    fold1 = fold1 . symbolVector

    {-# INLINE foldMap1 #-}
    foldMap1 f = foldMap1 f . symbolVector

    {-# INLINE toNonEmpty #-}
    toNonEmpty = toNonEmpty . symbolVector


instance FoldableWithKey Alphabet where

    {-# INLINE foldrWithKey #-}
    foldrWithKey f e = foldrWithKey f e . symbolVector

    {-# INLINE foldlWithKey #-}
    foldlWithKey f e = foldlWithKey f e . symbolVector


instance FoldableWithKey1 Alphabet where

    {-# INLINE foldMapWithKey1 #-}
    foldMapWithKey1 f = foldMapWithKey1 f . symbolVector


instance Indexable Alphabet where

    {-# INLINE index #-}
    index a i = fromMaybe raiseError $ i `lookup` a
        where
            raiseError = error $ fold
                [ "Error indexing Alphabet at location "
                , show i
                , ", valid inclusive index range is [0, "
                , show $ length a - 1
                , "]."
                ]


instance (Eq a, IsString a) => InternalClass (AlphabetInputSingle a) where

    gapSymbol'        = ASI $ fromString "-"

    isGapSymboled     = (gapSymbol' ==)

    isMissingSymboled = (ASI (fromString "?") ==)


instance IsString Char where

    fromString = head


instance Lookup Alphabet where

    {-# INLINE lookup #-}
    lookup i = lookup i . symbolVector


instance NFData a => NFData (Alphabet a)


instance NFData a => NFData (UnnamedSymbol a)


instance NFData a => NFData (  NamedSymbol a)


instance Show a => Show (Alphabet a) where

    show x = fold ["Alphabet: {", intercalate ", " $ show <$> toList x, "}"]
