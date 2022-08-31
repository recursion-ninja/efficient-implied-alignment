-----------------------------------------------------------------------------
-- |
-- Module:  Test.NucleotideSequence
--
-- Arbitrary instance for DNA sequencea
--
-- Allows for base ambiguities and gaps. The sequence will be non-empty.
--
-----------------------------------------------------------------------------

{-# Language FlexibleInstances #-}
{-# Language ImportQualifiedPost #-}
{-# Language LambdaCase #-}
{-# Language MultiParamTypeClasses #-}

module Test.NucleotideSequence
    ( NucleotideBase (..)
    , NucleotideSequence (..)
    ) where

import Data.Alphabet.IUPAC
import Data.Bimap (elems)
import Data.Foldable
import Data.List (delete)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.SymbolString
import Data.Vector.NonEmpty hiding (generate)
import Test.QuickCheck hiding (generate)
import Test.SmallCheck.Series


-- |
-- Represents an arbitrary, non-empty ambiguity group which may include gaps.
newtype NucleotideBase
    = NB SymbolAmbiguityGroup


-- |
-- Represents an arbitrary, non-empty sequence of nucleotide bases that may be
-- ambiguous and/or include gaps.
newtype NucleotideSequence
    = NS SymbolString


instance Arbitrary NucleotideBase where

    arbitrary = NB . encodeAmbiguityGroup alphabet <$> elementGen
        where elementGen = elements $ elems iupacToDna


instance Arbitrary NucleotideSequence where

    arbitrary =
        let streamGen :: Gen a -> Gen (Vector a)
            streamGen = fmap (fromNonEmpty . NE.fromList) . listOf1

            randRange :: Gen Word
            randRange  = choose (0, 3)

            contextGen = oneof [alignGen, deleteGen, insertGen]
            leafGen    = elementGen >>= (\x -> pure $ Align x x x)
            alignGen   = Align <$> elementGen <*> elementGen <*> elementGen
            deleteGen  = Delete <$> elementGen <*> elementGen
            insertGen  = Insert <$> elementGen <*> elementGen
            elementGen = fmap (encodeAmbiguityGroup alphabet) . elements $ elems iupacToDna
        in  randRange >>= \case
            0 -> NS <$> streamGen leafGen
            _ -> NS <$> streamGen contextGen


instance Monad m => Serial m NucleotideBase where

    series = generate $ const (NB . encodeAmbiguityGroup alphabet <$> validSpace)
        where
            validSpace = fmap NE.fromList $ [] `delete` powerSet (toList alphabet)
            powerSet :: [a] -> [[a]]
            powerSet []       = [[]]
            powerSet (x : xs) = [ x : ps | ps <- powerSet xs ] <> powerSet xs


instance Show NucleotideBase where

    show (NB x) = toList . NE.head . encodeIUPAC iupacToDna . (:| []) $ decodeAmbiguityGroup alphabet x


instance Show NucleotideSequence where

    show (NS xs) =
        toList
            .   fmap NE.head
            .   encodeIUPAC iupacToDna
            $   decodeAmbiguityGroup alphabet
            .   symbolAlignmentMedian
            <$> xs


alphabet :: Alphabet Char
alphabet = fromSymbols ['A', 'C', 'G', 'T']
