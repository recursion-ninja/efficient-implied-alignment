-----------------------------------------------------------------------------
-- |
-- Module      :  Alignment.Pairwise
-- Copyright   :  (c) 2018 Alex Washburn
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-- Pairwise direct optimization alignment functions using a variety of techniques.
--
-----------------------------------------------------------------------------

{-# Language ConstraintKinds #-}
{-# Language FlexibleContexts #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}

module Alignment.Pairwise
    ( comparativeDO
    , naiveDO
    , naiveDOMemo
    , ukkonenDO
    , unboxedUkkonenDO
    ) where

import Alignment.Pairwise.NeedlemanWunsch
import Alignment.Pairwise.Ukkonen
import Alignment.Pairwise.UnboxedUkkonen

import Data.Alphabet
import Data.Alphabet.IUPAC
import Data.Foldable
import Data.Key
import Data.List.NonEmpty (NonEmpty)
import Data.SymbolString
import Data.Vector.Instances ()
import Data.Vector.NonEmpty hiding (filter)
import Prelude hiding (zipWith)

import Debug.Trace


{-# INLINABLE comparativeDO #-}
{-# SPECIALIZE comparativeDO :: Alphabet Char -> (SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> (SymbolAmbiguityGroup, Word)) -> Vector SymbolContext -> Vector SymbolContext -> (Word, Vector SymbolContext) #-}
comparativeDO
    :: (Foldable f, Indexable f, Key f ~ Int)
    => Alphabet Char
    -> (SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> (SymbolAmbiguityGroup, Word))
    -> f SymbolContext
    -> f SymbolContext
    -> (Word, Vector SymbolContext)
comparativeDO alphabet overlapFunction lhs rhs
    | original /= unboxed = trace rendered unboxed
    | otherwise           = unboxed
    where
        original@(c1, a1) = ukkonenDO alphabet overlapFunction lhs rhs
        unboxed@( c2, a2) = unboxedUkkonenDO alphabet overlapFunction lhs rhs

        rendered          = unlines
            [ "Original, boxed:"
            , ""
            , "  Cost: " <> show c1
            , "  Str:  " <> renderSmartly alphabet a1
            , ""
            , ""
            , "Updated, unboxed:"
            , ""
            , "  Cost: " <> show c2
            , "  Str:  " <> renderSmartly alphabet a2
            , ""
            , ""
    --      , "  " <> show a1
    --      , "  " <> show a2
            , "|" <> renderWith symbolAlignmentLeft a1
            , "|" <> renderWith symbolAlignmentRight a1
            , ""
            , "|" <> renderWith symbolAlignmentLeft a2
            , "|" <> renderWith symbolAlignmentRight a2
            , ""
            , "|" <> renderSmartly alphabet a1
            , "|" <> renderSmartly alphabet a2
            , "|" <> toList (zipWith (\x y -> if x == y then ' ' else '^') a1 a2)
            , ""
            ]

        renderWith :: (SymbolContext -> Maybe SymbolAmbiguityGroup) -> SymbolString -> String
        renderWith f = foldMap toList . interpretAmbiguityAsDNA
            where
                interpretAmbiguityAsDNA :: SymbolString -> [NonEmpty Char]
                interpretAmbiguityAsDNA =
                    encodeIUPAC iupacToDna . fmap (maybe gap getAmbiguityAsDNA . f) . toList

                gap = pure $ gapSymbol dnaAlphabet

                getAmbiguityAsDNA :: SymbolAmbiguityGroup -> NonEmpty Char
                getAmbiguityAsDNA = decodeAmbiguityGroup dnaAlphabet

                dnaAlphabet       = fromSymbols ['A', 'C', 'G', 'T', '-']
