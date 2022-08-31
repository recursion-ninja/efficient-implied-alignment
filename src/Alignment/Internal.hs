-----------------------------------------------------------------------------
-- |
-- Module      :  Alignment.Internal
-- Copyright   :  (c) 2018 Alex Washburn
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# Language FlexibleContexts #-}
{-# Language ImportQualifiedPost #-}
{-# Language TypeFamilies #-}

module Alignment.Internal
    ( PairwiseAlignment
    , postorderLogic
    , preorderInternalLogic
    , preorderLeafLogic
    , preorderRootLogic
    ) where

import Control.Lens
import Data.Decoration
import Data.Foldable
import Data.Key
import Data.List.NonEmpty qualified as NE
import Data.SymbolString
import Data.Vector.NonEmpty hiding (reverse)


-- |
-- A function representing an alignment of two strings.
type PairwiseAlignment s = Vector SymbolContext -> Vector SymbolContext -> (Word, Vector SymbolContext)


-- |
-- The post-order scoring logic for dynamic characters.
--
-- Parameterized over a 'PairwiseAlignment' function to allow for different
-- atomic alignments depending on the character's metadata.
{-# INLINABLE postorderLogic #-}
postorderLogic :: PairwiseAlignment Char -> PreliminaryNode -> PreliminaryNode -> PreliminaryNode
postorderLogic pairwiseAlignment lhs rhs = PreliminaryNode totalSubtreeCost alignmentCost alignmentContext
    where
        totalSubtreeCost = alignmentCost + lhs ^. subtreeCost + rhs ^. subtreeCost
        (alignmentCost, alignmentContext) =
            pairwiseAlignment (lhs ^. preliminaryString) (rhs ^. preliminaryString)


-- |
-- The pre-order scoring logic for root node.
{-# INLINABLE preorderRootLogic #-}
preorderRootLogic :: PreliminaryNode -> FinalizedNode
preorderRootLogic =
    FinalizedNode
        <$> (^. subtreeCost)
        <*> (^. localCost)
        <*> (^. preliminaryString)
        <*> (^. preliminaryString)
        <*> const True


-- |
-- The pre-order scoring logic for intenral nodes.
{-# INLINABLE preorderLeafLogic #-}
preorderLeafLogic
    :: FinalizedNode                          -- ^ Parent decoration
    -> Either PreliminaryNode PreliminaryNode -- ^ Current decoration, whether it is Left or Right child of parent.
    -> FinalizedNode                          -- ^ Updated decoration
preorderLeafLogic parent current =
    (   FinalizedNode
        <$> (^. subtreeCost)
        <*> (^. localCost)
        <*> (^. preliminaryString)
        <*> const derivedStringAlignment
        <*> const False
        )
        $ either id id current
    where
        (c, p, a) = case current of
            Left  x -> (x ^. preliminaryString, r <$> parent ^. preliminaryString, parent ^. alignedString)
            Right x -> (x ^. preliminaryString, parent ^. preliminaryString, parent ^. alignedString)

        r                      = reverseContext

        derivedStringAlignment = deriveAlignment a p c


-- |
-- The pre-order scoring logic for intenral nodes.
{-# INLINABLE preorderInternalLogic #-}
preorderInternalLogic
    :: FinalizedNode                          -- ^ Parent decoration
    -> Either PreliminaryNode PreliminaryNode -- ^ Current decoration, whether it is Left or Right child of parent.
    -> FinalizedNode                          -- ^ Updated decoration
preorderInternalLogic parent current =
    (   FinalizedNode
        <$> (^. subtreeCost)
        <*> (^. localCost)
        <*> (^. preliminaryString)
        <*> const derivedStringAlignment
        <*> const False
        )
        $ either id id current
    where
        derivedStringAlignment = deriveAlignment (parent ^. alignedString) p c

        (c, p)                 = case current of
            Left  x -> (x ^. preliminaryString, reverseContext <$> parent ^. preliminaryString)
            Right x -> (x ^. preliminaryString, parent ^. preliminaryString)


{-# INLINABLE deriveAlignment #-}
deriveAlignment
    :: SymbolString -- ^ Parent Alignment
    -> SymbolString -- ^ Parent Context
    -> SymbolString -- ^ Child  Context
    -> SymbolString -- ^ Child  Alignment
deriveAlignment pAlignment pContext cContext = alignment
    where
        alignment =
            extractVector {-- . traceResult --}
                          $ foldlWithKey f ([], toList cContext, toList pContext) pAlignment

        extractVector e@(x, ys, zs) = case (ys, zs) of
            ([], []) -> fromNonEmpty . NE.fromList $ reverse x
            _        -> error $ unlines
                [ "The impossible happened! (after the ``sliding zip'')"
                , "> The parent and/or child context(s) were not fully consumed."
                , ""
                , renderResult e
                ]

    --    traceResult e = trace (renderResult e) e

        renderResult (x, y, z) = ("\n" <>) $ unlines
            [ "While deriving INTERNAL label"
            , "A & I in parent alignment: " <> show (countAlignInsert pAlignment)
            , "Length parent context:     " <> show (length pContext)
            , if countAlignInsert pAlignment == length pContext
                then "Parental alignments valid"
                else "-=-=-=-=-=- INVALID ALIGNMENTS -=-=-=-=-=-"
            , "Parent Alignment: " <> show pAlignment
            , "Parent Context:   " <> show pContext
            , "Child  Context:   " <> show cContext
            , "Parent Context Renamining: "
            , show z
            , "Child Context Renamining: "
            , show y
            , "Result Alignment: "
            , show $ reverse x
            ]

        f
            :: ([SymbolContext], [SymbolContext], [SymbolContext])
            -> Int
            -> SymbolContext
            -> ([SymbolContext], [SymbolContext], [SymbolContext])
        f z@(acc, [], _) k e = case e of
            Gapping{}  -> (e : acc, [], [])
            Delete _ v -> (Gapping v : acc, [], [])
            Insert _ v -> (Gapping v : acc, [], [])
            _          -> error $ unlines
                [ "The impossible happened!"
                , "> Align encountered after child contexts consumed."
                , "  index " <> show k <> "/" <> show (length pAlignment - 1) <> ": " <> show e
                , ""
                , renderResult z
                ]

        f z@(_, _ : _, []) k e = error $ unlines
            [ "The impossible happened!"
            , "> The parent context was consumed before the child context."
            , "  index " <> show k <> "/" <> show (length pAlignment - 1) <> ": " <> show e
            , ""
            , renderResult z
            ]

        f (acc, x : xs, y : ys) _ e = case e of
            Gapping{}  -> (e : acc, x : xs, y : ys)
            Align{}    -> (x : acc, xs, ys)
            Delete _ v -> case y of
                Delete{} -> (Gapping v : acc, x : xs, ys)
                _        -> (x : acc, xs, ys)

            Insert _ v -> case y of
                Insert{} -> (x : acc, xs, ys)
                _        -> (Gapping v : acc, x : xs, ys)


{-# INLINABLE countAlignInsert #-}
{-# SPECIALIZE countAlignInsert :: Vector SymbolContext -> Int #-}
countAlignInsert :: (Functor f, Foldable f) => f SymbolContext -> Int
countAlignInsert = sum . fmap g
    where
        g :: Num a => SymbolContext -> a
        g Delete{} = 0
        g _        = 1
