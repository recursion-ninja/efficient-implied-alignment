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

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Alignment.Internal
  ( postorderLogic
  , preorderInternalLogic
  , preorderLeafLogic
  , preorderRootLogic
  ) where

import           Control.Lens
import           Data.Bits
import           Data.Decoration
import           Data.Foldable
import           Data.Key
import qualified Data.List.NonEmpty   as NE
import           Data.SymbolString
import           Data.Vector.NonEmpty hiding (reverse)

--import Debug.Trace


-- |
-- A function representing an alignment of two strings.
type PairwiseAlignment s =  Vector SymbolContext
                         -> Vector SymbolContext
                         -> (Word, Vector SymbolContext)


-- |
-- The post-order scoring logic for dynamic characters.
--
-- Parameterized over a 'PairwiseAlignment' function to allow for different
-- atomic alignments depending on the character's metadata.
postorderLogic
  :: PairwiseAlignment Char
  -> InitialInternalNode
  -> InitialInternalNode
  -> InitialInternalNode
postorderLogic pairwiseAlignment lhs rhs =
    InitialInternalNode totalSubtreeCost alignmentCost alignmentContext
  where
    totalSubtreeCost = alignmentCost + lhs ^. subtreeCost + rhs ^. subtreeCost
    (alignmentCost, alignmentContext) = pairwiseAlignment
                                          (lhs ^. preliminaryString)
                                          (rhs ^. preliminaryString)


-- |
-- The pre-order scoring logic for root node.
preorderRootLogic
  :: InitialInternalNode
  -> FinalizedInternalNode
preorderRootLogic =
    FinalizedInternalNode
      <$> (^. subtreeCost)
      <*> (^. localCost)
      <*> (^. preliminaryString)
      <*> setInitialAlignment   . (^. preliminaryString)
      <*> const True


-- |
-- The pre-order scoring logic for intenral nodes.
preorderLeafLogic
  :: FinalizedInternalNode                          -- ^ Parent decoration
  -> Either InitialInternalNode InitialInternalNode -- ^ Current decoration, whether it is Left or Right child of parent.
  -> FinalizedInternalNode                          -- ^ Updated decoration
preorderLeafLogic parent current =
    ( FinalizedInternalNode
      <$> (^. subtreeCost)
      <*> (^. localCost)
      <*> (^. preliminaryString)
      <*> const derivedStringAlignment
      <*> const False
    ) $ either id id current
  where
    (c, p, a) =
        case current of
          Left  x -> (x ^. preliminaryString, r <$> parent ^. preliminaryString, parent ^. alignedString)
          Right x -> (x ^. preliminaryString,       parent ^. preliminaryString, parent ^. alignedString)

    r = reverseContext

    derivedStringAlignment = deriveLeafAlignment a p c


-- |
-- The pre-order scoring logic for intenral nodes.
preorderInternalLogic
  :: FinalizedInternalNode                          -- ^ Parent decoration
  -> Either InitialInternalNode InitialInternalNode -- ^ Current decoration, whether it is Left or Right child of parent.
  -> FinalizedInternalNode                          -- ^ Updated decoration
preorderInternalLogic parent current =
    ( FinalizedInternalNode
      <$> (^. subtreeCost)
      <*> (^. localCost)
      <*> (^. preliminaryString)
      <*> const derivedStringAlignment
      <*> const False
    ) $ either id id current
  where
    derivedStringAlignment = deriveAlignment (parent ^. alignedString) p c
      
    (c, p) = case current of
               Left  x -> (x ^. preliminaryString, reverseContext <$> parent ^. preliminaryString)
               Right x -> (x ^. preliminaryString,                    parent ^. preliminaryString)


-- TODO: Make this Delete gap gap, don't assum alphabet of size 5
del :: SymbolContext
del = let x = bit 4 in Delete x x 


deriveAlignment
  :: SymbolString -- ^ Parent Alignment
  -> SymbolString -- ^ Parent Context
  -> SymbolString -- ^ Child Context
  -> SymbolString -- ^ Child Alignment
deriveAlignment pAlignment pContext cContext = alignment
  where
    alignment = extractVector {-- . traceResult --} $ foldlWithKey f ([], toList cContext, toList pContext) pAlignment

    extractVector e@ (x,ys,zs) =
        case (ys, zs) of
          ([],[]) -> fromNonEmpty . NE.fromList $ reverse x
          _       -> error $ renderResult e

--    traceResult e = trace (renderResult e) e

    renderResult (x,y,z) = ("\n"<>) $ unlines
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

--    gap = point '-'

    f :: ([SymbolContext], [SymbolContext], [SymbolContext]) -> Int -> SymbolContext -> ([SymbolContext], [SymbolContext], [SymbolContext])
    f (acc, [], []) k e =
        case e of
          Delete {} -> (del : acc, [], [])
          _         -> error $ unlines
                           [ "While deriving INTERNAL label"
                           , "Cannot Align or Insert when there is no child symbol:"
                           , "at index " <> show k <> "/" <> show (length pAlignment - 1) <> ": " <> show e
                           , "Parent Alignment: " <> show pAlignment
                           , "Parent Context:   " <> show pContext
                           , "Child  Context:   " <> show cContext
                           ]
    f z@(acc, [], y:ys) k e =
        case e of
          Delete {} -> (del : acc, [], y:ys)
          Insert {} -> (del : acc, [],   ys)
--            if v == gap
--            then (Delete 0 gap gap : acc, [], y:ys)
--            else case y of
--                   Delete {} -> (Delete 0 gap gap : acc,  [], ys)
--                   _         -> error "SAD!"
          Align  {} ->
            case y of
              Delete {}  -> (del : acc, [], ys)
              _          -> error $ unlines
                                [ "BAD!"
                                , "at index " <> show k <> ": " <> show e
                                , "Parent Alignment: " <> show pAlignment
                                , "Parent Context:   " <> show pContext
                                , "Child  Context:   " <> show cContext
                                , renderResult z
                                ]

    f (_, _:_, []) _ _ = error "MAD!"

    f (acc, x:xs, y:ys) _ e =
        case e of
          Delete {}  ->
            case y of
              Delete {} -> (del : acc, x:xs,  y:ys)
              _         -> (del : acc, x:xs,  y:ys)
          Insert {} -> -- (               x : acc,    xs, ys)
              case y of
                Delete {} -> (                  del : acc, x:xs, ys)
                Insert {} -> (deleteionToInserion x : acc,   xs, ys)
                Align  {} -> (                    x : acc,   xs, ys)
--            if v == gap
--            then (Delete 0 gap gap : acc, x:xs, y:ys)
--            else case y of
--                   Delete {} -> (Delete 0 gap gap : acc,  x:xs, ys)
--                   _         -> (               x : acc,    xs, ys)
          Align  {} -> -- (               x : acc,    xs, ys)
              case y of
                Delete {} -> (del : acc, x:xs, ys)
                Insert {} -> (  x : acc,   xs, ys)
                Align  {} -> (  y : acc,   xs, ys)
--            if v == gap
--            then (Delete 0 gap gap : acc, x:xs, y:ys)
--            else case y of
--                 Delete {} -> (Delete 0 gap gap : acc,  x:xs, ys)
--                 _         -> (               x : acc,    xs, ys)



countAlignInsert :: (Functor f, Foldable f) => f SymbolContext -> Int
countAlignInsert = sum . fmap g
  where
    g Delete {} = 0
    g _           = 1


setInitialAlignment :: Functor f => f SymbolContext -> f SymbolContext
setInitialAlignment = fmap deleteionToInserion


deleteionToInserion :: SymbolContext -> SymbolContext
deleteionToInserion e@Delete {} = reverseContext e
deleteionToInserion e           = e


deriveLeafAlignment
  :: SymbolString -- ^ Parent Alignment
  -> SymbolString -- ^ Parent Context
  -> SymbolString -- ^ Child Context
  -> SymbolString -- ^ Child Alignment
deriveLeafAlignment pAlignment pContext cContext = alignment
  where
    alignment = extractVector {-- . traceResult --} $ foldlWithKey f ([], toList cContext, toList pContext) pAlignment

    extractVector e@ (x,ys,zs) =
        case (ys, zs) of
          ([],[]) -> fromNonEmpty . NE.fromList $ reverse x
          _       -> error $ renderResult e


{---}
--    traceResult e = trace (renderResult e) e

    renderResult (x,y,z) = ("\n"<>) $ unlines
        [ "While deriving LEAF label"
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
{--}

    f (acc, [], []) k e =
        case e of
          Delete {} -> (del : acc, [], [])
          Insert {} -> (del : acc, [], [])
          _         -> error $ unlines
                           [ "While deriving LEAF label"
                           , "Cannot Align when there is no child symbol:"
                           , "at index " <> show k <> "/" <> show (length pAlignment - 1) <> ": " <> show e
                           , "Parent Alignment: " <> show pAlignment
                           , "Parent Context:   " <> show pContext
                           , "Child  Context:   " <> show cContext
                           ]

    f z@(acc, [], y:ys) _ e =
        case e of
          Delete {} -> (del : acc, [], y:ys)
          Insert {} -> (del : acc, [],   ys)
--            if v == gap
--            then (Delete 0 gap gap : acc, [], y:ys)
--            else case y of
--                   Delete {} -> (Delete 0 gap gap : acc,  [], ys)
--                   _         -> error "SAD!"
          Align  {} ->
              case y of
                Insert {} -> (del : acc, [], ys)
                Delete {} -> (del : acc, [], ys)
                _         -> error $ "SAD!\n" <> renderResult z
{-
              _          -> error $ unlines
                                [ "BAD!"
                                , "at index " <> show k <> ": " <> show e
                                , "Parent Alignment: " <> show pAlignment
                                , "Parent Context:   " <> show pContext
                                , "Child  Context:   " <> show cContext
                                , renderResult z
                                ]
-}

    f (_, _:_, []) _ _ = error "MAD!"

    f (acc, x:xs, y:ys) _ e =
        case e of
          Delete {} -> (del : acc, x:xs, y:ys)
          Insert {} -> -- (               x : acc,    xs, ys)
              case y of
                Delete {} -> (del : acc,  x:xs, ys)
                Insert _ v  ->
                  if v == symbolAlignmentMedian x
                  then (  x : acc,    xs, ys)
                  else (del : acc,  x:xs, ys)
                Align  {} -> (x : acc,    xs, ys)
--            if v == gap
--            then (Delete 0 gap gap : acc, x:xs, y:ys)
--            else case y of
--                   Delete {} -> (Delete 0 gap gap : acc,  x:xs, ys)
--                   _         -> (               x : acc,    xs, ys)
          Align {} -> -- (               x : acc,    xs, ys)
              case y of
                Delete {} -> (del : acc, x:xs, ys)
                Insert {} -> (  x : acc,   xs, ys)
                Align  {} -> (  x : acc,   xs, ys)
--            if v == gap
--            then (Delete 0 gap gap : acc, x:xs, y:ys)
--            else case y of
--                 Delete {} -> (Delete 0 gap gap : acc,  x:xs, ys)
--                 _         -> (               x : acc,    xs, ys)



{-
    -}


-- |
-- Given a node, its parent, and its children; this function aligns the dynamic
-- characters around the current node.
alignAroundCurrentNode
  :: EncodableDynamicCharacter c
  => PairwiseAlignment c
  -> c -- ^ local character
  -> c -- ^ parent character
  -> c -- ^ one child character
  -> c -- ^ other child character
  -> (c, c, c) -- ^ parent & child characters aligned with respect to the current node
alignAroundCurrentNode pairwiseAlignment current parent child1 child2 =
    (extendedParent, extendedChild1, extendedChild2)
  where
    (_, _, _, parentAlignment, currentAlignment) = pairwiseAlignment parent current

    newGapIndiciesInParent  = newGapLocations parent  parentAlignment
    newGapIndiciesInCurrent = newGapLocations current currentAlignment

    extendedParent = insertNewGaps newGapIndiciesInParent  parent
    extendedChild1 = insertNewGaps newGapIndiciesInCurrent child1
    extendedChild2 = insertNewGaps newGapIndiciesInCurrent child2


-- |
-- Returns the indices of the gaps that were added in the second character when
-- compared to the first character.
newGapLocations :: EncodableDynamicCharacter c => c -> c -> IntMap Int
newGapLocations unaligned aligned
  | olength unaligned == olength aligned = mempty
  | otherwise                            = newGaps
  where
    (_, _, newGaps)  = ofoldl' f accumulator aligned
    accumulator      = (otoList unaligned, 0, mempty)
    gap              = gapOfStream unaligned
    incrementAt is i = IM.insertWith (+) i 1 is

    f (remainingUnalignedElements, unalignedIndex, newGapIndices) alignedElement =
        case remainingUnalignedElements of

          -- In the case that the unaligned input character has had all of its
          -- elements accounted for, we can determine if a deletion event happened
          -- by simply checking whether the remaining element from the aligned
          -- character is a gap character.
          []   -> ( remainingUnalignedElements
                  , unalignedIndex
                  , if   alignedElement == gap
                    then incrementedGapIndices
                    else newGapIndices
                  )

          -- In the case that the unaligned character has one or more elements
          -- that have not been accounted for in the alignment, we use standard
          -- logic for determining if a deletion event occurred.
          --
          -- If a deletion event *DID* occur, we note the index in the unaligned
          -- character where deletion events occurred and *DO NOT* advance the
          -- "cursor" in our accumulator.
          --
          -- If a deletion event *DID NOT* occur, we just advance the "cursor"
          -- in our accumulator.
          unalignedElement:tailUnalignedElements ->
              if   unalignedElement /= gap && alignedElement == gap -- Deletion Event Occured!
              then (remainingUnalignedElements, unalignedIndex    , incrementedGapIndices)
              else (     tailUnalignedElements, unalignedIndex + 1,         newGapIndices)
      where
        incrementedGapIndices = newGapIndices `incrementAt` unalignedIndex


-- |
-- Given a list of gap locations and a character, returns a longer character with
-- the supplied gaps inserted at the corresponding locations.
insertNewGaps :: EncodableDynamicCharacter c => IntMap Int -> c -> c
insertNewGaps insertionIndicies character = constructDynamic . (<> trailingGaps) . foldMapWithKey f $ otoList character
  where
    len = olength character
    gap = getGapElement $ character `indexStream` 0
    trailingGaps = maybe [] (`replicate` gap) $ len `lookup` insertionIndicies
    f i e =
      case i `lookup` insertionIndicies of
        Nothing -> [e]
        Just n  -> replicate n gap <> [e]


-- |
-- Calculates the mean character and cost between three supplied characters.
threeWayMean
  :: ( EncodableDynamicCharacter c
     -- , EncodedAmbiguityGroupContainer c
     )
  => (Element c -> Element c -> Element c -> (Element c, Word))
  -> c
  -> c
  -> c
  -> (Word, c, c)
threeWayMean sigma char1 char2 char3 =
  case invariantTransformation olength [char1, char2, char3] of
    Nothing -> error $ unwords [ "Three sequences supplied to 'threeWayMean' function did not have uniform length.", show (olength char1), show (olength char2), show (olength char3) ]
    Just 0  -> (0, char1, char1)
    Just _  -> (unsafeToFinite $ sum costValues, constructDynamic $ filter (/= gap) meanStates, constructDynamic meanStates)
  where
    gap = gapOfStream char1
    -- zed = gap `xor` gap
    -- singletonStates = (zed `setBit`) <$> [0 .. fromEnum (symbolCount char1) - 1]
    (meanStates, costValues) = unzip $ zipWith3 sigma (otoList char1) (otoList char2) (otoList char3)
    {-
    f a b c = foldl' g (zed, infinity :: ExtendedNatural) singletonStates
      where
        g acc@(combinedState, curentMinCost) singleState =
            case combinedCost `compare` curentMinCost of
              EQ -> (combinedState .|. singleState, curentMinCost)
              LT -> (                  singleState, combinedCost)
              GT -> acc
          where
            combinedCost = fromFinite . sum $ (snd . sigma singleState) <$> [a, b, c]
    -}
{-
f a b c = minimalChoice $
              sigma a b  :|
            [ sigma a c
            , sigma b c
            ]
-}
-}
