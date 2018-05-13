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

module Alignment.Internal where

import           Alignment.Pairwise
import           Control.Lens
import           Data.Decoration
import           Data.Foldable
import           Data.IntMap                 (IntMap)
import qualified Data.IntMap          as IM
import           Data.Key
import           Data.List.NonEmpty          (NonEmpty( (:|) ))
import qualified Data.List.NonEmpty   as NE
import           Data.List.Utility           (invariantTransformation)
import           Data.Maybe
import           Data.Pointed
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.SymbolString
import           Data.TCM
import           Data.MonoTraversable hiding (headMay)
import           Data.Vector.NonEmpty hiding (reverse)
import           Data.Word
import           Numeric.Extended.Natural
import           Prelude              hiding (lookup, zipWith)
import           Safe


-- |
-- A function representing an alignment of two strings.
type PairwiseAlignment s =  Vector (SymbolContext s)
                         -> Vector (SymbolContext s)
                         -> (Word, Vector (SymbolContext s))


-- |
-- The post-order scoring logic for dynamic characters.
--
-- Parameterized over a 'PairwiseAlignment' function to allow for different
-- atomic alignments depending on the character's metadata.
postorderLogic
  :: PairwiseAlignment String
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
      <*> deriveFinalizedString . (^. preliminaryString)
      <*> (^. preliminaryString)
  where
    deriveFinalizedString = toVector . foldMap f

--    deriveStringAlignment = foldMap1 g
    
    gapGroup = point "-"

    f x
      | gapGroup == v = []
      | otherwise     = [v]
      where
        v = symbolAlignmentMedian x

--    g = pure . symbolAlignmentMedian

    toVector = fromNonEmpty . NE.fromList


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
      <*> fmap symbolAlignmentMedian . (^. preliminaryString)
      <*> const undefined -- localAlignedStrings
    ) $ either id id current
  where
    gap    = point "-"
     
    (c, p) = case current of
               Left  x -> (x ^. preliminaryString,                    parent ^. preliminaryString)
               Right x -> (x ^. preliminaryString, reverseContext <$> parent ^. preliminaryString)

    localAlignedStrings :: Vector (SymbolAmbiguityGroup String)
    localAlignedStrings = fromNonEmpty . NE.fromList . snd $ foldl' f (toList c, []) p
      where
        f (ss, acc) e =
            case e of
              Delete {} -> ([], gap : acc)
              _         ->
                  case ss of
                    []   -> ([], point "?" : acc) -- error "Cannot Align or Insert when there is no child symbol."
                    x:xs -> (xs, symbolAlignmentMedian x : acc)


-- |
-- The pre-order scoring logic for intenral nodes.
preorderInternalLogic
  :: ThreewayCompare String
  -> FinalizedInternalNode                          -- ^ Parent decoration
  -> Either InitialInternalNode InitialInternalNode -- ^ Current decoration, whether it is Left or Right child of parent.
  -> FinalizedInternalNode                          -- ^ Updated decoration
preorderInternalLogic sigma parent current =
    ( FinalizedInternalNode
      <$> (^. subtreeCost)
      <*> (^. localCost)
      <*> (^. preliminaryString)
      <*> const finalSymbols
      <*> const derivedStringAlignment
    ) $ either id id current
  where
    finalSymbols :: Vector (SymbolAmbiguityGroup String)
    finalSymbols = foldMap1 (\(a,b,c) -> pure . fst $ sigma a b c) alignedSurroundingStrings

    derivedStringAlignment = deriveAlignment (parent ^. alignedString) p c
      
    gap    = point "-"
     
    (c, p) = case current of
               Left  x -> (x ^. preliminaryString, reverseContext <$> parent ^. preliminaryString)
               Right x -> (x ^. preliminaryString,                    parent ^. preliminaryString)

    alignedSurroundingStrings :: NonEmpty (SymbolAmbiguityGroup String, SymbolAmbiguityGroup String, SymbolAmbiguityGroup String)
    alignedSurroundingStrings = NE.fromList . reverse . (\(_,_,x) -> x) $ foldl' f (0, toList c, []) p
      where
        f (i,  [], acc) e =
            case e of
              Delete _ m _   -> (i+1, [], (m, gap, gap) : acc)
              _              -> error $ unlines
                                  [ "Cannot Align or Insert when there is no child symbol:"
                                  , "at index " <> show i <> ": " <> show e
                                  , "Parent: " <> show p
                                  , "Child:  " <> show c
                                  ]
        f (i, x:xs, acc) e =
            case e of
              Delete _ m _   ->     (i+1, x:xs, (  m, gap, gap) : acc)
              Insert _ m   _ -> let (lhs, rhs) = getLhsRhs x
                                in  (i+1,   xs, (gap, lhs, rhs) : acc)
              Align  _ m _ _ -> let (lhs, rhs) = getLhsRhs x
                                in  (i+1,   xs, (  m, lhs, rhs) : acc)

    getLhsRhs x = case x of
                    Align  _ _ a b -> (  a,   b)
                    Delete _ _ a   -> (  a, gap)
                    Insert _ _   b -> (gap,   b)


{-
     -- |
     -- The length of first argument is /less than or equal to/ the length of
     -- the second argument. 
     aligner
       :: SymbolString
       -> Vector (SymbolAmbiguityGroup String)
-       -> Vector (SymbolAmbiguityGroup String)
     aligner lhs rhs = undefined
       where
         (_,_,result) = foldl' f (0 :: Int, toList rhs, []) $ toList lhs

         f (basesSeen, xs, ys) e
           | isGap e   = (basesSeen    , xs , gapGroup : ys )
           | otherwise = (basesSeen + 1, xs',            ys')
           where
             gapGroup = point "-"
             isGap x  = symbolAlignmentMedian x == gapGroup
             xs'      = fromMaybe []   $ tailMay xs
             ys'      = maybe ys (:ys) $ headMay xs
-}

{-
  (_,_remaining,result)    = foldl' f (0 :: Int, characterTokens, []) psuedoCharacterVal
    where
      f (basesSeen, xs, ys) e
        | isPseudoGap e = (basesSeen    , xs , gap : ys )
        | otherwise     = (basesSeen + 1, xs',       ys')
        where
          xs' = fromMaybe []   $ tailMay xs
          ys' = maybe ys (:ys) $ headMay xs
-}



deriveAlignment
  :: SymbolString -- ^ Parent Alignment
  -> SymbolString -- ^ Parent Context
  -> SymbolString -- ^ Child Context
  -> SymbolString -- ^ Child Alignment
deriveAlignment pAlignment pContext cContext = alignment
  where
    alignment = extractVector $ foldlWithKey f ([], toList cContext, toList pContext) pAlignment

    extractVector (x,_,_) = fromNonEmpty . NE.fromList $ reverse x

    gap = point "-"

    f :: ([SymbolContext String], [SymbolContext String], [SymbolContext String]) -> Int -> SymbolContext String -> ([SymbolContext String], [SymbolContext String], [SymbolContext String])
    f (acc, [], []) k e =
        case e of
          Delete _ m _ -> (Delete 0 gap gap : acc, [], [])
          _            -> error $ unlines
                              [ "Cannot Align or Insert when there is no child symbol:"
                              , "at index " <> show k <> ": " <> show e
                              , "Parent Alignment: " <> show pAlignment
                              , "Parent Context:   " <> show pContext
                              , "Child  Context:   " <> show cContext
                              ]
    f (acc, [], y:ys) k e =
        case e of
          Delete _ _ _   -> (Delete 0 gap gap : acc, [], [])
          Insert _ _   v ->
            if v == gap
            then (Delete 0 gap gap : acc, [], y:ys)
            else case y of
                   Delete {} -> (Delete 0 gap gap : acc,  [], ys)
                   _         -> error "SAD!"
          Align  _ _ _ v ->
            case y of
              Delete {}  -> (Delete 0 gap gap : acc, [], ys)
              _          -> error "BAD!"

    f (acc, x:xs, []) k e = error "MAD!"

    f (acc, x:xs, y:ys) k e =
        case e of
          Delete _ _ _   -> (Delete 0 gap gap : acc, x:xs, y:ys)
          Insert _ v   _ ->
            if v == gap
            then (Delete 0 gap gap : acc, x:xs, y:ys)
            else case y of
                   Delete {} -> (Delete 0 gap gap : acc,  x:xs, ys)
                   _         -> (               x : acc,    xs, ys)
          Align  _ v _ _ ->
            if v == gap
            then (Delete 0 gap gap : acc, x:xs, y:ys)
            else case y of
                   Delete {} -> (Delete 0 gap gap : acc,  x:xs, ys)
                   _         -> (               x : acc,    xs, ys)



{-
-- |
-- The pre-order scoring logic for dynamic characters.
--
-- Parameterized over a 'PairwiseAlignment' function to allow for different
-- atomic alignments depending on the character's metadata.
directOptimizationPreOrder
  :: ( DirectOptimizationPostOrderDecoration d c
     )
  => PairwiseAlignment c
  -> d
  -> [(Word, DynamicDecorationDirectOptimization c)]
  ->  DynamicDecorationDirectOptimization c
directOptimizationPreOrder pairwiseAlignment charDecoration parents =
    case parents of
        []            -> initializeRoot charDecoration
        (_, parent):_ -> updateFromParent pairwiseAlignment charDecoration parent


-- |
-- Given a post-order traversal result of a dynamic character as input,
-- initializes the root node decoration as the base case of the pre-order
-- traversal.
initializeRoot
  :: DirectOptimizationPostOrderDecoration d c
  => d
  -> DynamicDecorationDirectOptimization c
initializeRoot =
    extendPostOrderToDirectOptimization
      <$> id
      <*> (^. preliminaryUngapped)
      <*> (^. preliminaryGapped)
      <*> lexicallyDisambiguate . (^. preliminaryUngapped)


-- |
-- Disambiguate the elements of a dynamic character using only lexical ordering
-- of the alphabet.
lexicallyDisambiguate :: (MonoFunctor f, FiniteBits (Element f)) => f -> f
lexicallyDisambiguate = omap disambiguateElement


-- |
-- Disambiguate a single element of a Dynamic Character.
disambiguateElement :: FiniteBits b => b -> b
disambiguateElement x = zed `setBit` idx
  where
    idx = countLeadingZeros x
    zed = x `xor` x


-- |
-- Disambiguate the elements of a dynamic Character so that they are consistent
-- with the ancestral disambiguation.
disambiguateFromParent
  :: EncodableDynamicCharacter c
  => c -- ^ parent single disambiguation field
  -> c -- ^ child  final gapped
  -> c -- ^ child  single disambiguation field
disambiguateFromParent {- pGaps cGaps -} pSingle cFinal = result
  where
    result = constructDynamic $ zipWith f (otoList pSingle) (otoList cFinal)
    f pS cF
      | popCount val /= 0 = val
      | otherwise         = disambiguateElement cF
      where
        -- Since pS will have only one bit set,
        -- there can only ever be an symbol intersection of size 1
        val = pS .&. cF


-- |
-- Use the decoration(s) of the ancestral nodes to calculate the corrent node
-- decoration. The recursive logic of the pre-order traversal.
updateFromParent
  :: ( DirectOptimizationPostOrderDecoration d c
     , Exportable (Element c)
     -- , EncodedAmbiguityGroupContainer c
     )
  => PairwiseAlignment c
  -> d
  -> DynamicDecorationDirectOptimization c
  -> DynamicDecorationDirectOptimization c
updateFromParent pairwiseAlignment currentDecoration parentDecoration = resultDecoration
  where
    -- If the current node has a missing character value representing its
    -- preliminary median assignment then we take the parent's final assignment
    -- values and assign them to the current node as its own final assignments.
    --
    -- Otherwise we perform a local alignment between the parent's *UNGAPPED*
    -- final assignment and the current node's *GAPPED* preliminary assignment.
    -- Afterward we calculate the indices of the new gaps in the alignment and
    -- insert these gaps into the current node's left and right child alignments.
    -- Lastly, a three-way mean between the locally-aligned parent assignment and
    -- the expanded left and right child alignments is used to calculate the
    -- final assignment of the current node.
    --
    -- We do these convoluted operations to account for deletion events in the
    -- parent assignment when comparing to child assignments.
    resultDecoration = extendPostOrderToDirectOptimization currentDecoration ungapped gapped single
    (ungapped, gapped, single)
      | isMissing $ currentDecoration ^. preliminaryGapped = (pUngapped, pGapped, pSingle)
      | otherwise = tripleComparison pairwiseAlignment currentDecoration pUngapped pSingle
    pUngapped     = parentDecoration ^. finalUngapped
    pGapped       = parentDecoration ^. finalGapped
    pSingle       = parentDecoration ^. singleDisambiguation


-- |
-- A three way comparison of characters used in the DO preorder traversal.
tripleComparison
  :: ( Exportable (Element c)
     -- , EncodedAmbiguityGroupContainer c
     , DirectOptimizationPostOrderDecoration d c
     )
  => PairwiseAlignment c
  -> d
  -> c
  -> c
  -> (c, c, c)
tripleComparison pairwiseAlignment childDecoration parentCharacter parentSingle =
   {-  trace context () `seq` -} (ungapped, gapped, single)
  where
    childCharacter    = childDecoration ^. preliminaryGapped
    childLeftAligned  = childDecoration ^. leftAlignment
    childRightAligned = childDecoration ^. rightAlignment

    -- We conditionally decide how to derive the metric.
    -- If we are working with large alphabets we use the memoized TCM.
    -- Otherwise we use the naive calculations.
    --
    -- We do this so that we don't allocate and begin using a memoized TCM
    -- for all characters regardless of alphabet size on the pre-order.
    -- If we have a small alphabet, there will not have been a call to
    -- initialize a memoized TCM. We certainly don't want to force that here!
    costStructure =
        case childDecoration ^. denseTransitionCostMatrix of
          Nothing -> getMedianAndCost3D (childDecoration ^. sparseTransitionCostMatrix)
          -- Compute things naively
          Just _  -> naiveMedianAndCost3D
      where
        !scm = childDecoration ^. symbolChangeMatrix
        gap = gapOfStream parentCharacter
        zed = gap `xor` gap
        singletonStates = (zed `setBit`) <$> [0 .. fromEnum (symbolCount zed) - 1]
        naiveMedianAndCost3D a b c = unsafeToFinite <$> foldl' g (zed, infinity :: ExtendedNatural) singletonStates
          where
            g acc@(combinedState, curentMinCost) singleState =
                case combinedCost `compare` curentMinCost of
                  EQ -> (combinedState .|. singleState, curentMinCost)
                  LT -> (                  singleState, combinedCost)
                  GT -> acc
              where
                combinedCost = fromFinite . sum $ snd . overlap scm singleState <$> [a, b, c]


    single = lexicallyDisambiguate $ filterGaps almostSingle
    (_, ungapped, gapped)  = threeWayMean costStructure extendedParentFinal  extendedLeftCharacter1 extendedRightCharacter1
    (_, almostSingle, _)   = threeWayMean costStructure extendedParentSingle extendedLeftCharacter2 extendedRightCharacter2

    (extendedParentFinal , extendedLeftCharacter1, extendedRightCharacter1) = alignAroundCurrentNode pairwiseAlignment childCharacter parentCharacter childLeftAligned childRightAligned
    (extendedParentSingle, extendedLeftCharacter2, extendedRightCharacter2) = alignAroundCurrentNode pairwiseAlignment childCharacter parentSingle    childLeftAligned childRightAligned

    {-
    context = unlines
        [ ""
        , "Center char (prelim/final/single):"
        , showStream alph childCharacter
        , showStream alph ungapped
        , showStream alph single
--        , showStream alph childAlignment
        , ""
        , "Parent Final Char:"
        , showStream alph parentCharacter
--        , showStream alph parentAlignment
        , mconcat [showStream alph extendedParentFinal, " (", show (olength extendedParentFinal), ")"]
        , "Left  chars:"
        , mconcat [showStream alph childLeftAligned, " (", show (olength childLeftAligned), ")"]
        , mconcat [showStream alph extendedLeftCharacter1, " (", show (olength extendedLeftCharacter1), ")"]
        , "Right chars:"
        , mconcat [showStream alph childRightAligned, " (", show (olength childRightAligned), ")"]
        , mconcat [showStream alph extendedRightCharacter1, " (", show (olength extendedRightCharacter1), ")"]
        , ""
        , "Parent Single char:"
        , showStream alph parentSingle
--        , showStream alph singleAlignment
        , mconcat [showStream alph extendedParentSingle, " (", show (olength extendedParentSingle), ")"]
        , "Left  chars:"
        , mconcat [showStream alph childLeftAligned, " (", show (olength childLeftAligned), ")"]
        , mconcat [showStream alph extendedLeftCharacter2, " (", show (olength extendedLeftCharacter2), ")"]
        , "Right chars:"
        , mconcat [showStream alph childRightAligned, " (", show (olength childRightAligned), ")"]
        , mconcat [showStream alph extendedRightCharacter2, " (", show (olength extendedRightCharacter2), ")"]
        ]
      where
        alph = childDecoration ^. characterAlphabet
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
