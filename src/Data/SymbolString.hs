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

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedSums                #-}

module Data.SymbolString
  ( SymbolAmbiguityGroup(..)
  , SymbolContext(..)
  , SymbolString()
  , (/\)
  , encodeAmbiguityGroup
  , decodeAmbiguityGroup
  , filterGaps
  , reverseContext
  , symbolAlignmentMedian
  , symbolAlignmentLeft
  , symbolAlignmentRight
  , renderAligns
  , renderLikeDNA
  , renderMonospacedGroup
  , renderSingleton
  , renderSmartly
  , renderString
  , renderSymbolString
  -- * Constructors
  , alignElement
  , deleteElement
  , insertElement
  , gappingElement
  -- * Queries
  , isAlign
  , isDelete
  , isInsert
  , isGapping
  ) where

import           Control.DeepSeq
import           Data.Alphabet
import           Data.Alphabet.IUPAC
import           Data.Bits
--import           Data.Coerce
import           Data.Foldable
import           Data.Key
import           Data.List.NonEmpty      (NonEmpty (..))
import qualified Data.List.NonEmpty      as NE
import           Data.Semigroup.Foldable
import           Data.Vector.NonEmpty
--import           Data.Vector.Unboxed     (Unbox)
import           Data.Word
import           GHC.Generics
import           Prelude                 hiding (filter)


type SymbolString = Vector SymbolContext


data Blank = Blank


-- |
-- A non-empty set of characters.
newtype SymbolAmbiguityGroup = SAG Word16
    deriving stock   (Generic)
    deriving newtype (Bits, Enum, Eq, FiniteBits, Ord)


data  SymbolContext
    = Align   {-# UNPACK #-} !SymbolAmbiguityGroup {-# UNPACK #-} !SymbolAmbiguityGroup {-# UNPACK #-} !SymbolAmbiguityGroup
    | Delete  {-# UNPACK #-} !SymbolAmbiguityGroup {-# UNPACK #-} !SymbolAmbiguityGroup
    | Insert  {-# UNPACK #-} !SymbolAmbiguityGroup                                      {-# UNPACK #-} !SymbolAmbiguityGroup
    | Gapping {-# UNPACK #-} !SymbolAmbiguityGroup
    deriving stock (Eq, Generic, Ord)


alignElement :: SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> SymbolContext
alignElement = Align


deleteElement :: SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> SymbolContext
deleteElement = Delete


insertElement :: SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> SymbolContext
insertElement = Insert


gappingElement :: Word -> SymbolContext
gappingElement = Gapping . bit . fromEnum . pred


isAlign   :: SymbolContext -> Bool
isAlign   Align {} = True
isAlign   _        = False


isDelete  :: SymbolContext -> Bool
isDelete  Delete{} = True
isDelete  _        = False


isInsert  :: SymbolContext -> Bool
isInsert  Insert{} = True
isInsert  _        = False


isGapping :: SymbolContext -> Bool
isGapping Gapping{} = True
isGapping _         = False


{-
data  SymbolContext2
    = SymbolContext2
    { getMedian :: {-# UNPACK #-} !SymbolAmbiguityGroup
    , getLeft   :: {-# UNPACK #-} !SymbolAmbiguityGroup
    , getRight  :: {-# UNPACK #-} !SymbolAmbiguityGroup
    }
    deriving stock (Eq, Generic, Ord)


alignElement :: SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> SymbolContext2
alignElement = SymbolContext2


deleteElement :: SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> SymbolContext2
deleteElement m y = SymbolContext2 m 0 y


insertElement :: SymbolAmbiguityGroup -> SymbolAmbiguityGroup -> SymbolContext2
insertElement m x = SymbolContext2 m x 0


gappingElement :: Word -> SymbolContext2
gappingElement w = let g = bit . fromEnum $ w - 1
                   in  SymbolContext2 g 0 0


isAlign   :: SymbolContext2 -> Bool
isAlign   (SymbolContext2 _ x y) = coerce x /= (0 :: Word16) && coerce y /= (0 :: Word16)


isDelete  :: SymbolContext2 -> Bool
isDelete  (SymbolContext2 _ x y) = coerce x /= (0 :: Word16) && coerce y == (0 :: Word16)


isInsert  :: SymbolContext2 -> Bool
isInsert  (SymbolContext2 _ x y) = coerce x == (0 :: Word16) && coerce y /= (0 :: Word16)


isGapping :: SymbolContext2 -> Bool
isGapping (SymbolContext2 _ x y) = coerce x == (0 :: Word16) && coerce y == (0 :: Word16)
-}


encodeAmbiguityGroup :: (Eq a, Foldable1 f) => Alphabet a -> f a -> SymbolAmbiguityGroup
encodeAmbiguityGroup alphabet xs = force . SAG $ foldlWithKey f 0 alphabet
  where
    f a k e
      | e `elem` xs = a `setBit` k
      | otherwise   = a


decodeAmbiguityGroup :: Alphabet a -> SymbolAmbiguityGroup -> NonEmpty a
decodeAmbiguityGroup alphabet xs =
    case foldMapWithKey f alphabet of
      y:ys -> y:|ys
      []   -> error context
  where
    f k e
      | xs `testBit` k = [e]
      | otherwise      = []

    context = unwords ["No set bits found: ", show $ length alphabet, show xs]


instance NFData SymbolContext


instance NFData SymbolAmbiguityGroup


instance Semigroup SymbolAmbiguityGroup where

    (<>) (SAG lhs) (SAG rhs) = SAG $ lhs .|. rhs


instance Show Blank where

    show _ = ""


instance Show SymbolAmbiguityGroup where

    show (SAG v) = show v


instance Show SymbolContext where

    show (Align   m x y) = fold ["A|", showPad m, "|", showPad x, "|", showPad y, "|"]
    show (Delete  m x  ) = fold ["D|", showPad m, "|", showPad x, "|",   "     ", "|"]
    show (Insert  m   y) = fold ["I|", showPad m, "|",   "     ", "|", showPad y, "|"]
    show (Gapping v    ) = fold ["G|", showPad v, "|",   "     ", "|",   "     ", "|"]

    showList [] str = str <> "[]"
    showList xs str = str <> "[" <> foldMap g xs <> "]"
      where
        pad :: Show a => a -> String
        pad x = let s = show x in replicate (maxStrLen - length s) ' ' <> s
        blank = pad Blank

        maxStrLen        = maximum $ f <$> xs
        f (Align  m x y) = maximum $ length <$> [show m, show x, show y]
        f (Delete m x  ) = maximum $ length <$> [show m, show x        ]
        f (Insert m   y) = maximum $ length <$> [show m,         show y]
        f (Gapping v   ) = length $ show v

        g (Align  m x y) = fold ["A:", pad m, ".", pad x, ".", pad y, "|"]
        g (Delete m x  ) = fold ["D:", pad m, ".", pad x, ".", blank, "|"]
        g (Insert m   y) = fold ["I:", pad m, ".", blank, ".", pad y, "|"]
        g (Gapping v   ) = fold ["G:", pad v, ".", blank, ".", blank, "|"]


-- We pad things to be exactly 5 characters long with leading space because a
-- Word16 can have at most five, base 10 digits
showPad :: SymbolAmbiguityGroup -> String
showPad (SAG x) = replicate (5 - length shown) ' ' <> shown
  where
    shown = show x


renderString :: Alphabet Char -> SymbolString -> String
renderString alphabet = foldMap renderGroup . toNonEmpty
  where
    renderGroup Gapping{} = "-"
    renderGroup grp =
      case decodeAmbiguityGroup alphabet $ symbolAlignmentMedian grp of
        x:|[] -> [x]
        x:|xs -> fold ["[",[x],xs,"]"]


renderSymbolString :: Alphabet Char -> SymbolString -> String
renderSymbolString alphabet = (\s -> "[ "<>s<>" ]") . intercalate1 ", " . fmap renderContext . toNonEmpty
  where
    renderContext (Align  x _ _) = fold ["α: ", renderGroup x ]
    renderContext (Delete x _  ) = fold ["δ: ", renderGroup x ]
    renderContext (Insert x   _) = fold ["ι: ", renderGroup x ]
    renderContext (Gapping v   ) = fold ["—: ", renderGroup v ]

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
    renderContext _             = renderAmbiguityGroup . encodeAmbiguityGroup alphabet $ '-':|[]


renderSingleton :: Alphabet Char -> SymbolString -> String
renderSingleton alphabet = foldMap renderContext . toNonEmpty
  where
    gap = gapSymbol alphabet
    renderContext (Align x _ _) = pure . NE.head $ decodeAmbiguityGroup alphabet x
    renderContext _ = [gap]


renderSmartly :: Alphabet Char -> SymbolString -> String
renderSmartly alphabet
  | length alphabet == 5 = renderLikeDNA
  | otherwise            = renderSingleton alphabet


renderLikeDNA :: SymbolString -> String
renderLikeDNA = foldMap toList . interpretAmbiguityAsDNA
  where
    interpretAmbiguityAsDNA :: SymbolString -> [NonEmpty Char]
    interpretAmbiguityAsDNA = encodeIUPAC iupacToDna . fmap getAmbiguityAsDNA . toList

    getAmbiguityAsDNA :: SymbolContext -> NonEmpty Char
    getAmbiguityAsDNA Gapping{} = '-':|[]
    getAmbiguityAsDNA x         = decodeAmbiguityGroup dnaAlphabet $ symbolAlignmentMedian x

    dnaAlphabet = fromSymbols ['A','C','G','T','-']


renderAmbiguityGroup :: SymbolAmbiguityGroup -> String
renderAmbiguityGroup = show


symbolAlignmentMedian :: SymbolContext -> SymbolAmbiguityGroup
symbolAlignmentMedian (Align   x _ _) = x
symbolAlignmentMedian (Delete  x _  ) = x
symbolAlignmentMedian (Insert  x   _) = x
symbolAlignmentMedian (Gapping _    ) = undefined


symbolAlignmentLeft :: SymbolContext -> Maybe SymbolAmbiguityGroup
symbolAlignmentLeft   (Align   _ x _) = Just x
symbolAlignmentLeft   (Delete  _ x  ) = Just x
symbolAlignmentLeft   _               = Nothing


symbolAlignmentRight :: SymbolContext -> Maybe SymbolAmbiguityGroup
symbolAlignmentRight  (Align   _ _ x) = Just x
symbolAlignmentRight  (Insert  _   x) = Just x
symbolAlignmentRight  _               = Nothing


reverseContext :: SymbolContext -> SymbolContext
reverseContext (Align  med x y) = Align  med y x
reverseContext (Delete med x  ) = Insert med   x
reverseContext (Insert med   y) = Delete med y
reverseContext x                = x


filterGaps :: SymbolString -> SymbolString
filterGaps = filter f
  where
    f Align   {} = True
    f Delete  {} = True
    f Insert  {} = True
    f Gapping {} = False


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
