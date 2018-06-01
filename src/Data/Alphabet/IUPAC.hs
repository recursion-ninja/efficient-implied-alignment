module Data.Alphabet.IUPAC
  ( decodeIUPAC
  , encodeIUPAC
  -- * IUPAC alphabets
  , isAlphabetAminoAcid
  , isAlphabetDna
  , isAlphabetRna
  , iupacToAminoAcid
  , iupacToDna
  , iupacToRna
  , module Data.Alphabet
  ) where


import           Control.Arrow            ((***))
import           Data.Alphabet
import           Data.Bimap               (Bimap)
import qualified Data.Bimap         as BM
import           Data.Foldable
import qualified Data.List.NonEmpty as NE
import qualified Data.Set              as Set 


decodeIUPAC
  :: ( Functor f
     , Ord a
     )
  => Bimap a a
  -> f a 
  -> f a
decodeIUPAC iupacCodes = fmap f
  where
    f e
      | e `BM.member` iupacCodes = iupacCodes BM.! e
      | otherwise = e


encodeIUPAC
  :: ( Functor f
     , Ord a
     )
  => Bimap a a
  -> f a
  -> f a
encodeIUPAC iupacCodes = fmap f
  where
    f e
      | e `BM.memberR` iupacCodes = iupacCodes BM.!> e
      | otherwise = e


-- |
-- Substitutions for converting to an Amino Acid sequence based on IUPAC codes.
iupacToAminoAcid :: Bimap (AmbiguityGroup Char) (AmbiguityGroup Char)
iupacToAminoAcid = toBimap
    [ ('A', "A")
    , ('B', "DN")
    , ('C', "C")
    , ('D', "D")
    , ('E', "E")
    , ('F', "F")
    , ('G', "G")
    , ('H', "H")
    , ('I', "I")
    , ('K', "K")
    , ('L', "L")
    , ('M', "M")
    , ('N', "N")
    , ('P', "P")
    , ('Q', "Q")
    , ('R', "R")
    , ('S', "S")
    , ('T', "T")
    , ('V', "V")
    , ('W', "W")
    , ('X', "ACDEFGHIKLMNPQRSTVWY")
    , ('Y', "Y")
    , ('Z', "EQ")
    , ('-', "-")
    , ('?', "ACDEFGHIKLMNPQRSTVWY-")
    ] 


-- |
-- Substitutions for converting to a DNA sequence based on IUPAC codes.
iupacToDna :: Bimap (AmbiguityGroup Char) (AmbiguityGroup Char)
iupacToDna = toBimap
    [ ('A', "A")
    , ('C', "C")
    , ('G', "G")
    , ('T', "T")
    , ('R', "AG")
    , ('Y', "CT")
    , ('S', "CG")
    , ('W', "AT")
    , ('K', "GT")
    , ('M', "AC")
    , ('B', "CGT")
    , ('D', "AGT")
    , ('H', "ACT")
    , ('V', "ACG")
    , ('N', "ACGT")
    , ('-', "-")
    , ('?', "ACGT-")
    
    , ('a', "A-")
    , ('c', "C-")
    , ('g', "G-")
    , ('t', "T-")
    , ('r', "AG-")
    , ('y', "CT-")
    , ('s', "CG-")
    , ('w', "AT-")
    , ('k', "GT-")
    , ('m', "AC-")
    , ('b', "CGT-")
    , ('d', "AGT-")
    , ('h', "ACT-")
    , ('v', "ACG-")
    , ('n', "ACGT-")
    ] 


-- | Substitutions for converting to a RNA sequence based on IUPAC codes.
iupacToRna :: Bimap (AmbiguityGroup Char) (AmbiguityGroup Char)
iupacToRna = BM.mapMonotonic setUpdate $ BM.mapMonotonicR setUpdate iupacToDna
  where
    setUpdate = fmap f
      where
        f x
          | x == 'T'  = 'U'
          | otherwise =  x


-- |
-- /O(n)/
--
-- Determines if the supplied alphabet represents amino acid symbols.
--
-- Useful for determining if an 'AmbiguityGroup' should be rendered as an IUPAC
-- code.
isAlphabetAminoAcid :: Alphabet Char -> Bool
isAlphabetAminoAcid = (`isAlphabetSubsetOf` "ACDEFGHIKLMNPQRSTVWY-")


-- |
-- /O(n)/
--
-- Determines if the supplied alphabet represents DNA symbols.
--
-- Useful for determining if an 'AmbiguityGroup' should be rendered as an IUPAC
-- code.
isAlphabetDna :: Alphabet Char -> Bool
isAlphabetDna = (`isAlphabetSubsetOf` "ACGT-")


-- |
-- /O(n)/
--
-- Determines if the supplied alphabet represents DNA symbols.
--
-- Useful for determining if an 'AmbiguityGroup' should be rendered as an IUPAC
-- code.
isAlphabetRna :: Alphabet Char -> Bool
isAlphabetRna = (`isAlphabetSubsetOf` "ACGU-")


isAlphabetSubsetOf :: Alphabet Char -> String -> Bool
isAlphabetSubsetOf alpha str = alphaSet `Set.isSubsetOf` strSet
  where
    alphaSet = Set.fromList $ toList alpha
    strSet   = Set.fromList str


toBimap :: [(Char, String)] ->  Bimap (AmbiguityGroup Char) (AmbiguityGroup Char)
toBimap = BM.fromList . fmap transform
  where
    transform = pure *** NE.fromList
    



