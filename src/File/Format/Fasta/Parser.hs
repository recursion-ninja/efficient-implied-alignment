-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Fasta.Parser
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for parsing FASTA files.
--
-----------------------------------------------------------------------------

{-# Language ApplicativeDo #-}
{-# Language DeriveAnyClass #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language FlexibleContexts #-}
{-# Language ImportQualifiedPost #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}

module File.Format.Fasta.Parser
    ( FastaParseResult
    , FastaSequence (..)
    , Identifier
    , fastaSequence
    , fastaStreamParser
    , fastaTaxonSequenceDefinition
    ) where

import Control.DeepSeq (NFData)
import Control.Monad.Combinators.NonEmpty
import Data.Alphabet.IUPAC
import Data.Bimap (Bimap, toMap)
import Data.Char (isLower, isUpper, toLower, toUpper)
import Data.Data
import Data.Foldable
import Data.Functor
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map (keysSet)
import Data.Set (Set, mapMonotonic)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Vector.Unboxed.NonEmpty (Vector)
import Data.Vector.Unboxed.NonEmpty qualified as V
import Data.Void
import File.Format.Fasta.Internal
import GHC.Generics (Generic)
import Text.Megaparsec hiding (some, someTill)
import Text.Megaparsec.Char
import Text.Megaparsec.Custom


-- |
-- Unconverted result of a fasta parse
type FastaParseResult = NonEmpty FastaSequence


-- |
-- Pairing of taxa with an unconverted sequence
data  FastaSequence
    = FastaSequence
    { fastaLabel   :: {-# UNPACK #-} !Identifier
    , fastaSymbols :: {-# UNPACK #-} !(Vector Char)
    }
    deriving stock (Data, Eq, Generic, Show)
    deriving anyclass NFData


-- |
-- Consumes a stream of 'Char's and parses the stream into a 'FastaParseResult'
-- that has been validated for information consistency
{-# INLINABLE fastaStreamParser #-}
{-# SPECIALISE fastaStreamParser :: Parsec Void  T.Text FastaParseResult #-}
{-# SPECIALISE fastaStreamParser :: Parsec Void LT.Text FastaParseResult #-}
{-# SPECIALISE fastaStreamParser :: Parsec Void  String FastaParseResult #-}
fastaStreamParser :: (MonadParsec e s m, Monoid (Tokens s), Token s ~ Char) => m FastaParseResult
fastaStreamParser = some fastaTaxonSequenceDefinition <* eof


-- |
-- Parses a single FASTA defined taxon sequence from a Char stream
{-# INLINABLE fastaTaxonSequenceDefinition #-}
{-# SPECIALISE fastaTaxonSequenceDefinition :: Parsec Void  T.Text FastaSequence #-}
{-# SPECIALISE fastaTaxonSequenceDefinition :: Parsec Void LT.Text FastaSequence #-}
{-# SPECIALISE fastaTaxonSequenceDefinition :: Parsec Void  String FastaSequence #-}
fastaTaxonSequenceDefinition :: (MonadParsec e s m, Monoid (Tokens s), Token s ~ Char) => m FastaSequence
fastaTaxonSequenceDefinition = do
    name <- fastaTaxonName
    seq' <- try fastaSequence
    _    <- space
    pure $ FastaSequence name seq'


-- |
-- Consumes a line from the Char stream and parses a FASTA identifier
{-# INLINE fastaTaxonName #-}
{-# SPECIALISE fastaTaxonName :: Parsec Void  T.Text Identifier #-}
{-# SPECIALISE fastaTaxonName :: Parsec Void LT.Text Identifier #-}
{-# SPECIALISE fastaTaxonName :: Parsec Void  String Identifier #-}
fastaTaxonName :: (MonadParsec e s m, Token s ~ Char) => m Identifier
fastaTaxonName = identifierLine


-- |
-- Consumes one or more lines from the Char stream to produce a list of Chars
-- constrained to a valid Char alphabet representing possible character states
{-# INLINABLE fastaSequence #-}
{-# SPECIALISE fastaSequence :: Parsec Void  T.Text (Vector Char) #-}
{-# SPECIALISE fastaSequence :: Parsec Void LT.Text (Vector Char) #-}
{-# SPECIALISE fastaSequence :: Parsec Void  String (Vector Char) #-}
fastaSequence :: forall e s m . (MonadParsec e s m, Monoid (Tokens s), Token s ~ Char) => m (Vector Char)
fastaSequence = space *> fullSequence
    where
        fullSequence     = buildVector . fold <$> some taxonContentLine

        -- A line in the "taxon contents" can start with zero or more "inline whitespace" characters.
        -- After all leading whitespace has been consumed on the line, what remains must be either:
        --
        --   * A newline, signifying the end of the line
        --
        --   * One or more sequence data symbols, possibly separated by spaces,
        --       followed by a newline or the end of the file.
        taxonContentLine = inlinedSpace *> (sequenceLine <|> (endOfLine $> mempty))

        -- Defines the contents of a taxon line which contains sequence data
        sequenceLine     = fold <$> ((seqChunk <* inlinedSpace) `someTill` flexEOL)
            where seqChunk = someOfThese alphabet

        -- Matches on the end of line or the end of the stream.
        flexEOL = void (try endOfLine) <|> lookAhead eof

        buildVector :: Tokens s -> Vector Char
        buildVector = V.fromNonEmpty . NE.fromList . chunkToTokens (Proxy :: Proxy s)


-- |
-- Extract the keys from a 'Bimap'.
{-# INLINE extractFromBimap #-}
extractFromBimap :: Bimap (NonEmpty Char) a -> Set Char
extractFromBimap = mapMonotonic NE.head . keysSet . toMap
--extractFromBimap = mapMonotonic (head . NE.head) . keysSet . toMap


alphabet, otherValidChars, iupacAminoAcidChars, iupacNucleotideChars, iupacRNAChars :: Set Char
alphabet = fold [iupacAminoAcidChars, iupacNucleotideChars, iupacRNAChars]
otherValidChars = S.fromList ".-?#"
iupacAminoAcidChars = otherValidChars <> caseInsensitiveOptions (extractFromBimap iupacToAminoAcid)
iupacNucleotideChars = otherValidChars <> caseInsensitiveOptions (extractFromBimap iupacToDna)
iupacRNAChars = otherValidChars <> caseInsensitiveOptions (extractFromBimap iupacToRna)


-- |
-- Adds the lowercase and uppercase Chars to string when only the upper or
-- lower is present in the String
{-# INLINE caseInsensitiveOptions #-}
caseInsensitiveOptions :: Set Char -> Set Char
caseInsensitiveOptions = foldMap f
    where
        f x
            | isLower x = S.singleton x <> S.singleton (toUpper x)
            | isUpper x = S.singleton x <> S.singleton (toLower x)
            | otherwise = S.singleton x
