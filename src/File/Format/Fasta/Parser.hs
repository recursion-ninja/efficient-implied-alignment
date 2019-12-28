{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module File.Format.Fasta.Parser
  ( CharacterSequence
  , FastaParseResult
  , FastaSequence(..)
  , Identifier
  , Symbol
  , fastaStreamParser
--  , fastaSymbolSequence
--  , fastaTaxonSequenceDefinition
  ) where


import           Control.Monad.Combinators.NonEmpty
import           Data.Char                          (isSpace)
import           Data.List.NonEmpty                 (NonEmpty, some1)
import           Data.Semigroup.Foldable
import qualified Data.Vector.NonEmpty               as V
import           File.Format.Fasta.Internal
import           Text.Megaparsec                    hiding (some, someTill)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Custom


-- |
-- Unconverted result of a fasta parse
type FastaParseResult = NonEmpty FastaSequence


-- |
-- Pairing of taxa label with an unconverted sequence
data FastaSequence
   = FastaSequence
   { fastaLabel   :: Identifier
   , fastaSymbols :: CharacterSequence
   } deriving (Eq,Show)


-- |
-- Consumes a stream of 'Char's and parses the stream into a 'FastaParseResult'
fastaStreamParser :: (MonadParsec e s m, Token s ~ Char) => m FastaParseResult
fastaStreamParser = some fastaTaxonSequenceDefinition <* eof


-- |
-- Parses a FASTA 'Identifier' and the associated sequence, discarding any
-- comments
fastaTaxonSequenceDefinition :: (MonadParsec e s m, Token s ~ Char) => m FastaSequence
fastaTaxonSequenceDefinition = do
    name <- identifierLine
    seq' <- try fastaSymbolSequence <?> ("Unable to read symbol sequence for label: '" <> name <> "'")
    _    <- space
    pure $ FastaSequence name seq'


-- |
-- Parses a sequence of 'Symbol's represneted by a 'CharacterSequence'.
-- Symbols can be multi-character and are assumed to be separated by whitespace.
fastaSymbolSequence :: (MonadParsec e s m, Token s ~ Char) => m CharacterSequence
fastaSymbolSequence = V.fromNonEmpty <$> (space *> fullSequence)
  where
    fullSequence = fold1 <$> some1 (inlineSpace *> sequenceLine)
    sequenceLine = symbolGroup `someTill` endOfLine


-- |
-- Parses either an ambiguity group of 'Symbol's or a single, unambiguous
-- 'Symbol'.
symbolGroup :: (MonadParsec e s m, Token s ~ Char) => m (NonEmpty Char)
symbolGroup = ambiguityGroup <|> (pure <$> validSymbol)


-- |
-- Parses an ambiguity group of symbols. Ambiguity groups are delimited by the
-- '\'|\'' character.
ambiguityGroup :: (MonadParsec e s m, Token s ~ Char) => m (NonEmpty Char)
ambiguityGroup = begin *> (validSymbol `someTill` close) <* close
  where
    begin = char '[' <* inlineSpace
    close = char ']' <* inlineSpace


-- |
-- Parses a 'Symbol' token ending with whitespace and excluding the forbidden
-- characters: '[\'>\',\'[\',\']\']'.
validSymbol :: (MonadParsec e s m, Token s ~ Char) => m Char
validSymbol = validChar <* inlineSpace
  where
    validChar = satisfy $ \x -> x /= '>' -- need to be able to match new taxa lines
                             && x /= '[' -- need to be able to start an ambiguity list
                             && x /= ']' -- need to be able to close an ambiguity list
                             && (not . isSpace) x
