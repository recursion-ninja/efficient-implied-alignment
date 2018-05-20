{-# LANGUAGE ApplicativeDo, FlexibleContexts, TypeFamilies #-}

module File.Format.Fastc.Parser 
  ( CharacterSequence
  , FastcParseResult
  , FastcSequence(..)
  , Identifier
  , Symbol
  , fastcStreamParser
  , fastcSymbolSequence
  , fastcTaxonSequenceDefinition
  ) where


import           Control.Monad.Combinators.NonEmpty
import           Data.Char                   (isSpace)
import           Data.List.NonEmpty          (NonEmpty, some1)
import qualified Data.List.NonEmpty   as NE
import           Data.Semigroup
import qualified Data.Vector.NonEmpty as V
import           File.Format.Fastc.Internal
import           Text.Megaparsec      hiding (sepBy1, some, someTill)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Custom


-- |
-- Unconverted result of a fastc parse
type FastcParseResult = NonEmpty FastcSequence


-- |
-- Pairing of taxa label with an unconverted sequence
data FastcSequence
   = FastcSequence
   { fastcLabel   :: Identifier
   , fastcSymbols :: CharacterSequence
   } deriving (Eq,Show)


-- |
-- Consumes a stream of 'Char's and parses the stream into a 'FastcParseResult'
fastcStreamParser :: (MonadParsec e s m, Token s ~ Char) => m FastcParseResult
fastcStreamParser = some fastcTaxonSequenceDefinition <* eof


-- |
-- Parses a FASTC 'Identifier' and the associated sequence, discarding any
-- comments
fastcTaxonSequenceDefinition :: (MonadParsec e s m, Token s ~ Char) => m FastcSequence
fastcTaxonSequenceDefinition = do
    name <- identifierLine
    seq' <- try fastcSymbolSequence <?> ("Unable to read symbol sequence for label: '" ++ name ++ "'")
    _    <- space
    pure $ FastcSequence name seq'


-- |
-- Parses a sequence of 'Symbol's represneted by a 'CharacterSequence'.
-- Symbols can be multi-character and are assumed to be seperated by whitespace.
fastcSymbolSequence :: (MonadParsec e s m, Token s ~ Char) => m CharacterSequence
fastcSymbolSequence = V.fromNonEmpty <$> (space *> fullSequence)
  where
    fullSequence = sconcat <$> some1 (inlineSpace *> sequenceLine)
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
