{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}

module File.Format.Newick.Parser
  ( newickStandardDefinition
  ) where

import Control.Monad.Combinators.NonEmpty
import Data.BTree
import Data.Char                   (isSpace)
import Data.Foldable
import Data.Functor                (void)
import Data.List.NonEmpty          (NonEmpty(..), some1)
import Data.Maybe                  (fromMaybe)
import Data.Proxy
import Text.Megaparsec      hiding (sepBy1)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer  (skipBlockCommentNested)
import Text.Megaparsec.Custom


-- |
-- Parses a stream producing a standard Newick tree
newickStandardDefinition :: (MonadParsec e s m, Token s ~ Char) => m (BTree () ())
newickStandardDefinition = whitespace *> newickNodeDefinition <* symbol (char ';')


-- |
-- Definition of a serialized Newick node consisiting of the node's descendants,
-- optional label, and optional branch length. Mutually recursive with
-- 'subtreeDefinition '.
newickNodeDefinition :: (MonadParsec e s m, Token s ~ Char) => m (BTree () ())
newickNodeDefinition = do
    x:|xs  <- descendantListDefinition
    case xs of
      []  -> pure x
      y:_ -> do
        label' <- optional newickLabelDefinition
        _      <- optional branchLengthDefinition
        pure $ Internal (NodeDatum (fromMaybe "" label') ()) x y


-- |
-- Parses one or more subtrees consisting of a single node or a further
-- descendant list.
descendantListDefinition :: (MonadParsec e s m, Token s ~ Char) => m (NonEmpty (BTree () ()))
descendantListDefinition = char '(' *> trimmed subtreeDefinition `sepBy1` char ',' <* char ')' <* whitespace


-- |
-- Definition of a Newick subtree consisting of either a single leaf node or a
-- greater subtree. Mutually recursive with 'newickNodeDefinition'.
subtreeDefinition :: (MonadParsec e s m, Token s ~ Char) => m (BTree () ())
subtreeDefinition = newickNodeDefinition <|> newickLeafDefinition


-- |
-- Definition of a sigle leaf node in a Newick tree. Must contain a node label.
-- Has no descendants be definition.
newickLeafDefinition :: (MonadParsec e s m, Token s ~ Char) => m (BTree () ())
newickLeafDefinition = do
    label' <- newickLabelDefinition
    _      <- optional branchLengthDefinition
    pure . Leaf $ NodeDatum label' ()


-- |
-- Defines the label for a 'NewickNode' which can be either quoted or unquoted.
newickLabelDefinition :: (MonadParsec e s m, Token s ~ Char) => m String
newickLabelDefinition = (quotedLabel <|> unquotedLabel) <* whitespace


-- |
-- We use a recursive parsing technique to handle the quoted escape sequence
-- of two single quotes ("''") to denote an escaped quotation character 
-- in the quoted label rather than signifying the end of the quoted label
quotedLabel :: (MonadParsec e s m, Token s ~ Char) => m String
quotedLabel = do
    _ <- char '\''
    x <- quotedLabelData
    case filter (not.isSpace) x of
      [] -> fail $ "Blank quoted identifier found. The identifier '"<>x<>"' is not valid"
      _  -> pure x
  where 
    quotedLabelData = do
      prefix <- many (noneOf $ '\'':invalidQuotedLabelChars)
      _      <- char '\''
      suffix <- optional . try $ char '\'' *> quotedLabelData
      pure $
        case suffix of
          Just y  -> prefix <> ('\'':y)
          Nothing -> prefix


-- |
-- The following characters are not allowed in a newick unquoted label:
-- " \r\n\t\v\b':;,()[]<>"
-- We disallow the '<' & '>' characters in unquoted labels in all newick 
-- file formats because they would interfere with the parsing of Foreset 
-- Extended Newick file types. The '<' & '>' characters are technically
-- allowed in an unquoted newick label according to the Gary Olsen
-- interpretation of the standard Newick format and the Extended Newick
-- format. However, if a user really want to put '<' & '>' characters in
-- a node label, they can always put such characters in a quoted label.
unquotedLabel :: (MonadParsec e s m, Token s ~ Char) => m String
unquotedLabel = fmap toList . some1 $ noneOf invalidUnquotedLabelChars


-- |
-- Characters which can ontly appear in a quoted 'NewickNode' label.
requiresQuotedLabelChars :: String
requiresQuotedLabelChars = " ':;,()[]<>"


-- |
-- List of chacracters which __cannot__ appear in an /quoted/ label of a
-- 'NewickNode'.
invalidQuotedLabelChars :: String
invalidQuotedLabelChars = "\r\n\t\f\v\b"


-- |
-- List of chacracters which __cannot__ appear in an /unquoted/ label of a
-- 'NewickNode'. A superset of 'invalidQuotedLabelChars'.
invalidUnquotedLabelChars :: String
invalidUnquotedLabelChars = invalidQuotedLabelChars <> requiresQuotedLabelChars


-- |
-- Definition of a serialized branch length between two nodes in the Newick
-- tree. Since the Newick tree is impicitly rooted in it's serialization form,
-- the 'branchLength' of a given 'NewickNode' is the branch length itself and
-- it's parent. Becomes non-sensical with extended Newick trees that have nodes
-- with "in-degree" greater than one.
branchLengthDefinition :: (MonadParsec e s m, Token s ~ Char) => m Double
branchLengthDefinition = symbol (char ':') *> symbol double


-- |
-- Convinience combinator for stripping /leading and trailing/ whitespace from a
-- combinator.
trimmed :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
trimmed x = whitespace *> x <* whitespace


-- |
-- Convinience combinator for stripping /trailing/ whitespace from a combinator.
symbol :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
symbol x = x <* whitespace


-- |
-- Definition of space between tokens which can be discarded. This includes
-- spaces /and/ comments.
whitespace :: forall e s m. (MonadParsec e s m, Token s ~ Char) => m ()
whitespace = skipMany $ choice [ hidden singleSpace, hidden block ]
  where
    singleSpace = void spaceChar
    block = skipBlockCommentNested (tokenToChunk proxy '[') (tokenToChunk proxy ']')
    proxy = Proxy :: Proxy s
