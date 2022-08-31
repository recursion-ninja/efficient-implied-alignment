-----------------------------------------------------------------------------
-- |
-- Module      :  File.Format.Newick
-- Copyright   :  (c) 2015-2015 Ward Wheeler
-- License     :  BSD-style
--
-- Maintainer  :  wheeler@amnh.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Function for parsing Newick tree files into a topological tree structure.
--
-----------------------------------------------------------------------------

{-# Language ApplicativeDo #-}
{-# Language FlexibleContexts #-}
{-# Language ImportQualifiedPost #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}

module File.Format.Newick.Parser
    ( newickStandardDefinition
    ) where

import Control.Applicative.Combinators.NonEmpty
import Control.Monad.Fail
import Data.BTree
import Data.Char (isSpace)
import Data.Foldable
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy
import Data.Scientific (toRealFloat)
import Data.String
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Void
import Prelude hiding (lookup)
import Text.Megaparsec hiding (label, sepBy1)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (scientific, skipBlockCommentNested)
import Text.Megaparsec.Custom


-- |
-- Parses a stream producing a standard Newick tree
{-# INLINABLE newickStandardDefinition #-}
{-# SPECIALISE newickStandardDefinition :: Parsec Void  T.Text (BTree () ()) #-}
{-# SPECIALISE newickStandardDefinition :: Parsec Void LT.Text (BTree () ()) #-}
{-# SPECIALISE newickStandardDefinition :: Parsec Void  String (BTree () ()) #-}
newickStandardDefinition :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m (BTree () ())
newickStandardDefinition = whitespace *> newickNodeDefinition <* symbol (char ';')


-- |
-- Definition of a serialized Newick node consisting of the node's descendants,
-- optional label, and optional branch length.
{-# INLINABLE newickNodeDefinition #-}
{-# SPECIALISE newickNodeDefinition :: Parsec Void  T.Text (BTree () ()) #-}
{-# SPECIALISE newickNodeDefinition :: Parsec Void LT.Text (BTree () ()) #-}
{-# SPECIALISE newickNodeDefinition :: Parsec Void  String (BTree () ()) #-}
newickNodeDefinition :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m (BTree () ())
newickNodeDefinition = do
    x :| xs <- descendantListDefinition
    case xs of
        []    -> pure x
        y : _ -> do
            label' <- optional newickLabelDefinition
            _      <- optional branchLengthDefinition
            pure $ Internal (NodeDatum (fold label') ()) x y


-- |
-- Parses one or more subtrees consisting of a single node or a further
-- descendant list.
{-# INLINABLE descendantListDefinition #-}
{-# SPECIALISE descendantListDefinition :: Parsec Void  T.Text (NonEmpty (BTree () ())) #-}
{-# SPECIALISE descendantListDefinition :: Parsec Void LT.Text (NonEmpty (BTree () ())) #-}
{-# SPECIALISE descendantListDefinition :: Parsec Void  String (NonEmpty (BTree () ())) #-}
descendantListDefinition :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m (NonEmpty (BTree () ()))
descendantListDefinition = char '(' *> trimmed subtreeDefinition `sepBy1` char ',' <* char ')' <* whitespace


-- |
-- Definition of a Newick subtree consisting of either a single leaf node or a
-- greater subtree.
{-# INLINABLE subtreeDefinition #-}
{-# SPECIALISE subtreeDefinition :: Parsec Void  T.Text (BTree () ()) #-}
{-# SPECIALISE subtreeDefinition :: Parsec Void LT.Text (BTree () ()) #-}
{-# SPECIALISE subtreeDefinition :: Parsec Void  String (BTree () ()) #-}
subtreeDefinition :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m (BTree () ())
subtreeDefinition = newickNodeDefinition <|> newickLeafDefinition


-- |
-- Definition of a single leaf node in a Newick tree. Must contain a node label.
-- Has no descendants be definition.
{-# INLINABLE newickLeafDefinition #-}
{-# SPECIALISE newickLeafDefinition :: Parsec Void  T.Text (BTree () ()) #-}
{-# SPECIALISE newickLeafDefinition :: Parsec Void LT.Text (BTree () ()) #-}
{-# SPECIALISE newickLeafDefinition :: Parsec Void  String (BTree () ()) #-}
newickLeafDefinition :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m (BTree () ())
newickLeafDefinition = do
    label' <- newickLabelDefinition
    _      <- optional branchLengthDefinition
    pure . Leaf $ NodeDatum label' ()


-- |
-- Defines the label for a '(BTree () ())' which can be either quoted or unquoted.
{-# INLINABLE newickLabelDefinition #-}
{-# SPECIALISE newickLabelDefinition :: Parsec Void  T.Text T.Text #-}
{-# SPECIALISE newickLabelDefinition :: Parsec Void LT.Text T.Text #-}
{-# SPECIALISE newickLabelDefinition :: Parsec Void  String T.Text #-}
newickLabelDefinition :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m T.Text
newickLabelDefinition = (quotedLabel <|> unquotedLabel) <* whitespace


-- |
-- We use a recursive parsing technique to handle the quoted escape sequence
-- of two single quotes ("''") to denote an escaped quotation character
-- in the quoted label rather than signifying the end of the quoted label
{-# INLINABLE quotedLabel #-}
{-# SPECIALISE quotedLabel :: Parsec Void  T.Text T.Text #-}
{-# SPECIALISE quotedLabel :: Parsec Void LT.Text T.Text #-}
{-# SPECIALISE quotedLabel :: Parsec Void  String T.Text #-}
quotedLabel :: forall e s m . (MonadFail m, MonadParsec e s m, Token s ~ Char) => m T.Text
quotedLabel = do
    _ <- char '\''
    x <- quotedLabelData
    case filter (not . isSpace) x of
        [] -> fail $ fold ["Blank quoted identifier found. The identifier '", x, "' is not valid"]
        _  -> pure $ fromString x
    where
        quotedLabelData = do
            prefix <- noneOfThese $ '\'' : invalidQuotedLabelChars
            _      <- char '\''
            suffix <- optional . try $ char '\'' *> quotedLabelData
            pure
                $ let p = chunkToTokens (Proxy :: Proxy s) prefix
                  in  case suffix of
                        Just y  -> p <> ('\'' : y)
                        Nothing -> p


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
{-# INLINABLE unquotedLabel #-}
{-# SPECIALISE unquotedLabel :: Parsec Void  T.Text T.Text #-}
{-# SPECIALISE unquotedLabel :: Parsec Void LT.Text T.Text #-}
{-# SPECIALISE unquotedLabel :: Parsec Void  String T.Text #-}
unquotedLabel :: forall e s m . (MonadParsec e s m, Token s ~ Char) => m T.Text
unquotedLabel = fromString . chunkToTokens (Proxy :: Proxy s) <$> noneOfThese invalidUnquotedLabelChars


-- |
-- Characters which can only appear in a quoted '(BTree () ())' label.
{-# INLINE requiresQuotedLabelChars #-}
requiresQuotedLabelChars :: String
requiresQuotedLabelChars = " ':;,()[]<>"


-- |
-- List of characters which __cannot__ appear in an /quoted/ label of a
-- '(BTree () ())'.
{-# INLINE invalidQuotedLabelChars #-}
invalidQuotedLabelChars :: String
invalidQuotedLabelChars = "\r\n\t\f\v\b"


-- |
-- List of characters which __cannot__ appear in an /unquoted/ label of a
-- '(BTree () ())'.
{-# INLINE invalidUnquotedLabelChars #-}
invalidUnquotedLabelChars :: String
invalidUnquotedLabelChars = invalidQuotedLabelChars <> requiresQuotedLabelChars


-- |
-- Definition of a serialized branch length between two nodes in the Newick
-- tree. Since the Newick tree is implicitly rooted in it's serialization form,
-- the 'branchLength' of a given '(BTree () ())' is the branch length itself and
-- it's parent. Becomes non-sensical with extended Newick trees that have nodes
-- with "in-degree" greater than one.
{-# INLINABLE branchLengthDefinition #-}
{-# SPECIALISE branchLengthDefinition :: Parsec Void  T.Text Double #-}
{-# SPECIALISE branchLengthDefinition :: Parsec Void LT.Text Double #-}
{-# SPECIALISE branchLengthDefinition :: Parsec Void  String Double #-}
branchLengthDefinition :: (MonadParsec e s m, Token s ~ Char) => m Double
branchLengthDefinition = symbol (char ':') *> (toRealFloat <$> symbol scientific)


-- |
-- Convenience combinator for stripping /leading and trailing/ whitespace from a
-- combinator.
{-# INLINE trimmed #-}
{-# SPECIALISE trimmed :: Parsec Void  T.Text a -> Parsec Void  T.Text a #-}
{-# SPECIALISE trimmed :: Parsec Void LT.Text a -> Parsec Void LT.Text a #-}
{-# SPECIALISE trimmed :: Parsec Void  String a -> Parsec Void  String a #-}
trimmed :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
trimmed x = whitespace *> x <* whitespace


-- |
-- Convenience combinator for stripping /trailing/ whitespace from a combinator.
{-# INLINE symbol #-}
{-# SPECIALISE symbol :: Parsec Void  T.Text a -> Parsec Void  T.Text a #-}
{-# SPECIALISE symbol :: Parsec Void LT.Text a -> Parsec Void LT.Text a #-}
{-# SPECIALISE symbol :: Parsec Void  String a -> Parsec Void  String a #-}
symbol :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
symbol x = x <* whitespace


-- |
-- Definition of space between tokens which can be discarded. This includes
-- spaces /and/ comments.
{-# INLINE whitespace #-}
{-# SPECIALISE whitespace :: Parsec Void  T.Text () #-}
{-# SPECIALISE whitespace :: Parsec Void LT.Text () #-}
{-# SPECIALISE whitespace :: Parsec Void  String () #-}
whitespace :: forall e s m . (MonadParsec e s m, Token s ~ Char) => m ()
whitespace =
    let spChar = void spaceChar
        block  = skipBlockCommentNested (tokenToChunk proxy '[') (tokenToChunk proxy ']')
        proxy  = Proxy :: Proxy s
    in  skipMany $ choice [hidden spChar, hidden block]
