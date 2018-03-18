{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module File.Format.Newick
  ( NewickForest 
  , NewickNode(branchLength, descendants, newickLabel)
  , isLeaf
  , newickNode
  , newickStreamParser
  , renderNewickForest
  ) where


import Data.List.NonEmpty          (NonEmpty, some1)
import File.Format.Newick.Internal
import File.Format.Newick.Parser
import Text.Megaparsec


-- |
-- Parses an entire stream into a zero or more 'NewickForest's.
newickStreamParser :: (MonadParsec e s m, Token s ~ Char) => m (NonEmpty NewickForest)
newickStreamParser = some1 forestDefinitions <* eof


forestDefinitions :: (MonadParsec e s m, Token s ~ Char) => m NewickForest 
forestDefinitions = explicitForest <|> implicitForest


explicitForest :: (MonadParsec e s m, Token s ~ Char) => m NewickForest
explicitForest = try newickForestDefinition


implicitForest :: (MonadParsec e s m, Token s ~ Char) => m NewickForest
implicitForest = pure <$> newickExtendedDefinition
