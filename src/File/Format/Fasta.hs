{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module File.Format.Fasta
  ( CharacterSequence
  , FastaParseResult
  , FastaSequence(..)
  , Identifier
  , Symbol
  , fastaStreamParser
  ) where


import File.Format.Fasta.Parser
