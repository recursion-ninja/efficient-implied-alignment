{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module File.Format.Fastc
  ( CharacterSequence
  , FastcParseResult
  , FastcSequence(..)
  , Identifier
  , Symbol
  , fastcStreamParser
  ) where


import File.Format.Fastc.Parser
