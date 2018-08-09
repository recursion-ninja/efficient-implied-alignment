{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Data.UserInput
  ( AlphabetType(..)
  , UserInput(..)
  , ExampleFileRequest(..)
  ) where

import Control.DeepSeq


data  UserInput
    = UserInput
    { dataFile     :: FilePath
    , treeFile     :: FilePath
    , tcmFile      :: FilePath
    , outputFile   :: FilePath
    , verbose      :: Bool
    , alphabetType :: AlphabetType
--    , commandHelp :: ExampleFileRequest
    } deriving (Show)


data  AlphabetType
    = Standard
    | DNA
    | RNA
    deriving (Eq, Show)


data  ExampleFileRequest
    = DataFileRequest
    | TreeFileRequest
    | TcmFileRequest
    | NoFileRequest
    deriving (Eq, Show)


instance NFData AlphabetType where

    rnf Standard = ()
    rnf DNA      = ()
    rnf RNA      = ()


instance NFData ExampleFileRequest where

    rnf DataFileRequest = ()
    rnf TreeFileRequest = ()
    rnf TcmFileRequest  = ()
    rnf NoFileRequest   = ()


instance NFData UserInput where

    rnf (UserInput a b c d e f) = rnf a `seq` rnf b `seq` rnf c `seq`
                                  rnf d `seq` rnf e `seq` rnf f
