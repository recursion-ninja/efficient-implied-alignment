{-# Language DerivingStrategies #-}
{-# Language FlexibleContexts #-}
{-# Language Safe #-}
{-# Language TypeFamilies #-}

module Data.UserInput
    ( AlphabetType (..)
    , ExampleFileRequest (..)
    , UserInput (..)
    ) where

import Control.DeepSeq


data  UserInput
    = UserInput
    { dataFile     :: FilePath
    , treeFile     :: FilePath
    , tcmFile      :: FilePath
    , outputFile   :: FilePath
    , verbose      :: Bool
    , timing       :: Bool
    , alphabetType :: AlphabetType
    }
    deriving stock Show


data  AlphabetType
    = Standard
    | DNA
    | RNA
    deriving stock (Eq, Show)


data  ExampleFileRequest
    = DataFileRequest
    | TreeFileRequest
    | TcmFileRequest
    | NoFileRequest
    deriving stock (Eq, Show)


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

    rnf (UserInput a b c d e f g) =
        rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f `seq` rnf g
