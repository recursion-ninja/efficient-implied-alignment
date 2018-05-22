{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Data.UserInput
  ( UserInput(..)
  , ExampleFileRequest(..)
  ) where

import           Control.DeepSeq


data  UserInput
    = UserInput
    { dataFile    :: String
    , treeFile    :: String
    , tcmFile     :: String
    , outputFile  :: String
    , verbose     :: Bool
--    , commandHelp :: ExampleFileRequest
    } deriving (Show)


data  ExampleFileRequest
    = DataFileRequest
    | TreeFileRequest
    | TcmFileRequest
    | NoFileRequest
    deriving (Eq, Show)


instance NFData ExampleFileRequest where

    rnf DataFileRequest = ()
    rnf TreeFileRequest = ()
    rnf TcmFileRequest  = ()
    rnf NoFileRequest   = ()


instance NFData UserInput where

    rnf (UserInput a b c d e {-f-}) = rnf a `seq` rnf b `seq` rnf c `seq`
                                  rnf d `seq` rnf e -- `seq` rnf f
