{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Data.UserInput
  ( UserInput(..)
  , ExampleFileRequest(..)
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Data.Alphabet
import           Data.BTree
import           Data.Char
import           Data.Decoration
import           Data.Foldable
import           Data.Functor                 (($>))
import           Data.Key
import           Data.List.NonEmpty           (NonEmpty(..))
import qualified Data.List.NonEmpty    as NE
import           Data.Matrix.ZeroIndexed      (matrix)
import           Data.Map                     (Map)
import qualified Data.Map              as M
import           Data.Pointed
import           Data.Semigroup               ((<>))
import           Data.Semigroup.Foldable
import           Data.Set                     (Set)
import           Data.SymbolString
import           Data.TCM
import           Data.Validation
import           System.IO


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
