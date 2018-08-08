{-# LANGUAGE TypeFamilies #-}

module Data.DecTree where

import Data.Decoration
  (InitialInternalNode
  , FinalizedInternalNode
  , InitialLeaf(..)
  , FinalizedLeaf(..)
  )
import Data.BTree (BTree)


type family NodeDec i
type family LeafDec i

type BTreeInit = BTree (NodeDec Init) (LeafDec Init)
data Init
type instance NodeDec Init = InitialInternalNode
type instance LeafDec Init = InitialLeaf


type BTreeFinal = BTree (NodeDec Final) (LeafDec Final)
data Final
type instance NodeDec Final = FinalizedInternalNode
type instance LeafDec Final = FinalizedLeaf

postOrderTraverse :: BtreeInit 
