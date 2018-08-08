{-# LANGUAGE TypeFamilies #-}

module Data.DecTree where

import Data.Decoration
  (InitialInternalNode
  , FinalizedInternalNode
  , InitialLeaf(..)
  , FinalizedLeaf(..)
  )
import Data.BTree (BTree)
import Alignment
  (postorderLogic
  , preorderInternalLogic
  , preorderLeafLogic
  , preorderRootLogic
  , PairwiseAlignment)


type family NodeDec i
type family LeafDec i
type BTreeF i = BTree (NodeDec i) (LeafDec i)


data Init   -- ^ Read in from User
data Prelim -- ^ Result of PostOrder
data Final  -- ^ Result of PreOrder

type instance NodeDec Init = ()
type instance LeafDec Init = InitialLeaf

type instance NodeDec Prelim = InitialInternalNode
type instance LeafDec Prelim = InitialLeaf

type instance NodeDec Final = FinalizedInternalNode
type instance LeafDec Final = FinalizedLeaf

type Metric = PairwiseAlignment Char

postOrderTraverse :: Metric -> BTreeF Init -> BTreeF Prelim
postOrderTraverse f = undefined-- postorder f postorderLogic

preOrderTraverse :: Metric -> BTreeF Prelim -> BTreeF Final
preOrderTraverse f = undefined--preorder f preorderRootLogic preorderInternalLogic preorderLeafLogic
