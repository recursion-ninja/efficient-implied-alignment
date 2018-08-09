{-# LANGUAGE TypeFamilies #-}

module Data.DecTree where

import Data.Decoration
  ( PreliminaryNode(..)
  , FinalizedNode(..)
  )
import Data.BTree (BTree, preorder, postorder)
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
type instance LeafDec Init = PreliminaryNode

type instance NodeDec Prelim = PreliminaryNode
type instance LeafDec Prelim = PreliminaryNode

type instance NodeDec Final = FinalizedNode
type instance LeafDec Final = FinalizedNode

type Metric = PairwiseAlignment Char

postOrderTraverse :: Metric -> BTreeF Init -> BTreeF Prelim
postOrderTraverse m = postorder (postorderLogic m)

preOrderTraverse :: Metric -> BTreeF Prelim -> BTreeF Final
preOrderTraverse m = preorder preorderRootLogic preorderInternalLogic preorderLeafLogic
