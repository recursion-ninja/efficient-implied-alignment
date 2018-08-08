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

type BTree2 a = BTree (NodeDec a) (LeafDec a)

data Init   -- ^ Read in from User
data Prelim -- ^ Result of PostOrder
data Final  -- ^ Result of PreOrder

type instance NodeDec Init = ()
type instance LeafDec Init = InitialLeaf

type instance NodeDec Prelim = InitialInternalNode
type instance LeafDec Prelim = InitialLeaf

type instance NodeDec Final = FinalizedInternalNode
type instance LeafDec Final = FinalizedLeaf

postOrderTraverse :: TheMetricWeReadIn -> BTree2 Init -> BTree2 Prelim
postOrderTraverse f = postorder f postorderLogic

preOrderTraverse :: TheMetric -> Btree2 Prelim -> BTree2 Final
preOrderTraverse f = preorder f preorderRootLogic preorderInternalLogic preorderLeafLogic
