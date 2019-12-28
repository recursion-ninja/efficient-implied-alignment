{-# LANGUAGE TypeFamilies #-}

module Data.DecTree where

import           Alignment       (PairwiseAlignment, postorderLogic,
                                  preorderInternalLogic, preorderLeafLogic,
                                  preorderRootLogic)
import           Data.BTree      (BTree, postorder, preorder)
import           Data.Decoration (FinalizedNode (..), PreliminaryNode (..))


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

postorderTraverse :: Metric -> BTreeF Init -> BTreeF Prelim
postorderTraverse m = postorder (postorderLogic m)

preorderTraverse :: BTreeF Prelim -> BTreeF Final
preorderTraverse = preorder preorderRootLogic preorderInternalLogic preorderLeafLogic
