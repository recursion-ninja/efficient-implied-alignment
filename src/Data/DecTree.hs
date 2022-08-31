{-# Language TypeFamilies #-}

module Data.DecTree
    ( -- * Types of decorated trees
      Final
    , Init
    , Prelim
      -- * Tree traversals
    , postorderTraverse
    , preorderTraverse
    ) where

import Alignment (PairwiseAlignment, postorderLogic, preorderInternalLogic, preorderLeafLogic, preorderRootLogic)
import Data.BTree (BTree, postorder, preorder)
import Data.Decoration (FinalizedNode(..), PreliminaryNode(..))


type BTreeF i = BTree (NodeDec i) (LeafDec i)


type Metric = PairwiseAlignment Char


data  Init


data  Prelim


data  Final


type family NodeDec i
type family LeafDec i


type instance NodeDec Init = ()
type instance LeafDec Init = PreliminaryNode
type instance NodeDec Prelim = PreliminaryNode
type instance LeafDec Prelim = PreliminaryNode
type instance NodeDec Final = FinalizedNode
type instance LeafDec Final = FinalizedNode


postorderTraverse :: Metric -> BTreeF Init -> BTreeF Prelim
postorderTraverse m = postorder (postorderLogic m)


preorderTraverse :: BTreeF Prelim -> BTreeF Final
preorderTraverse = preorder preorderRootLogic preorderInternalLogic preorderLeafLogic
