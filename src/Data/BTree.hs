{-# LANGUAGE DeriveFunctor #-}

module Data.BTree where

import Data.Bifunctor


data BTree b a
   = Internal (NodeDatum b) (BTree b a) (BTree b a)
   | Leaf     (NodeDatum a)
   deriving (Eq, Functor)


data NodeDatum a
   = NodeDatum
   { identifier :: String
   , nodeDatum  :: a
   } deriving (Eq, Functor)



instance Bifunctor BTree where

    bimap f g (Internal x lhs rhs) = Internal (f <$> x) (bimap f g lhs) (bimap f g rhs)
    bimap f g (Leaf     x        ) = Leaf $ g <$> x

    first  f (Internal x lhs rhs) = Internal (f <$> x) (first f lhs) (first f rhs)
    first  f (Leaf     x        ) = Leaf x

    second = fmap 


getNodeDatum :: BTree a a -> a
getNodeDatum (Leaf     (NodeDatum _ x)) = x
getNodeDatum (Internal (NodeDatum _ x) _ _) = x


postorder :: (a -> a -> a) -> BTree b a -> BTree a a
postorder f node =
    case node of
      Leaf x -> Leaf x
      Internal (NodeDatum i _) lhs rhs ->
         let  lhs' = postorder f lhs
              rhs' = postorder f rhs
              x = getNodeDatum lhs'
              y = getNodeDatum rhs'
         in   Internal (NodeDatum i (f x y)) lhs' rhs'


preorder :: (b -> c) -> (c -> b -> c) -> (c -> a -> d) -> BTree b a -> BTree c d
preorder = undefined
