{-# LANGUAGE DeriveFunctor #-}

module Data.BTree where

import Control.Arrow             ((&&&))
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Foldable
import Data.List.NonEmpty hiding (length, takeWhile)
import Data.Semigroup
import Prelude            hiding (head)



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


instance Bifoldable BTree where

    bifoldr f g a (Leaf (NodeDatum _ x)) = g x a
    bifoldr f g a (Internal (NodeDatum _ x) lhs rhs) =
        f x $ bifoldr f g (bifoldr f g a lhs) rhs


instance Bitraversable BTree where

    bitraverse f g (Leaf (NodeDatum i x)) = Leaf . NodeDatum i <$> g x
    bitraverse f g (Internal (NodeDatum i x) lhs rhs) =
        Internal
          <$> (NodeDatum i <$> f x)
          <*> bitraverse f g lhs
          <*> bitraverse f g rhs


instance Foldable (BTree b) where

    foldr f a (Leaf (NodeDatum _ x)) = f x a
    foldr f a (Internal _ lhs rhs)   = foldr f (foldr f a lhs) rhs


instance Traversable (BTree b) where

    traverse f (Leaf (NodeDatum i x)) = Leaf . NodeDatum i <$> f x
    traverse f (Internal n lhs rhs)   = Internal n <$> traverse f lhs <*> traverse f rhs


getNodeDatum :: BTree a a -> a
getNodeDatum (Leaf     (NodeDatum _ x)) = x
getNodeDatum (Internal (NodeDatum _ x) _ _) = x


setLeafLabels :: BTree b a -> BTree b String
setLeafLabels (Leaf     (NodeDatum i _)) = Leaf $ NodeDatum i i
setLeafLabels (Internal n lhs rhs) = Internal n (setLeafLabels lhs) $ setLeafLabels rhs


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


preorder
  :: (b -> c)      -- ^ Root node case
  -> (c -> b -> c) -- ^ Internal node case
  -> (c -> a -> d) -- ^ Leaf node case
  -> BTree b a
  -> BTree c d
preorder rootTransformation internalTransformation leafTransformation rootNode =
    case rootNode of
      Leaf x -> undefined -- Single node trees are beyond the scope of this example.
      Internal (NodeDatum i x) lhs rhs ->
        let transformedRoot = rootTransformation x
            lhs' = go transformedRoot lhs
            rhs' = go transformedRoot rhs
        in  Internal (NodeDatum i transformedRoot) lhs' rhs'
  where
    go parentNode currentNode =
        case currentNode of
          Leaf x -> Leaf $ leafTransformation parentNode <$> x
          Internal (NodeDatum i x) lhs rhs ->
            let transformedDatum = internalTransformation parentNode x
                lhs' = go transformedDatum lhs
                rhs' = go transformedDatum rhs
            in  Internal (NodeDatum i transformedDatum) lhs' rhs'


renderPhylogeny
  :: (a -> String -> String)
  -> BTree b a
  -> String
renderPhylogeny f = horizontalRendering . toBinaryRenderingTree (const (const "")) f


renderAlignment
  :: (b -> String ->String)
  -> (a -> String -> String)
  -> BTree b a -> String
renderAlignment f g = horizontalRendering . toBinaryRenderingTree f g


toBinaryRenderingTree
  :: (b -> String -> String)
  -> (a -> String -> String)
  -> BTree b a
  -> BinaryRenderingTree
toBinaryRenderingTree f g tree = 
    case tree of
      Leaf     (NodeDatum i a)         -> Terminal $ g a i
      Internal (NodeDatum i b) lhs rhs -> 
          let lhs'    = toBinaryRenderingTree f g lhs
              rhs'    = toBinaryRenderingTree f g rhs
              subSize = subtreeSize lhs' + subtreeSize rhs'
          in  Branch (f b i) subSize Nothing $ lhs' :| [rhs']

-- |
-- An intermediate structure for rendering directed, acyclic graphs.
data  BinaryRenderingTree
    = Terminal String
    | Branch   String Word (Maybe String) (NonEmpty BinaryRenderingTree)
    deriving (Eq, Show)


-- |
-- Get the number of leaves present in a subtree.
subtreeSize :: BinaryRenderingTree -> Word
subtreeSize (Terminal _)       = 1
subtreeSize (Branch   _ x _ _) = x


-- |
-- Render a directed, acyclic graph in a horizontal fashion. Bias larger subtrees
-- towards the bottom and smaller subtrees to the top. Apply symbolic references
-- to network nodes.
horizontalRendering :: BinaryRenderingTree -> String
horizontalRendering = fold . intersperse "\n" . go
  where
    go :: BinaryRenderingTree -> NonEmpty String
    go (Terminal label) = pure $ "─ " <> label
    go (Branch   label _ labelMay kids) = sconcat paddedSubtrees
      where
        paddedSubtrees   = maybe prefixedSubtrees (`applyPadding` prefixedSubtrees) labelMay
        
        prefixedSubtrees :: NonEmpty (NonEmpty String)
        prefixedSubtrees = applyPrefixes medianLabel alignedSubtrees

        alignedSubtrees  :: NonEmpty (NonEmpty String)
        alignedSubtrees  = applySubtreeAlignment maxSubtreeDepth <$> renderedSubtrees

        renderedSubtrees :: NonEmpty (Int, NonEmpty String)
        renderedSubtrees = fmap (prefixLength &&& id) $ go <$> sortWith subtreeSize kids

        maxSubtreeDepth  = maximum $ fst <$> renderedSubtrees

        prefixLength     = length . takeWhile (`elem` "└┌│├┤─ ") . head

        medianLabel
          | null label = ""
          | otherwise  = replicate (maxSubtreeDepth - 1) '-' <> " " <> label

    applyPadding :: String -> NonEmpty (NonEmpty String) -> NonEmpty (NonEmpty String)
    applyPadding e input =
        case input of
          v:|[]     -> applyAtCenter e pad pad v :| []
          v:|(x:xs) -> fmap (pad<>) v :| (applyAtCenter e pad pad x : fmap (fmap (pad<>)) xs)
      where
        pad   = replicate (length e) ' '

    applyPrefixes :: String -> NonEmpty (NonEmpty String) -> NonEmpty (NonEmpty String)
    applyPrefixes medianLabel = run True
      where
        run :: Bool -> NonEmpty (NonEmpty String) -> NonEmpty (NonEmpty String) 
        run True  (v:|[])     = pure $ applyAtCenter "─" " " " " v
        run False (v:|[])     = pure $ applyAtCenter "└" "│" " " v
        run True  (v:|(x:xs)) = applyPrefixAndGlue v "┤" "┌" " " "│" (x:|xs)
        run False (v:|(x:xs)) = applyPrefixAndGlue v "│" "├" "│" " " (x:|xs)

        applyPrefixAndGlue v glue center upper lower xs =
            pure (applyAtCenter center upper lower v) <> pure (pure glue') <> run False xs
          where
            glue'
              | glue == "┤" = glue <> medianLabel
              | otherwise   = glue

    applySubtreeAlignment :: Int -> (Int, NonEmpty String) -> NonEmpty String
    applySubtreeAlignment maxLength (currLength, xs) = applyAtCenter branch pad pad xs
      where
        branch = replicate (maxLength - currLength) '─'
        pad    = replicate (maxLength - currLength) ' '

    applyAtCenter :: String -> String -> String -> NonEmpty String -> NonEmpty String
    applyAtCenter center     _     _ (x:|[]) = (center<>x) :| []
    applyAtCenter center upper lower (x:|xs) = ( upper<>x) :| snd (foldr f (False, []) xs)
      where
        f :: String -> (Bool, [String]) -> (Bool, [String])
        f str (crossedMidPoint, acc) =
          case str of
            h:_ | not crossedMidPoint && h `notElem` "└┌│├ " -> ( True, (center<>str):acc)
            _   | crossedMidPoint                            -> ( True, ( upper<>str):acc)
                | otherwise                                  -> (False, ( lower<>str):acc)

