{-# Language Safe #-}

module Data.List.Utility
    ( duplicates
    , mostCommon
    ) where

import Data.Foldable
import Data.List (sort, sortBy)
import Data.Map (Map, assocs, empty, insertWith)
import Data.Ord (comparing)


-- |
-- \( \mathcal{O} \left( n * \log_2 n \right) \)
--
-- Returns the list of elements which are not unique in the input list.
--
-- ==_Example==
--
-- >>> duplicates "duplicate string"
-- "it"
--
-- >>> duplicates "GATACACATCAGATT"
-- "ACGT"
--
-- >>> duplicates [ 'A' .. 'Z' ]
-- []
duplicates :: (Foldable t, Ord a) => t a -> [a]
duplicates = duplicates' . sort . toList
    where
        duplicates' :: Ord a => [a] -> [a]
        duplicates' []  = []
        duplicates' [_] = []
        duplicates' (x : y : ys) =
            if x == y then (x :) . duplicates $ dropWhile (== y) ys else duplicates (y : ys)


-- |
-- \( \mathcal{O} \left( n * \log_2 n \right) \)
--
-- Returns the element that occurs the most often in the list.
--
-- ==_Example==
--
-- >>> mostCommon "GATACACATCAGATT"
-- Just 'A'
--
-- >>> mostCommon "AABCDDDEFGGT"
-- Just 'D'
mostCommon :: (Foldable t, Ord a) => t a -> Maybe a
mostCommon xs
    | null xs = Nothing
    | otherwise = case occurrences xs of
        []         -> Nothing
        (x, _) : _ -> Just x


-- |
-- \( \mathcal{O} \left( n * \log_2 n \right) \)
--
-- Returns a mapping of each unique element in the list paired with how often
-- the element occurs in the list.
--
-- The elements are in descending order of occurrence.
--
-- ==_Example==
--
-- >>> occurrences "GATACACATCAGATT"
-- [('A',6),('T',4),('C',3),('G',2)]
--
-- >>> occurrences "AABCDDDEFGGT"
-- [('D',3),('A',2),('G',2),('B',1),('C',1),('E',1),('F',1),('T',1)]
occurrences :: (Foldable t, Ord a) => t a -> [(a, Int)]
occurrences = collateOccurrenceMap . buildOccurrenceMap
    where

        buildOccurrenceMap =
            let occurrence :: (Ord k, Enum a, Num a) => k -> Map k a -> Map k a
                occurrence e = insertWith (const succ) e 1
            in  foldr occurrence empty

        collateOccurrenceMap :: Ord v => Map k v -> [(k, v)]
        collateOccurrenceMap =
            let comparator :: Ord v => (k, v) -> (k, v) -> Ordering
                comparator x y = descending $ comparing snd x y

                descending LT = GT
                descending GT = LT
                descending x  = x
            in  sortBy comparator . assocs
