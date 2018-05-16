{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies #-}

module SampleData
  ( LeafInput
  , TreeInput
  , defaultAlphabet
  , defaultTripleCompare
  , discreteMetricTCM
  , sampleDataSets
  ) where

import           Alignment
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
import           Prelude               hiding (lookup, zip)



--case toEither $ unifyInput dataSetA topologyA of

type LeafInput = Map String (NonEmpty (NonEmpty String)) 


type TreeInput = BTree () ()


sampleDataSets :: [(String, LeafInput, TreeInput, TransitionCostMatrix String)]
sampleDataSets =
    [ ("Appended Deletions"                    , dataSetA, topologyA, discreteMetricTCM)
    , ("Prepended Deletions"                   , dataSetB, topologyB, discreteMetricTCM)
    , ("Appended Insertions"                   , dataSetC, topologyC, discreteMetricTCM)
    , ("Prepended Deletions"                   , dataSetD, topologyD, discreteMetricTCM)
    , ("Adjacent Deletion Insertion Events"    , dataSetE, topologyE, discreteMetricTCM)
    , ("Adjacent Deletion Insertion Events Two", dataSetF, topologyF, discreteMetricTCM)
    , ("Non-homology Deleted Insertion"        , dataSetG, topologyG, discreteMetricTCM)
    , ("Single Deleted Insertion"              , dataSetY, topologyY, discreteMetricTCM)
    , ("Non-homology Double Deleted Insertion" , dataSetX, topologyX, discreteMetricTCM)
    ]


defaultAlphabet :: Alphabet String
defaultAlphabet = fromSymbols $ pure <$> "ACGT-"


defaultTripleCompare :: ThreewayCompare String
defaultTripleCompare = buildThreeWayCompare defaultAlphabet discreteMetricTCM


discreteMetricTCM :: TransitionCostMatrix String
discreteMetricTCM = tcm
  where
    tcm = buildTransitionCostMatrix defaultAlphabet scm
    scm = buildSymbolChangeMatrix   defaultAlphabet fakeParseInput
    fakeParseInput = matrix 5 5 (\(i,j) -> if i == j then 0 else 1)


preferGapsTCM :: TransitionCostMatrix String
preferGapsTCM = tcm
  where
    tcm = buildTransitionCostMatrix defaultAlphabet scm
    scm = buildSymbolChangeMatrix   defaultAlphabet fakeParseInput
    fakeParseInput = matrix 5 5 f
      where
        f (i,j)
          | i == j    = 0
          | i == 4    = 1
          | j == 4    = 1
          | otherwise = 2


blank :: NodeDatum ()
blank = NodeDatum "" ()


toNonEmpties :: Foldable1 f => f Char -> NonEmpty (NonEmpty String)
toNonEmpties = foldMap1 (pure . pure . pure) 


dataSetA :: Map String (NonEmpty (NonEmpty String))
dataSetA = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"CGT"
    , 'A':|"CG"
    , 'A':|"C"
    , 'A':|""
    ]


topologyA :: BTree () ()
topologyA =
    Internal blank
    ( Leaf (NodeDatum "A" ()) )
    ( Internal blank
      ( Leaf (NodeDatum "B" ()) )
      ( Internal blank
        ( Leaf (NodeDatum "C" ()) )
        ( Leaf (NodeDatum "D" ()) )
      )
    )


dataSetB :: Map String (NonEmpty (NonEmpty String))
dataSetB = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|""
    , 'A':|"C"
    , 'A':|"CG"
    , 'A':|"CGT"
    ]


topologyB :: BTree () ()
topologyB =
    Internal blank
    ( Leaf (NodeDatum "A" ()) )
    ( Internal blank
      ( Leaf (NodeDatum "B" ()) )
      ( Internal blank
        ( Leaf (NodeDatum "C" ()) )
        ( Leaf (NodeDatum "D" ()) )
      )
    )


dataSetC :: Map String (NonEmpty (NonEmpty String))
dataSetC = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'T':|"GCA"
    , 'G':|"CA"
    , 'C':|"A"
    , 'A':|""
    ]


topologyC :: BTree () ()
topologyC =
    Internal blank
    ( Leaf (NodeDatum "A" ()) )
    ( Internal blank
      ( Leaf (NodeDatum "B" ()) )
      ( Internal blank
        ( Leaf (NodeDatum "C" ()) )
        ( Leaf (NodeDatum "D" ()) )
      )
    )


dataSetD :: Map String (NonEmpty (NonEmpty String))
dataSetD = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|""
    , 'C':|"A"
    , 'G':|"CA"
    , 'T':|"GCA"
    ]


topologyD :: BTree () ()
topologyD =
    Internal blank
    ( Leaf (NodeDatum "A" ()) )
    ( Internal blank
      ( Leaf (NodeDatum "B" ()) )
      ( Internal blank
        ( Leaf (NodeDatum "C" ()) )
        ( Leaf (NodeDatum "D" ()) )
      )
    )


dataSetE :: Map String (NonEmpty (NonEmpty String))
dataSetE = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"TA"
    , 'A':|"TA"
    , 'A':|"TA"
    , 'A':|"A"
    , 'A':|"A"
    , 'A':|"A"
    , 'A':|"CA"
    , 'A':|"CA"
    ]


topologyE :: BTree () ()
topologyE =
    Internal blank
    ( Internal blank
        ( Leaf (NodeDatum "A" ()) )
        ( Leaf (NodeDatum "B" ()) )
    )
    ( Internal blank
      ( Leaf (NodeDatum "C" ()) )
      ( Internal blank
        ( Leaf (NodeDatum "D" ()) )
        ( Internal blank
          ( Leaf (NodeDatum "E" ()) )
          ( Internal blank
            ( Leaf (NodeDatum "F" ()) )
            ( Internal blank
              ( Leaf (NodeDatum "G" ()) )
              ( Leaf (NodeDatum "H" ()) )
            )
          )
        )
      )
    )


dataSetF :: Map String (NonEmpty (NonEmpty String))
dataSetF = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"TTA"
    , 'A':|"TTA"
    , 'A':|"TTA"
    , 'A':|"TTA"
    , 'A':|"TA"
    , 'A':|"TA"
    , 'A':|"TA"
    , 'A':|"CTAA"
    , 'A':|"CTAA"
    ]


topologyF :: BTree () ()
topologyF =
    Internal blank
    ( Internal blank
        ( Leaf (NodeDatum "A" ()) )
        ( Leaf (NodeDatum "B" ()) )
    )
    ( Internal blank
      ( Leaf (NodeDatum "C" ()) )
      ( Internal blank
        ( Leaf (NodeDatum "D" ()) )
        ( Internal blank
          ( Leaf (NodeDatum "E" ()) )
          ( Internal blank
            ( Leaf (NodeDatum "F" ()) )
            ( Internal blank
              ( Leaf (NodeDatum "G" ()) )
              ( Internal blank
                ( Leaf (NodeDatum "H" ()) )
                ( Leaf (NodeDatum "I" ()) )
              )
            )
          )
        )
      )
    )


dataSetG :: Map String (NonEmpty (NonEmpty String))
dataSetG = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"AA"
    , 'A':|"AA"
    , 'A':|"CATA"
    , 'A':|"CATA"
    , 'A':|"CAA"
    , 'A':|"CAA"
    , 'A':|"AA"
    , 'A':|"AA"
    , 'A':|"CATA"
    , 'A':|"CATA"
    , 'A':|"ATA"
    , 'A':|"ATA"
    ]


topologyG :: BTree () ()
topologyG =
    Internal blank
    ( Internal blank
      ( Leaf (NodeDatum "A" ()) )
      ( Internal blank
        ( Leaf (NodeDatum "B" ()) )
        ( Internal blank
          ( Leaf (NodeDatum "C" ()) )
          ( Internal blank
            ( Leaf (NodeDatum "D" ()) )
            ( Internal blank
              ( Leaf (NodeDatum "E" ()) )
              ( Leaf (NodeDatum "F" ()) )
            )
          )
        )
      )
    )
    ( Internal blank
      ( Leaf (NodeDatum "G" ()) )
      ( Internal blank
        ( Leaf (NodeDatum "H" ()) )
        ( Internal blank
          ( Leaf (NodeDatum "I" ()) )
          ( Internal blank
            ( Leaf (NodeDatum "J" ()) )
            ( Internal blank
              ( Leaf (NodeDatum "K" ()) )
              ( Leaf (NodeDatum "L" ()) )
             )
          )
        )
      )
    )






dataSetX :: Map String (NonEmpty (NonEmpty String))
dataSetX = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"A"
    , 'A':|"A"
    , 'A':|"TA"
    , 'A':|"TA"
    , 'A':|"TA"
    , 'A':|"A"
    , 'A':|"A"
    , 'A':|"A"
    , 'A':|"TA"
    , 'A':|"TA"
    ]


topologyX :: BTree () ()
topologyX =
    Internal blank
    ( Leaf (NodeDatum "A" ()) )
    ( Internal blank
      ( Leaf (NodeDatum "B" ()) )
      ( Internal blank
        ( Leaf (NodeDatum "C" ()) )
        ( Internal blank
          ( Leaf (NodeDatum "D" ()) )
          ( Internal blank
            ( Leaf (NodeDatum "E" ()) )
            ( Internal blank
              ( Leaf (NodeDatum "F" ()) )
              ( Internal blank
                ( Leaf (NodeDatum "G" ()) )
                ( Internal blank
                  ( Leaf (NodeDatum "H" ()) )
                  ( Internal blank
                    ( Leaf (NodeDatum "I" ()) )
                    ( Leaf (NodeDatum "J" ()) )
                  )
                )
              )
            )
          )
        )
      )
    )


dataSetY :: Map String (NonEmpty (NonEmpty String))
dataSetY = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"ATT"
    , 'A':|"ATT"
    , 'A':|"ATT"
    , 'A':|"ACTT"
    , 'A':|"ACTT"
    , 'A':|"ACTT"
    , 'A':|"ATT"
    , 'A':|"ATT"
    ]


topologyY :: BTree () ()
topologyY =
    Internal blank
    ( Leaf (NodeDatum "A" ()) )
    ( Internal blank
      ( Leaf (NodeDatum "B" ()) )
      ( Internal blank
        ( Leaf (NodeDatum "C" ()) )
        ( Internal blank
          ( Leaf (NodeDatum "D" ()) )
          ( Internal blank
            ( Leaf (NodeDatum "E" ()) )
            ( Internal blank
              ( Leaf (NodeDatum "F" ()) )
              ( Internal blank
                ( Leaf (NodeDatum "G" ()) )
                ( Leaf (NodeDatum "H" ()) )
              )
            )
          )
        )
      )
    )
