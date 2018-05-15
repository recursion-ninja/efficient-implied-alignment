{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies #-}

module SampleData
  ( LeafInput
  , TreeInput
  , defaultAlphabet
  , defaultTripleCompare
  , defaultTCM
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
import           Prelude               hiding (lookup)



--case toEither $ unifyInput dataSetA topologyA of

type LeafInput = Map String (NonEmpty (NonEmpty String)) 


type TreeInput = BTree () ()


sampleDataSets :: [(String, LeafInput, TreeInput)]
sampleDataSets =
    [ ("Appended Deletions" , dataSetA, topologyA)
    , ("Prepended Deletions", dataSetB, topologyB)
    , ("Appended Insertions", dataSetC, topologyC)
    , ("Prepended Deletions", dataSetD, topologyD)
    , ("Single Deleted Insertion", dataSetF, topologyF)
    , ("Non-homology Double Deleted Insertion", dataSetF, topologyF)
    ]


defaultAlphabet :: Alphabet String
defaultAlphabet = fromSymbols $ pure <$> "ACGT-"


defaultTripleCompare :: ThreewayCompare String
defaultTripleCompare = buildThreeWayCompare defaultAlphabet defaultTCM


defaultTCM :: TransitionCostMatrix String
defaultTCM = tcm
  where
    tcm = buildTransitionCostMatrix defaultAlphabet scm
    scm = buildSymbolChangeMatrix   defaultAlphabet fakeParseInput
    fakeParseInput = matrix 5 5 (\(i,j) -> if i == j then 0 else 1)


dataSetA :: Map String (NonEmpty (NonEmpty String))
dataSetA = M.fromList
    [ ("A", toNonEmpties $ 'A':|"CGT")
    , ("B", toNonEmpties $ 'A':|"CG")
    , ("C", toNonEmpties $ 'A':|"C")
    , ("D", toNonEmpties $ 'A':|"")
    ]
  where
    toNonEmpties = foldMap1 (pure . pure . pure) 


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
  where
    blank = NodeDatum "" ()


dataSetB :: Map String (NonEmpty (NonEmpty String))
dataSetB = M.fromList
    [ ("A", toNonEmpties $ 'A':|"")
    , ("B", toNonEmpties $ 'A':|"C")
    , ("C", toNonEmpties $ 'A':|"CG")
    , ("D", toNonEmpties $ 'A':|"CGT")
    ]
  where
    toNonEmpties = foldMap1 (pure . pure . pure) 


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
  where
    blank = NodeDatum "" ()


dataSetC :: Map String (NonEmpty (NonEmpty String))
dataSetC = M.fromList
    [ ("A", toNonEmpties $ 'T':|"GCA")
    , ("B", toNonEmpties $ 'G':|"CA")
    , ("C", toNonEmpties $ 'C':|"A")
    , ("D", toNonEmpties $ 'A':|"")
    ]
  where
    toNonEmpties = foldMap1 (pure . pure . pure) 


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
  where
    blank = NodeDatum "" ()


dataSetD :: Map String (NonEmpty (NonEmpty String))
dataSetD = M.fromList
    [ ("A", toNonEmpties $ 'A':|"")
    , ("B", toNonEmpties $ 'C':|"A")
    , ("C", toNonEmpties $ 'G':|"CA")
    , ("D", toNonEmpties $ 'T':|"GCA")
    ]
  where
    toNonEmpties = foldMap1 (pure . pure . pure) 


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
  where
    blank = NodeDatum "" ()


dataSetE :: Map String (NonEmpty (NonEmpty String))
dataSetE = M.fromList
    [ ("A", toNonEmpties $ 'A':|"A")
    , ("B", toNonEmpties $ 'A':|"A")
    , ("C", toNonEmpties $ 'A':|"TA")
    , ("D", toNonEmpties $ 'A':|"TA")
    , ("E", toNonEmpties $ 'A':|"TA")
    , ("F", toNonEmpties $ 'A':|"A")
    , ("G", toNonEmpties $ 'A':|"A")
    , ("H", toNonEmpties $ 'A':|"A")
    , ("I", toNonEmpties $ 'A':|"TA")
    , ("J", toNonEmpties $ 'A':|"TA")
    ]
  where
    toNonEmpties = foldMap1 (pure . pure . pure) 


topologyE :: BTree () ()
topologyE =
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
  where
    blank = NodeDatum "" ()


dataSetF :: Map String (NonEmpty (NonEmpty String))
dataSetF = M.fromList
    [ ("A", toNonEmpties $ 'A':|"ATT")
    , ("B", toNonEmpties $ 'A':|"ATT")
    , ("C", toNonEmpties $ 'A':|"ATT")
    , ("D", toNonEmpties $ 'A':|"ACTT")
    , ("E", toNonEmpties $ 'A':|"ACTT")
    , ("F", toNonEmpties $ 'A':|"ACTT")
    , ("G", toNonEmpties $ 'A':|"ATT")
    , ("H", toNonEmpties $ 'A':|"ATT")
    ]
  where
    toNonEmpties = foldMap1 (pure . pure . pure) 


topologyF :: BTree () ()
topologyF =
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
  where
    blank = NodeDatum "" ()
