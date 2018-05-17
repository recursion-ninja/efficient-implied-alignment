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
    [ ("Appended Deletions"                                         , dataSetA, topologyA, discreteMetricTCM)
    , ("Prepended Deletions"                                        , dataSetB, topologyB, discreteMetricTCM)
    , ("Appended Insertions"                                        , dataSetC, topologyC, discreteMetricTCM)
    , ("Prepended Deletions"                                        , dataSetD, topologyD, discreteMetricTCM)
    , ("Adjacent Deletion Insertion Events"                         , dataSetE, topologyE, discreteMetricTCM)
    , ("Adjacent Deletion Insertion Events Two"                     , dataSetF, topologyF, discreteMetricTCM)
    , ("Non-homology Deleted Insertion"                             , dataSetG, topologyG, discreteMetricTCM)
    , ("Ambiguous Non-homology Insertions"                          , dataSetH, topologyH, discreteMetricTCM)
    , ("Inserted Deletion"                                          , dataSetI, topologyI, discreteMetricTCM)
    , ("Insertion of Deletion #3"                                   , dataSetJ, topologyJ, discreteMetricTCM)
    , ("Non-homology Dual Insertions"                               , dataSetK, topologyK, discreteMetricTCM)
    , ("Non-homology Double Deleted Insertion"                      , dataSetL, topologyL, discreteMetricTCM)
    , ("Non-homology Dual Insertions With Extra Base"               , dataSetM, topologyM, discreteMetricTCM)
    , ("Deleted Insertion Single"                                   , dataSetN, topologyN, discreteMetricTCM)
    , ("Deleted Insertions Around Middle Group"                     , dataSetO, topologyO, discreteMetricTCM)
    , ("Deleted Insertions Prepended Before Group"                  , dataSetP, topologyP, discreteMetricTCM)
    , ("Deleted Insertions Appended After Group"                    , dataSetQ, topologyQ, discreteMetricTCM)
    , ("Two Adjacent Insertions Simultaneous Deletions"             , dataSetR, topologyR, discreteMetricTCM)
    , ("Two Non-adjacent Insertions Simultaneous Deletions"         , dataSetS, topologyS, discreteMetricTCM)
    , ("Two Adjacent Insertions Seperate Deletions"                 , dataSetT, topologyT, discreteMetricTCM)
    , ("Two Non-adjacent Symetric Insertions Seperate Deletions"    , dataSetU, topologyU, discreteMetricTCM)
    , ("Two Non-adjacent Antisymetric Insertions Seperate Deletions", dataSetV, topologyV, discreteMetricTCM)
    , ("That Darn Truncation Issue"                                 , dataSetW, topologyW, discreteMetricTCM)
    , ("Deletion Before Above Insertion"                            , dataSetX, topologyX, discreteMetricTCM)
    , ("Deletion Before Below Insertion"                            , dataSetY, topologyY, discreteMetricTCM)
    , ("Deletion After Above Insertion"                             , dataSetZ, topologyZ, discreteMetricTCM)
    , ("Deletion After Below Insertion"                             , dataSet0, topology0, discreteMetricTCM)
    , ("Nested Insertions"                                          , dataSet1, topology1, discreteMetricTCM)
    , ("Branches With Adjacent Insertions"                          , dataSet2, topology2, discreteMetricTCM)
    , ("Ambigous Resolution Consistency"                            , dataSet3, topology3, discreteMetricTCM)
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


dataSetH :: Map String (NonEmpty (NonEmpty String))
dataSetH = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"A"
    , 'A':|"A"
    , 'A':|"CA"
    , 'A':|"A"
    , 'A':|"TA"
    , 'A':|"A"
    , 'A':|"A"
    , 'A':|"GA"
    ]


topologyH :: BTree () ()
topologyH =
    Internal blank
    ( Internal blank
      ( Leaf (NodeDatum "A" ()) )
      ( Internal blank
        ( Internal blank
          ( Leaf (NodeDatum "B" ()) )
          ( Leaf (NodeDatum "C" ()) )
        )
        ( Internal blank
          ( Leaf (NodeDatum "D" ()) )
          ( Leaf (NodeDatum "E" ()) )
        )
      )
    )
    ( Internal blank
      ( Leaf (NodeDatum "F" ()) )
      ( Internal blank
        ( Leaf (NodeDatum "G" ()) )
        ( Leaf (NodeDatum "H" ()) )
      )
    )


dataSetI :: Map String (NonEmpty (NonEmpty String))
dataSetI = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"TT"
    , 'A':|"TT"
    , 'A':|"T"
    , 'A':|"T"
    , 'A':|"T"
    , 'A':|"TT"
    , 'A':|"TT"
    ]


topologyI :: BTree () ()
topologyI =
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
              ( Leaf (NodeDatum "G" ()) )
            )
          )
        )
      )
    )


dataSetJ :: Map String (NonEmpty (NonEmpty String))
dataSetJ = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"TTA"
    , 'A':|"TTA"
    , 'A':|"TTA"
    , 'A':|"TA"
    , 'A':|"TA"
    , 'A':|"TA"
    , 'A':|"TTA"
    , 'A':|"TTA"
    ]


topologyJ :: BTree () ()
topologyJ =
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


dataSetK :: Map String (NonEmpty (NonEmpty String))
dataSetK = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"A"
    , 'A':|"A"
    , 'A':|"TA"
    , 'A':|"TA"
    , 'A':|"A"
    , 'A':|"A"
    , 'A':|"TA"
    , 'A':|"TA"
    ]


topologyK :: BTree () ()
topologyK =
    Internal blank
    ( Internal blank
      ( Leaf (NodeDatum "A" ()) )
      ( Internal blank
        ( Leaf (NodeDatum "B" ()) )
        ( Internal blank
          ( Leaf (NodeDatum "C" ()) )
          ( Leaf (NodeDatum "D" ()) )
        )
      )
    )
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


dataSetL :: Map String (NonEmpty (NonEmpty String))
dataSetL = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
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


topologyL :: BTree () ()
topologyL =
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


dataSetM :: Map String (NonEmpty (NonEmpty String))
dataSetM = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"TA"
    , 'A':|"TA"
    , 'A':|"TTA"
    , 'A':|"TTA"
    , 'A':|"TA"
    , 'A':|"TA"
    , 'A':|"TTA"
    , 'A':|"TTA"
    ]


topologyM :: BTree () ()
topologyM =
    Internal blank
    ( Internal blank
      ( Leaf (NodeDatum "A" ()) )
      ( Internal blank
        ( Leaf (NodeDatum "B" ()) )
        ( Internal blank
          ( Leaf (NodeDatum "C" ()) )
          ( Leaf (NodeDatum "D" ()) )
        )
      )
    )
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


dataSetN :: Map String (NonEmpty (NonEmpty String))
dataSetN = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"ATT"
    , 'A':|"ATT"
    , 'A':|"ATT"
    , 'A':|"ACTT"
    , 'A':|"ACTT"
    , 'A':|"ACTT"
    , 'A':|"ATT"
    , 'A':|"ATT"
    ]


topologyN :: BTree () ()
topologyN =
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


dataSetO :: Map String (NonEmpty (NonEmpty String))
dataSetO = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"ATT"
    , 'A':|"ATT"
    , 'A':|"ATT"
    , 'A':|"ACGCTT"
    , 'A':|"ACGCTT"
    , 'A':|"ACGCTT"
    , 'A':|"ACCTT"
    , 'A':|"ACCTT"
    ]


topologyO :: BTree () ()
topologyO =
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


dataSetP :: Map String (NonEmpty (NonEmpty String))
dataSetP = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"ATT"
    , 'A':|"ATT"
    , 'A':|"ATT"
    , 'A':|"AGCCTT"
    , 'A':|"AGCCTT"
    , 'A':|"AGCCTT"
    , 'A':|"ACCTT"
    , 'A':|"ACCTT"
    ]


topologyP :: BTree () ()
topologyP =
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


dataSetQ :: Map String (NonEmpty (NonEmpty String))
dataSetQ = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"ATT"
    , 'A':|"ATT"
    , 'A':|"ATT"
    , 'A':|"ACCGTT"
    , 'A':|"ACCGTT"
    , 'A':|"ACCGTT"
    , 'A':|"ACCTT"
    , 'A':|"ACCTT"
    ]


topologyQ :: BTree () ()
topologyQ =
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


dataSetR :: Map String (NonEmpty (NonEmpty String))
dataSetR = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"ATT"
    , 'A':|"ATT"
    , 'A':|"ATT"
    , 'A':|"ACTT"
    , 'A':|"ACTT"
    , 'A':|"ACTT"
    , 'A':|"ACCTT"
    , 'A':|"ACCTT"
    , 'A':|"ACCTT"
    , 'A':|"ATT"
    , 'A':|"ATT"
    ]


topologyR :: BTree () ()
topologyR =
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
                    ( Internal blank
                      ( Leaf (NodeDatum "J" ()) )
                      ( Leaf (NodeDatum "K" ()) )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )


dataSetS :: Map String (NonEmpty (NonEmpty String))
dataSetS = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"AGTT"
    , 'A':|"AGTT"
    , 'A':|"AGTT"
    , 'A':|"ACGTT"
    , 'A':|"ACGTT"
    , 'A':|"ACGTT"
    , 'A':|"ACGCTT"
    , 'A':|"ACGCTT"
    , 'A':|"ACGCTT"
    , 'A':|"ATT"
    , 'A':|"ATT"
    ]


topologyS :: BTree () ()
topologyS =
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
                    ( Internal blank
                      ( Leaf (NodeDatum "J" ()) )
                      ( Leaf (NodeDatum "K" ()) )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )


dataSetT :: Map String (NonEmpty (NonEmpty String))
dataSetT = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"ATT"
    , 'A':|"ATT"
    , 'A':|"ATT"
    , 'A':|"ACTT"
    , 'A':|"ACTT"
    , 'A':|"ACTT"
    , 'A':|"ACCTT"
    , 'A':|"ACCTT"
    , 'A':|"ACCTT"
    , 'A':|"ACTT"
    , 'A':|"ACTT"
    , 'A':|"ACTT"
    , 'A':|"ATT"
    , 'A':|"ATT"
    ]


topologyT :: BTree () ()
topologyT =
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
                    ( Internal blank
                      ( Leaf (NodeDatum "J" ()) )
                      ( Internal blank
                        ( Leaf (NodeDatum "K" ()) )
                        ( Internal blank
                          ( Leaf (NodeDatum "L" ()) )
                          ( Internal blank
                              ( Leaf (NodeDatum "M" ()) )
                              ( Leaf (NodeDatum "N" ()) )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )


dataSetU :: Map String (NonEmpty (NonEmpty String))
dataSetU = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"AGTT"
    , 'A':|"AGTT"
    , 'A':|"AGTT"
    , 'A':|"ACGTT"
    , 'A':|"ACGTT"
    , 'A':|"ACGTT"
    , 'A':|"ACGCTT"
    , 'A':|"ACGCTT"
    , 'A':|"ACGCTT"
    , 'A':|"ACGTT"
    , 'A':|"ACGTT"
    , 'A':|"ACGTT"
    , 'A':|"AGTT"
    , 'A':|"AGTT"
    ]


topologyU :: BTree () ()
topologyU =
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
                    ( Internal blank
                      ( Leaf (NodeDatum "J" ()) )
                      ( Internal blank
                        ( Leaf (NodeDatum "K" ()) )
                        ( Internal blank
                          ( Leaf (NodeDatum "L" ()) )
                          ( Internal blank
                              ( Leaf (NodeDatum "M" ()) )
                              ( Leaf (NodeDatum "N" ()) )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )


dataSetV :: Map String (NonEmpty (NonEmpty String))
dataSetV = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"AGTT"
    , 'A':|"AGTT"
    , 'A':|"AGTT"
    , 'A':|"ACGTT"
    , 'A':|"ACGTT"
    , 'A':|"ACGTT"
    , 'A':|"ACGCTT"
    , 'A':|"ACGCTT"
    , 'A':|"ACGCTT"
    , 'A':|"AGCTT"
    , 'A':|"AGCTT"
    , 'A':|"AGCTT"
    , 'A':|"AGTT"
    , 'A':|"AGTT"
    ]


topologyV :: BTree () ()
topologyV =
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
                    ( Internal blank
                      ( Leaf (NodeDatum "J" ()) )
                      ( Internal blank
                        ( Leaf (NodeDatum "K" ()) )
                        ( Internal blank
                          ( Leaf (NodeDatum "L" ()) )
                          ( Internal blank
                              ( Leaf (NodeDatum "M" ()) )
                              ( Leaf (NodeDatum "N" ()) )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )


dataSetW :: Map String (NonEmpty (NonEmpty String))
dataSetW = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"T"
    , 'A':|"T"
    , 'A':|"CT"
    , 'A':|"CT"
    , 'A':|"CCT"
    , 'A':|"CCCT"
    ]


topologyW :: BTree () ()
topologyW =
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
            ( Leaf (NodeDatum "F" ()) )
          )
        )
      )
    )


dataSetX :: Map String (NonEmpty (NonEmpty String))
dataSetX = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'G':|"AA"
    , 'G':|"AA"
    , 'A':|"A"
    , 'A':|"A"
    , 'A':|"A"
    , 'A':|"CA"
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
            ( Leaf (NodeDatum "F" ()) )
          )
        )
      )
    )


dataSetY :: Map String (NonEmpty (NonEmpty String))
dataSetY = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'G':|"AA"
    , 'G':|"AA"
    , 'G':|"ACA"
    , 'G':|"ACA"
    , 'G':|"ACA"
    , 'A':|"CA"
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
            ( Leaf (NodeDatum "F" ()) )
          )
        )
      )
    )


dataSetZ :: Map String (NonEmpty (NonEmpty String))
dataSetZ = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"AG"
    , 'A':|"AG"
    , 'A':|"A"
    , 'A':|"A"
    , 'A':|"A"
    , 'A':|"CA"
    ]


topologyZ :: BTree () ()
topologyZ =
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
            ( Leaf (NodeDatum "F" ()) )
          )
        )
      )
    )


dataSet0 :: Map String (NonEmpty (NonEmpty String))
dataSet0 = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"AG"
    , 'A':|"AG"
    , 'A':|"CAG"
    , 'A':|"CAG"
    , 'A':|"CAG"
    , 'A':|"CA"
    ]


topology0 :: BTree () ()
topology0 =
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
            ( Leaf (NodeDatum "F" ()) )
          )
        )
      )
    )


dataSet1 :: Map String (NonEmpty (NonEmpty String))
dataSet1 = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"A"
    , 'A':|"A"
    , 'A':|"CCA"
    , 'A':|"CCA"
    , 'A':|"CCA"
    , 'A':|"CGCA"
    ]


topology1 :: BTree () ()
topology1 =
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
            ( Leaf (NodeDatum "F" ()) )
          )
        )
      )
    )


dataSet2 :: Map String (NonEmpty (NonEmpty String))
dataSet2 = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"AA"
    , 'A':|"AA"
    , 'A':|"CACA"
    , 'A':|"CACA"
    , 'A':|"CACA"
    , 'A':|"CAGCA"
    , 'A':|"CACA"
    , 'A':|"CACA"
    , 'A':|"CGACA"
    ]


topology2 :: BTree () ()
topology2 =
    Internal blank
    ( Leaf (NodeDatum "A" ()) )
    ( Internal blank
      ( Leaf (NodeDatum "B" ()) )
      ( Internal blank
        ( Leaf (NodeDatum "C" ()) )
        ( Internal blank
          ( Internal blank
            ( Leaf (NodeDatum "D" ()) )
            ( Internal blank
              ( Leaf (NodeDatum "E" ()) )
              ( Leaf (NodeDatum "F" ()) )
            )
          )
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


dataSet3 :: Map String (NonEmpty (NonEmpty String))
dataSet3 = fmap toNonEmpties . M.fromList $ zip (pure <$> ['A'..])
    [ 'A':|"A"
    , 'A':|"A"
    , 'A':|"CA"
    , 'A':|"CA"
    , 'A':|"CA"
    , 'A':|"GCTCA"
    ]


topology3 :: BTree () ()
topology3 =
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
            ( Leaf (NodeDatum "F" ()) )
          )
        )
      )
    )
