{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# Language TypeFamilies #-}

module SampleData
    ( LeafInput
    , LeafOutputs
    , StringValues
    , TreeInput
    , defaultAlphabet
      --  , defaultTripleCompare
      --  , discreteMetricTCM
    , sampleDataSets
    ) where

import Alignment
import Control.Arrow
import Control.Lens
import Data.Alphabet
import Data.BTree
import Data.Char
import Data.Decoration
import Data.Foldable
import Data.Functor (($>))
import Data.Key
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Matrix qualified as Mat
import Data.Pointed
import Data.Semigroup ((<>))
import Data.Semigroup.Foldable
import Data.Set (Set)
import Data.String (IsString(..))
import Data.SymbolString
import Data.TCM
import Data.Text (Text)
import Data.Validation
import Data.Vector.Unboxed.NonEmpty (Vector)
import Data.Vector.Unboxed.NonEmpty qualified as V
import Prelude hiding (zip)


type LeafInput = NonEmpty (Vector Char)


type LeafOutputs = NonEmpty LeafInput


type StringValues = Map Text (LeafInput, LeafOutputs)


type TreeInput = BTree () ()


sampleDataSets :: [(String, StringValues, TreeInput, TransitionCostMatrix)]
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
    , ("Two Adjacent Insertions Separate Deletions"                 , dataSetT, topologyT, discreteMetricTCM)
    , ("Two Non-adjacent Symmetric Insertions Separate Deletions"   , dataSetU, topologyU, discreteMetricTCM)
    , ("Two Non-adjacent Antisymetric Insertions Separate Deletions", dataSetV, topologyV, discreteMetricTCM)
    , ("That Darn Truncation Issue"                                 , dataSetW, topologyW, discreteMetricTCM)
    , ("Deletion Before Above Insertion"                            , dataSetX, topologyX, discreteMetricTCM)
    , ("Deletion Before Below Insertion"                            , dataSetY, topologyY, discreteMetricTCM)
    , ("Deletion After Above Insertion"                             , dataSetZ, topologyZ, discreteMetricTCM)
    , ("Deletion After Below Insertion"                             , dataSet0, topology0, discreteMetricTCM)
    , ("Nested Insertions"                                          , dataSet1, topology1, discreteMetricTCM)
    , ("Branches With Adjacent Insertions"                          , dataSet2, topology2, discreteMetricTCM)
    , ("Ambiguous Resolution Consistency"                           , dataSet3, topology3, discreteMetricTCM)
    ]


defaultAlphabet :: Alphabet Char
defaultAlphabet = fromSymbols ("ACGT-" :: String)


{-
defaultTripleCompare :: ThreewayCompare Char
defaultTripleCompare = buildThreeWayCompare defaultAlphabet discreteMetricTCM
-}


discreteMetricTCM :: TransitionCostMatrix
discreteMetricTCM = tcm
    where
        tcm            = buildTransitionCostMatrix defaultAlphabet scm
        scm            = buildSymbolChangeMatrix fakeParseInput
        fakeParseInput = Mat.fromList (5, 5) [ if i == j then 0 else 1 | i <- [0 .. 4], j <- [0 .. 4] ]


preferGapsTCM :: TransitionCostMatrix
preferGapsTCM = tcm
    where
        tcm            = buildTransitionCostMatrix defaultAlphabet scm
        scm            = buildSymbolChangeMatrix fakeParseInput
        fakeParseInput = Mat.fromList (5, 5) [ f i j | i <- [0 .. 4], j <- [0 .. 4] ]
            where
                f i j
                    | i == j    = 0
                    | i == 4    = 1
                    | j == 4    = 1
                    | otherwise = 2


blank :: NodeDatum ()
blank = NodeDatum "" ()


toNonEmpties :: Foldable f => f Char -> NonEmpty (Vector Char)
toNonEmpties = foldMap1 (pure . V.fromNonEmpty . (:| [])) . NE.fromList . toList


toNonEmptyInputs :: Foldable f => f Char -> NonEmpty (Vector Char)
toNonEmptyInputs = toNonEmpties


toNonEmptyOutputs :: (Foldable t, Foldable f) => t (f Char) -> NonEmpty (NonEmpty (Vector Char))
toNonEmptyOutputs = fmap toNonEmpties . NE.fromList . toList


--convertToValues :: (Foldable f, Foldable g, Foldable t) => [(f Char, t (g Char))] -> StringValues
convertToValues :: [(NonEmpty Char, [String])] -> StringValues
convertToValues =
    fmap (toNonEmptyInputs *** toNonEmptyOutputs) . M.fromList . zip (fromString . pure <$> ['A' ..])


dataSetA :: StringValues
dataSetA
    = convertToValues
    [('A' :| "CGT", ["ACGT"]), ('A' :| "CG", ["ACG-"]), ('A' :| "C", ["AC--"]), ('A' :| "", ["A---"])]


topologyA :: BTree () ()
topologyA = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal blank (Leaf (NodeDatum "C" ())) (Leaf (NodeDatum "D" ())))
    )


dataSetB :: StringValues
dataSetB
    = convertToValues
    [('A' :| "", ["A---"]), ('A' :| "C", ["AC--"]), ('A' :| "CG", ["ACG-"]), ('A' :| "CGT", ["ACGT"])]


topologyB :: BTree () ()
topologyB = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal blank (Leaf (NodeDatum "C" ())) (Leaf (NodeDatum "D" ())))
    )


dataSetC :: StringValues
dataSetC
    = convertToValues
    [('T' :| "GCA", ["TGCA"]), ('G' :| "CA", ["-GCA"]), ('C' :| "A", ["--CA"]), ('A' :| "", ["---A"])]


topologyC :: BTree () ()
topologyC = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal blank (Leaf (NodeDatum "C" ())) (Leaf (NodeDatum "D" ())))
    )


dataSetD :: StringValues
dataSetD
    = convertToValues
    [('A' :| "", ["---A"]), ('C' :| "A", ["--CA"]), ('G' :| "CA", ["-GCA"]), ('T' :| "GCA", ["TGCA"])]


topologyD :: BTree () ()
topologyD = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal blank (Leaf (NodeDatum "C" ())) (Leaf (NodeDatum "D" ())))
    )


dataSetE :: StringValues
dataSetE
    = convertToValues
    [ ('A' :| "TA", ["A-TA", "AT-A"])
    , ('A' :| "TA", ["A-TA", "AT-A"])
    , ('A' :| "TA", ["A-TA", "AT-A"])
    , ('A' :| "A" , ["A--A", "A--A"])
    , ('A' :| "A" , ["A--A", "A--A"])
    , ('A' :| "A" , ["A--A", "A--A"])
    , ('A' :| "CA", ["AC-A", "A-CA"])
    , ('A' :| "CA", ["AC-A", "A-CA"])
    ]


topologyE :: BTree () ()
topologyE = Internal
    blank
    (Internal blank (Leaf (NodeDatum "A" ())) (Leaf (NodeDatum "B" ())))
    (Internal
        blank
        (Leaf (NodeDatum "C" ()))
        (Internal
            blank
            (Leaf (NodeDatum "D" ()))
            (Internal
                blank
                (Leaf (NodeDatum "E" ()))
                (Internal
                    blank
                    (Leaf (NodeDatum "F" ()))
                    (Internal blank (Leaf (NodeDatum "G" ())) (Leaf (NodeDatum "H" ())))
                )
            )
        )
    )


dataSetF :: StringValues
dataSetF
    = convertToValues
    [ ('A' :| "TTA" , ["AT-T-A", "A-TT-A", "AT-TA-", "A-TTA-"])
    , ('A' :| "TTA" , ["AT-T-A", "A-TT-A", "AT-TA-", "A-TTA-"])
    , ('A' :| "TTA" , ["AT-T-A", "A-TT-A", "AT-TA-", "A-TTA-"])
    , ('A' :| "TTA" , ["AT-T-A", "A-TT-A", "AT-TA-", "A-TTA-"])
    , ('A' :| "TA"  , ["A--T-A", "A--T-A", "A--TA-", "A--TA-"])
    , ('A' :| "TA"  , ["A--T-A", "A--T-A", "A--TA-", "A--TA-"])
    , ('A' :| "TA"  , ["A--T-A", "A--T-A", "A--TA-", "A--TA-"])
    , ('A' :| "CTAA", ["A-CTAA", "AC-TAA", "A-CTAA", "AC-TAA"])
    , ('A' :| "CTAA", ["A-CTAA", "AC-TAA", "A-CTAA", "AC-TAA"])
    ]


topologyF :: BTree () ()
topologyF = Internal
    blank
    (Internal blank (Leaf (NodeDatum "A" ())) (Leaf (NodeDatum "B" ())))
    (Internal
        blank
        (Leaf (NodeDatum "C" ()))
        (Internal
            blank
            (Leaf (NodeDatum "D" ()))
            (Internal
                blank
                (Leaf (NodeDatum "E" ()))
                (Internal
                    blank
                    (Leaf (NodeDatum "F" ()))
                    (Internal
                        blank
                        (Leaf (NodeDatum "G" ()))
                        (Internal blank (Leaf (NodeDatum "H" ())) (Leaf (NodeDatum "I" ())))
                    )
                )
            )
        )
    )


dataSetG :: StringValues
dataSetG
    = convertToValues
    [ ('A' :| "AA"  , ["A--A--A", "A--A--A", "A--A--A", "A--A--A"])
    , ('A' :| "AA"  , ["A--A--A", "A--A--A", "A--A--A", "A--A--A"])
    , ('A' :| "CATA", ["AC-AT-A", "A-CAT-A", "AC-A-TA", "A-CA-TA"])
    , ('A' :| "CATA", ["AC-AT-A", "A-CAT-A", "AC-A-TA", "A-CA-TA"])
    , ('A' :| "CAA" , ["AC-A--A", "A-CA--A", "AC-A--A", "A-CA--A"])
    , ('A' :| "CAA" , ["AC-A--A", "A-CA--A", "AC-A--A", "A-CA--A"])
    , ('A' :| "AA"  , ["A--A--A", "A--A--A", "A--A--A", "A--A--A"])
    , ('A' :| "AA"  , ["A--A--A", "A--A--A", "A--A--A", "A--A--A"])
    , ('A' :| "CATA", ["A-CA-TA", "AC-A-TA", "A-CAT-A", "AC-AT-A"])
    , ('A' :| "CATA", ["A-CA-TA", "AC-A-TA", "A-CAT-A", "AC-AT-A"])
    , ('A' :| "ATA" , ["A--A-TA", "A--A-TA", "A--AT-A", "A--AT-A"])
    , ('A' :| "ATA" , ["A--A-TA", "A--A-TA", "A--AT-A", "A--AT-A"])
    ]


topologyG :: BTree () ()
topologyG = Internal
    blank
    (Internal
        blank
        (Leaf (NodeDatum "A" ()))
        (Internal
            blank
            (Leaf (NodeDatum "B" ()))
            (Internal
                blank
                (Leaf (NodeDatum "C" ()))
                (Internal
                    blank
                    (Leaf (NodeDatum "D" ()))
                    (Internal blank (Leaf (NodeDatum "E" ())) (Leaf (NodeDatum "F" ())))
                )
            )
        )
    )
    (Internal
        blank
        (Leaf (NodeDatum "G" ()))
        (Internal
            blank
            (Leaf (NodeDatum "H" ()))
            (Internal
                blank
                (Leaf (NodeDatum "I" ()))
                (Internal
                    blank
                    (Leaf (NodeDatum "J" ()))
                    (Internal blank (Leaf (NodeDatum "K" ())) (Leaf (NodeDatum "L" ())))
                )
            )
        )
    )


dataSetH :: StringValues
dataSetH
    = convertToValues
    [ ('A' :| "A" , ["A---A", "A---A", "A---A", "A---A", "A---A", "A---A"])
    , ('A' :| "A" , ["A---A", "A---A", "A---A", "A---A", "A---A", "A---A"])
    , ('A' :| "CA", ["AC--A", "AC--A", "A-C-A", "A-C-A", "A--CA", "A--CA"])
    , ('A' :| "A" , ["A---A", "A---A", "A---A", "A---A", "A---A", "A---A"])
    , ('A' :| "TA", ["A-T-A", "A--TA", "AT--A", "A--TA", "AT--A", "A-T-A"])
    , ('A' :| "A" , ["A---A", "A---A", "A---A", "A---A", "A---A", "A---A"])
    , ('A' :| "A" , ["A---A", "A---A", "A---A", "A---A", "A---A", "A---A"])
    , ('A' :| "GA", ["A--GA", "A-G-A", "A--GA", "AG--A", "A-G-A", "AG--A"])
    ]


topologyH :: BTree () ()
topologyH = Internal
    blank
    (Internal
        blank
        (Leaf (NodeDatum "A" ()))
        (Internal
            blank
            (Internal blank (Leaf (NodeDatum "B" ())) (Leaf (NodeDatum "C" ())))
            (Internal blank (Leaf (NodeDatum "D" ())) (Leaf (NodeDatum "E" ())))
        )
    )
    (Internal
        blank
        (Leaf (NodeDatum "F" ()))
        (Internal blank (Leaf (NodeDatum "G" ())) (Leaf (NodeDatum "H" ())))
    )


dataSetI :: StringValues
dataSetI
    = convertToValues
    [ ('A' :| "TT", ["AT-T", "ATT-", "A-TT", "ATT-", "A-TT", "AT-T"])
    , ('A' :| "TT", ["AT-T", "ATT-", "A-TT", "ATT-", "A-TT", "AT-T"])
    , ('A' :| "T" , ["A--T", "A-T-", "A--T", "AT--", "A-T-", "AT--"])
    , ('A' :| "T" , ["A--T", "A-T-", "A--T", "AT--", "A-T-", "AT--"])
    , ('A' :| "T" , ["A--T", "A-T-", "A--T", "AT--", "A-T-", "AT--"])
    , ('A' :| "TT", ["A-TT", "A-TT", "AT-T", "AT-T", "ATT-", "ATT-"])
    , ('A' :| "TT", ["A-TT", "A-TT", "AT-T", "AT-T", "ATT-", "ATT-"])
    ]


topologyI :: BTree () ()
topologyI = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal
            blank
            (Leaf (NodeDatum "C" ()))
            (Internal
                blank
                (Leaf (NodeDatum "D" ()))
                (Internal
                    blank
                    (Leaf (NodeDatum "E" ()))
                    (Internal blank (Leaf (NodeDatum "F" ())) (Leaf (NodeDatum "G" ())))
                )
            )
        )
    )


dataSetJ :: StringValues
dataSetJ
    = convertToValues
    [ ('A' :| "TTA", ["A-TTA", "A-TTA", "AT-TA", "AT-TA", "ATT-A", "ATT-A"])
    , ('A' :| "TTA", ["A-TTA", "A-TTA", "AT-TA", "AT-TA", "ATT-A", "ATT-A"])
    , ('A' :| "TTA", ["A-TTA", "A-TTA", "AT-TA", "AT-TA", "ATT-A", "ATT-A"])
    , ('A' :| "TA" , ["A--TA", "A-T-A", "A--TA", "AT--A", "A-T-A", "AT--A"])
    , ('A' :| "TA" , ["A--TA", "A-T-A", "A--TA", "AT--A", "A-T-A", "AT--A"])
    , ('A' :| "TA" , ["A--TA", "A-T-A", "A--TA", "AT--A", "A-T-A", "AT--A"])
    , ('A' :| "TTA", ["AT-TA", "ATT-A", "A-TTA", "ATT-A", "A-TTA", "AT-TA"])
    , ('A' :| "TTA", ["AT-TA", "ATT-A", "A-TTA", "ATT-A", "A-TTA", "AT-TA"])
    ]


topologyJ :: BTree () ()
topologyJ = Internal
    blank
    (Internal blank (Leaf (NodeDatum "A" ())) (Leaf (NodeDatum "B" ())))
    (Internal
        blank
        (Leaf (NodeDatum "C" ()))
        (Internal
            blank
            (Leaf (NodeDatum "D" ()))
            (Internal
                blank
                (Leaf (NodeDatum "E" ()))
                (Internal
                    blank
                    (Leaf (NodeDatum "F" ()))
                    (Internal blank (Leaf (NodeDatum "G" ())) (Leaf (NodeDatum "H" ())))
                )
            )
        )
    )


dataSetK :: StringValues
dataSetK
    = convertToValues
    [ ('A' :| "A" , ["A--A", "A--A"])
    , ('A' :| "A" , ["A--A", "A--A"])
    , ('A' :| "TA", ["AT-A", "A-TA"])
    , ('A' :| "TA", ["AT-A", "A-TA"])
    , ('A' :| "A" , ["A--A", "A--A"])
    , ('A' :| "A" , ["A--A", "A--A"])
    , ('A' :| "TA", ["A-TA", "AT-A"])
    , ('A' :| "TA", ["A-TA", "AT-A"])
    ]


topologyK :: BTree () ()
topologyK = Internal
    blank
    (Internal
        blank
        (Leaf (NodeDatum "A" ()))
        (Internal
            blank
            (Leaf (NodeDatum "B" ()))
            (Internal blank (Leaf (NodeDatum "C" ())) (Leaf (NodeDatum "D" ())))
        )
    )
    (Internal
        blank
        (Leaf (NodeDatum "E" ()))
        (Internal
            blank
            (Leaf (NodeDatum "F" ()))
            (Internal blank (Leaf (NodeDatum "G" ())) (Leaf (NodeDatum "H" ())))
        )
    )


dataSetL :: StringValues
dataSetL
    = convertToValues
    [ ('A' :| "A" , ["A--A", "A--A"])
    , ('A' :| "A" , ["A--A", "A--A"])
    , ('A' :| "TA", ["AT-A", "A-TA"])
    , ('A' :| "TA", ["AT-A", "A-TA"])
    , ('A' :| "TA", ["AT-A", "A-TA"])
    , ('A' :| "A" , ["A--A", "A--A"])
    , ('A' :| "A" , ["A--A", "A--A"])
    , ('A' :| "A" , ["A--A", "A--A"])
    , ('A' :| "TA", ["A-TA", "AT-A"])
    , ('A' :| "TA", ["A-TA", "AT-A"])
    ]


topologyL :: BTree () ()
topologyL = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal
            blank
            (Leaf (NodeDatum "C" ()))
            (Internal
                blank
                (Leaf (NodeDatum "D" ()))
                (Internal
                    blank
                    (Leaf (NodeDatum "E" ()))
                    (Internal
                        blank
                        (Leaf (NodeDatum "F" ()))
                        (Internal
                            blank
                            (Leaf (NodeDatum "G" ()))
                            (Internal
                                blank
                                (Leaf (NodeDatum "H" ()))
                                (Internal blank (Leaf (NodeDatum "I" ())) (Leaf (NodeDatum "J" ())))
                            )
                        )
                    )
                )
            )
        )
    )


dataSetM :: StringValues
dataSetM
    = convertToValues
    [ ('A' :| "TA" , ["A--TA", "A-T-A", "A--TA", "AT--A", "A-T-A", "AT--A"])
    , ('A' :| "TA" , ["A--TA", "A-T-A", "A--TA", "AT--A", "A-T-A", "AT--A"])
    , ('A' :| "TTA", ["AT-TA", "ATT-A", "A-TTA", "ATT-A", "A-TTA", "AT-TA"])
    , ('A' :| "TTA", ["AT-TA", "ATT-A", "A-TTA", "ATT-A", "A-TTA", "AT-TA"])
    , ('A' :| "TA" , ["A--TA", "A-T-A", "A--TA", "AT--A", "A-T-A", "AT--A"])
    , ('A' :| "TA" , ["A--TA", "A-T-A", "A--TA", "AT--A", "A-T-A", "AT--A"])
    , ('A' :| "TTA", ["A-TTA", "A-TTA", "AT-TA", "AT-TA", "ATT-A", "ATT-A"])
    , ('A' :| "TTA", ["A-TTA", "A-TTA", "AT-TA", "AT-TA", "ATT-A", "ATT-A"])
    ]


topologyM :: BTree () ()
topologyM = Internal
    blank
    (Internal
        blank
        (Leaf (NodeDatum "A" ()))
        (Internal
            blank
            (Leaf (NodeDatum "B" ()))
            (Internal blank (Leaf (NodeDatum "C" ())) (Leaf (NodeDatum "D" ())))
        )
    )
    (Internal
        blank
        (Leaf (NodeDatum "E" ()))
        (Internal
            blank
            (Leaf (NodeDatum "F" ()))
            (Internal blank (Leaf (NodeDatum "G" ())) (Leaf (NodeDatum "H" ())))
        )
    )


dataSetN :: StringValues
dataSetN
    = convertToValues
    [ ('A' :| "ATT" , ["AA-TT"])
    , ('A' :| "ATT" , ["AA-TT"])
    , ('A' :| "ATT" , ["AA-TT"])
    , ('A' :| "ACTT", ["AACTT"])
    , ('A' :| "ACTT", ["AACTT"])
    , ('A' :| "ACTT", ["AACTT"])
    , ('A' :| "ATT" , ["AA-TT"])
    , ('A' :| "ATT" , ["AA-TT"])
    ]


topologyN :: BTree () ()
topologyN = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal
            blank
            (Leaf (NodeDatum "C" ()))
            (Internal
                blank
                (Leaf (NodeDatum "D" ()))
                (Internal
                    blank
                    (Leaf (NodeDatum "E" ()))
                    (Internal
                        blank
                        (Leaf (NodeDatum "F" ()))
                        (Internal blank (Leaf (NodeDatum "G" ())) (Leaf (NodeDatum "H" ())))
                    )
                )
            )
        )
    )


dataSetO :: StringValues
dataSetO
    = convertToValues
    [ ('A' :| "ATT"   , ["AA---TT"])
    , ('A' :| "ATT"   , ["AA---TT"])
    , ('A' :| "ATT"   , ["AA---TT"])
    , ('A' :| "ACGCTT", ["AACGCTT"])
    , ('A' :| "ACGCTT", ["AACGCTT"])
    , ('A' :| "ACGCTT", ["AACGCTT"])
    , ('A' :| "ACCTT" , ["AAC-CTT"])
    , ('A' :| "ACCTT" , ["AAC-CTT"])
    ]


topologyO :: BTree () ()
topologyO = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal
            blank
            (Leaf (NodeDatum "C" ()))
            (Internal
                blank
                (Leaf (NodeDatum "D" ()))
                (Internal
                    blank
                    (Leaf (NodeDatum "E" ()))
                    (Internal
                        blank
                        (Leaf (NodeDatum "F" ()))
                        (Internal blank (Leaf (NodeDatum "G" ())) (Leaf (NodeDatum "H" ())))
                    )
                )
            )
        )
    )


dataSetP :: StringValues
dataSetP
    = convertToValues
    [ ('A' :| "ATT"   , ["AA---TT"])
    , ('A' :| "ATT"   , ["AA---TT"])
    , ('A' :| "ATT"   , ["AA---TT"])
    , ('A' :| "AGCCTT", ["AAGCCTT"])
    , ('A' :| "AGCCTT", ["AAGCCTT"])
    , ('A' :| "AGCCTT", ["AAGCCTT"])
    , ('A' :| "ACCTT" , ["AA-CCTT"])
    , ('A' :| "ACCTT" , ["AA-CCTT"])
    ]


topologyP :: BTree () ()
topologyP = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal
            blank
            (Leaf (NodeDatum "C" ()))
            (Internal
                blank
                (Leaf (NodeDatum "D" ()))
                (Internal
                    blank
                    (Leaf (NodeDatum "E" ()))
                    (Internal
                        blank
                        (Leaf (NodeDatum "F" ()))
                        (Internal blank (Leaf (NodeDatum "G" ())) (Leaf (NodeDatum "H" ())))
                    )
                )
            )
        )
    )


dataSetQ :: StringValues
dataSetQ
    = convertToValues
    [ ('A' :| "ATT"   , ["AA---TT"])
    , ('A' :| "ATT"   , ["AA---TT"])
    , ('A' :| "ATT"   , ["AA---TT"])
    , ('A' :| "ACCGTT", ["AACCGTT"])
    , ('A' :| "ACCGTT", ["AACCGTT"])
    , ('A' :| "ACCGTT", ["AACCGTT"])
    , ('A' :| "ACCTT" , ["AACC-TT"])
    , ('A' :| "ACCTT" , ["AACC-TT"])
    ]


topologyQ :: BTree () ()
topologyQ = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal
            blank
            (Leaf (NodeDatum "C" ()))
            (Internal
                blank
                (Leaf (NodeDatum "D" ()))
                (Internal
                    blank
                    (Leaf (NodeDatum "E" ()))
                    (Internal
                        blank
                        (Leaf (NodeDatum "F" ()))
                        (Internal blank (Leaf (NodeDatum "G" ())) (Leaf (NodeDatum "H" ())))
                    )
                )
            )
        )
    )


dataSetR :: StringValues
dataSetR
    = convertToValues
    [ ('A' :| "ATT"  , ["AA--TT", "AA--TT"])
    , ('A' :| "ATT"  , ["AA--TT", "AA--TT"])
    , ('A' :| "ATT"  , ["AA--TT", "AA--TT"])
    , ('A' :| "ACTT" , ["AA-CTT", "AAC-TT"])
    , ('A' :| "ACTT" , ["AA-CTT", "AAC-TT"])
    , ('A' :| "ACTT" , ["AA-CTT", "AAC-TT"])
    , ('A' :| "ACCTT", ["AACCTT", "AACCTT"])
    , ('A' :| "ACCTT", ["AACCTT", "AACCTT"])
    , ('A' :| "ACCTT", ["AACCTT", "AACCTT"])
    , ('A' :| "ATT"  , ["AA--TT", "AA--TT"])
    , ('A' :| "ATT"  , ["AA--TT", "AA--TT"])
    ]


topologyR :: BTree () ()
topologyR = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal
            blank
            (Leaf (NodeDatum "C" ()))
            (Internal
                blank
                (Leaf (NodeDatum "D" ()))
                (Internal
                    blank
                    (Leaf (NodeDatum "E" ()))
                    (Internal
                        blank
                        (Leaf (NodeDatum "F" ()))
                        (Internal
                            blank
                            (Leaf (NodeDatum "G" ()))
                            (Internal
                                blank
                                (Leaf (NodeDatum "H" ()))
                                (Internal
                                    blank
                                    (Leaf (NodeDatum "I" ()))
                                    (Internal blank (Leaf (NodeDatum "J" ())) (Leaf (NodeDatum "K" ())))
                                )
                            )
                        )
                    )
                )
            )
        )
    )


dataSetS :: StringValues
dataSetS
    = convertToValues
    [ ('A' :| "AGTT"  , ["AA-G-TT"])
    , ('A' :| "AGTT"  , ["AA-G-TT"])
    , ('A' :| "AGTT"  , ["AA-G-TT"])
    , ('A' :| "ACGTT" , ["AACG-TT"])
    , ('A' :| "ACGTT" , ["AACG-TT"])
    , ('A' :| "ACGTT" , ["AACG-TT"])
    , ('A' :| "ACGCTT", ["AACGCTT"])
    , ('A' :| "ACGCTT", ["AACGCTT"])
    , ('A' :| "ACGCTT", ["AACGCTT"])
    , ('A' :| "ATT"   , ["AA---TT"])
    , ('A' :| "ATT"   , ["AA---TT"])
    ]


topologyS :: BTree () ()
topologyS = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal
            blank
            (Leaf (NodeDatum "C" ()))
            (Internal
                blank
                (Leaf (NodeDatum "D" ()))
                (Internal
                    blank
                    (Leaf (NodeDatum "E" ()))
                    (Internal
                        blank
                        (Leaf (NodeDatum "F" ()))
                        (Internal
                            blank
                            (Leaf (NodeDatum "G" ()))
                            (Internal
                                blank
                                (Leaf (NodeDatum "H" ()))
                                (Internal
                                    blank
                                    (Leaf (NodeDatum "I" ()))
                                    (Internal blank (Leaf (NodeDatum "J" ())) (Leaf (NodeDatum "K" ())))
                                )
                            )
                        )
                    )
                )
            )
        )
    )


dataSetT :: StringValues
dataSetT
    = convertToValues
    [ ('A' :| "ATT"  , ["AA--TT", "AA--TT", "AA--TT", "AA--TT"])
    , ('A' :| "ATT"  , ["AA--TT", "AA--TT", "AA--TT", "AA--TT"])
    , ('A' :| "ATT"  , ["AA--TT", "AA--TT", "AA--TT", "AA--TT"])
    , ('A' :| "ACTT" , ["AA-CTT", "AA-CTT", "AAC-TT", "AAC-TT"])
    , ('A' :| "ACTT" , ["AA-CTT", "AA-CTT", "AAC-TT", "AAC-TT"])
    , ('A' :| "ACTT" , ["AA-CTT", "AA-CTT", "AAC-TT", "AAC-TT"])
    , ('A' :| "ACCTT", ["AACCTT", "AACCTT", "AACCTT", "AACCTT"])
    , ('A' :| "ACCTT", ["AACCTT", "AACCTT", "AACCTT", "AACCTT"])
    , ('A' :| "ACCTT", ["AACCTT", "AACCTT", "AACCTT", "AACCTT"])
    , ('A' :| "ACTT" , ["AA-CTT", "AAC-TT", "AA-CTT", "AAC-TT"])
    , ('A' :| "ACTT" , ["AA-CTT", "AAC-TT", "AA-CTT", "AAC-TT"])
    , ('A' :| "ACTT" , ["AA-CTT", "AAC-TT", "AA-CTT", "AAC-TT"])
    , ('A' :| "ATT"  , ["AA--TT", "AA--TT", "AA--TT", "AA--TT"])
    , ('A' :| "ATT"  , ["AA--TT", "AA--TT", "AA--TT", "AA--TT"])
    ]


topologyT :: BTree () ()
topologyT = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal
            blank
            (Leaf (NodeDatum "C" ()))
            (Internal
                blank
                (Leaf (NodeDatum "D" ()))
                (Internal
                    blank
                    (Leaf (NodeDatum "E" ()))
                    (Internal
                        blank
                        (Leaf (NodeDatum "F" ()))
                        (Internal
                            blank
                            (Leaf (NodeDatum "G" ()))
                            (Internal
                                blank
                                (Leaf (NodeDatum "H" ()))
                                (Internal
                                    blank
                                    (Leaf (NodeDatum "I" ()))
                                    (Internal
                                        blank
                                        (Leaf (NodeDatum "J" ()))
                                        (Internal
                                            blank
                                            (Leaf (NodeDatum "K" ()))
                                            (Internal
                                                blank
                                                (Leaf (NodeDatum "L" ()))
                                                (Internal
                                                    blank
                                                    (Leaf (NodeDatum "M" ()))
                                                    (Leaf (NodeDatum "N" ()))
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


dataSetU :: StringValues
dataSetU
    = convertToValues
    [ ('A' :| "AGTT"  , ["AA-G-TT"])
    , ('A' :| "AGTT"  , ["AA-G-TT"])
    , ('A' :| "AGTT"  , ["AA-G-TT"])
    , ('A' :| "ACGTT" , ["AACG-TT"])
    , ('A' :| "ACGTT" , ["AACG-TT"])
    , ('A' :| "ACGTT" , ["AACG-TT"])
    , ('A' :| "ACGCTT", ["AACGCTT"])
    , ('A' :| "ACGCTT", ["AACGCTT"])
    , ('A' :| "ACGCTT", ["AACGCTT"])
    , ('A' :| "ACGTT" , ["AACG-TT"])
    , ('A' :| "ACGTT" , ["AACG-TT"])
    , ('A' :| "ACGTT" , ["AACG-TT"])
    , ('A' :| "AGTT"  , ["AA-G-TT"])
    , ('A' :| "AGTT"  , ["AA-G-TT"])
    ]


topologyU :: BTree () ()
topologyU = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal
            blank
            (Leaf (NodeDatum "C" ()))
            (Internal
                blank
                (Leaf (NodeDatum "D" ()))
                (Internal
                    blank
                    (Leaf (NodeDatum "E" ()))
                    (Internal
                        blank
                        (Leaf (NodeDatum "F" ()))
                        (Internal
                            blank
                            (Leaf (NodeDatum "G" ()))
                            (Internal
                                blank
                                (Leaf (NodeDatum "H" ()))
                                (Internal
                                    blank
                                    (Leaf (NodeDatum "I" ()))
                                    (Internal
                                        blank
                                        (Leaf (NodeDatum "J" ()))
                                        (Internal
                                            blank
                                            (Leaf (NodeDatum "K" ()))
                                            (Internal
                                                blank
                                                (Leaf (NodeDatum "L" ()))
                                                (Internal
                                                    blank
                                                    (Leaf (NodeDatum "M" ()))
                                                    (Leaf (NodeDatum "N" ()))
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


dataSetV :: StringValues
dataSetV
    = convertToValues
    [ ('A' :| "AGTT"  , ["AA-G-TT"])
    , ('A' :| "AGTT"  , ["AA-G-TT"])
    , ('A' :| "AGTT"  , ["AA-G-TT"])
    , ('A' :| "ACGTT" , ["AACG-TT"])
    , ('A' :| "ACGTT" , ["AACG-TT"])
    , ('A' :| "ACGTT" , ["AACG-TT"])
    , ('A' :| "ACGCTT", ["AACGCTT"])
    , ('A' :| "ACGCTT", ["AACGCTT"])
    , ('A' :| "ACGCTT", ["AACGCTT"])
    , ('A' :| "AGCTT" , ["AA-GCTT"])
    , ('A' :| "AGCTT" , ["AA-GCTT"])
    , ('A' :| "AGCTT" , ["AA-GCTT"])
    , ('A' :| "AGTT"  , ["AA-G-TT"])
    , ('A' :| "AGTT"  , ["AA-G-TT"])
    ]


topologyV :: BTree () ()
topologyV = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal
            blank
            (Leaf (NodeDatum "C" ()))
            (Internal
                blank
                (Leaf (NodeDatum "D" ()))
                (Internal
                    blank
                    (Leaf (NodeDatum "E" ()))
                    (Internal
                        blank
                        (Leaf (NodeDatum "F" ()))
                        (Internal
                            blank
                            (Leaf (NodeDatum "G" ()))
                            (Internal
                                blank
                                (Leaf (NodeDatum "H" ()))
                                (Internal
                                    blank
                                    (Leaf (NodeDatum "I" ()))
                                    (Internal
                                        blank
                                        (Leaf (NodeDatum "J" ()))
                                        (Internal
                                            blank
                                            (Leaf (NodeDatum "K" ()))
                                            (Internal
                                                blank
                                                (Leaf (NodeDatum "L" ()))
                                                (Internal
                                                    blank
                                                    (Leaf (NodeDatum "M" ()))
                                                    (Leaf (NodeDatum "N" ()))
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


dataSetW :: StringValues
dataSetW
    = convertToValues
    [ ('A' :| "T"   , ["A---T", "A---T"])
    , ('A' :| "T"   , ["A---T", "A---T"])
    , ('A' :| "CT"  , ["AC--T", "A--CT"])
    , ('A' :| "CT"  , ["AC--T", "A--CT"])
    , ('A' :| "CCT" , ["ACC-T", "A-CCT"])
    , ('A' :| "CCCT", ["ACCCT", "ACCCT"])
    ]


topologyW :: BTree () ()
topologyW = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal
            blank
            (Leaf (NodeDatum "C" ()))
            (Internal
                blank
                (Leaf (NodeDatum "D" ()))
                (Internal blank (Leaf (NodeDatum "E" ())) (Leaf (NodeDatum "F" ())))
            )
        )
    )


dataSetX :: StringValues
dataSetX
    = convertToValues
    [ ('G' :| "AA", ["GA-A"])
    , ('G' :| "AA", ["GA-A"])
    , ('A' :| "A" , ["-A-A"])
    , ('A' :| "A" , ["-A-A"])
    , ('A' :| "A" , ["-A-A"])
    , ('A' :| "CA", ["-ACA"])
    ]


topologyX :: BTree () ()
topologyX = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal
            blank
            (Leaf (NodeDatum "C" ()))
            (Internal
                blank
                (Leaf (NodeDatum "D" ()))
                (Internal blank (Leaf (NodeDatum "E" ())) (Leaf (NodeDatum "F" ())))
            )
        )
    )


dataSetY :: StringValues
dataSetY
    = convertToValues
    [ ('G' :| "AA" , ["GA-A"])
    , ('G' :| "AA" , ["GA-A"])
    , ('G' :| "ACA", ["GACA"])
    , ('G' :| "ACA", ["GACA"])
    , ('G' :| "ACA", ["GACA"])
    , ('A' :| "CA" , ["-ACA"])
    ]


topologyY :: BTree () ()
topologyY = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal
            blank
            (Leaf (NodeDatum "C" ()))
            (Internal
                blank
                (Leaf (NodeDatum "D" ()))
                (Internal blank (Leaf (NodeDatum "E" ())) (Leaf (NodeDatum "F" ())))
            )
        )
    )


dataSetZ :: StringValues
dataSetZ
    = convertToValues
    [ ('A' :| "AG", ["A-AG"])
    , ('A' :| "AG", ["A-AG"])
    , ('A' :| "A" , ["A-A-"])
    , ('A' :| "A" , ["A-A-"])
    , ('A' :| "A" , ["A-A-"])
    , ('A' :| "CA", ["ACA-"])
    ]


topologyZ :: BTree () ()
topologyZ = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal
            blank
            (Leaf (NodeDatum "C" ()))
            (Internal
                blank
                (Leaf (NodeDatum "D" ()))
                (Internal blank (Leaf (NodeDatum "E" ())) (Leaf (NodeDatum "F" ())))
            )
        )
    )


dataSet0 :: StringValues
dataSet0
    = convertToValues
    [ ('A' :| "AG" , ["A-AG"])
    , ('A' :| "AG" , ["A-AG"])
    , ('A' :| "CAG", ["ACAG"])
    , ('A' :| "CAG", ["ACAG"])
    , ('A' :| "CAG", ["ACAG"])
    , ('A' :| "CA" , ["ACA-"])
    ]


topology0 :: BTree () ()
topology0 = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal
            blank
            (Leaf (NodeDatum "C" ()))
            (Internal
                blank
                (Leaf (NodeDatum "D" ()))
                (Internal blank (Leaf (NodeDatum "E" ())) (Leaf (NodeDatum "F" ())))
            )
        )
    )


dataSet1 :: StringValues
dataSet1
    = convertToValues
    [ ('A' :| "A"   , ["A---A"])
    , ('A' :| "A"   , ["A---A"])
    , ('A' :| "CCA" , ["AC-CA"])
    , ('A' :| "CCA" , ["AC-CA"])
    , ('A' :| "CCA" , ["AC-CA"])
    , ('A' :| "CGCA", ["ACGCA"])
    ]


topology1 :: BTree () ()
topology1 = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal
            blank
            (Leaf (NodeDatum "C" ()))
            (Internal
                blank
                (Leaf (NodeDatum "D" ()))
                (Internal blank (Leaf (NodeDatum "E" ())) (Leaf (NodeDatum "F" ())))
            )
        )
    )


dataSet2 :: StringValues
dataSet2
    = convertToValues
    [ ('A' :| "AA"   , ["A--A--A"])
    , ('A' :| "AA"   , ["A--A--A"])
    , ('A' :| "CACA" , ["AC-A-CA"])
    , ('A' :| "CACA" , ["AC-A-CA"])
    , ('A' :| "CACA" , ["AC-A-CA"])
    , ('A' :| "CAGCA", ["AC-AGCA"])
    , ('A' :| "CACA" , ["AC-A-CA"])
    , ('A' :| "CACA" , ["AC-A-CA"])
    , ('A' :| "CGACA", ["ACGA-CA"])
    ]


topology2 :: BTree () ()
topology2 = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal
            blank
            (Leaf (NodeDatum "C" ()))
            (Internal
                blank
                (Internal
                    blank
                    (Leaf (NodeDatum "D" ()))
                    (Internal blank (Leaf (NodeDatum "E" ())) (Leaf (NodeDatum "F" ())))
                )
                (Internal
                    blank
                    (Leaf (NodeDatum "G" ()))
                    (Internal blank (Leaf (NodeDatum "H" ())) (Leaf (NodeDatum "I" ())))
                )
            )
        )
    )


dataSet3 :: StringValues
dataSet3
    = convertToValues
    [ ('A' :| "A"    , ["A----A"])
    , ('A' :| "A"    , ["A----A"])
    , ('A' :| "CA"   , ["A---CA"])
    , ('A' :| "CA"   , ["A---CA"])
    , ('A' :| "CA"   , ["A---CA"])
    , ('A' :| "GCTCA", ["AGCTCA"])
    ]


topology3 :: BTree () ()
topology3 = Internal
    blank
    (Leaf (NodeDatum "A" ()))
    (Internal
        blank
        (Leaf (NodeDatum "B" ()))
        (Internal
            blank
            (Leaf (NodeDatum "C" ()))
            (Internal
                blank
                (Leaf (NodeDatum "D" ()))
                (Internal blank (Leaf (NodeDatum "E" ())) (Leaf (NodeDatum "F" ())))
            )
        )
    )
