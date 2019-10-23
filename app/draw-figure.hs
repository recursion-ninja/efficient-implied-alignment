{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Data.Foldable
import Data.Key
import Diagrams.Prelude
import Diagrams.TwoD.Text
import Diagrams.Backend.SVG.CmdLine
-- or:
-- import Diagrams.Backend.xxx.CmdLine
-- where xxx is the backend you would like to use.
 
myCircle :: Diagram B
myCircle = circle 1

--main = mainWith $ foldl' (|||) mempty $ sSquare <$> [0..5]
--main = mainWith $ alignmentAt 6 pAlign
main = mainWith . stackVertical $ pad 1.4 . makeAlignments <$> ijks 

makeAlignments :: (Word, Word, Word) -> Diagram B
makeAlignments (i,j,k) = stackVertical
    [ alignmentAt i pAlign
    , alignmentAt j pContext
    , alignmentAt k cContext
    ]

stackVertical = foldr underneath mempty . reverse

underneath = beside (r2 (0, 180))


sSquare :: Word -> Diagram B
sSquare i = txt  <> square 2
  where
    txt = centerXY . text $ show i


alignmentAt :: Word -> [(Char, Char, Char)] -> Diagram B
alignmentAt i = foldlWithKey makeCell mempty
  where
    makeCell :: Diagram B -> Int -> (Char, Char, Char) -> Diagram B
    makeCell a k (c,_,_) = a ||| (txt <> sqr)
      where
        txt = centerXY . bold . text $ [c]
	sqr = clr $ square 2
        clr = case toEnum k `compare` i of
	        EQ -> lineColor (sRGB 255   0 0)
		LT -> lineColor (sRGB   0 255 0)
		GT -> lineColor (sRGB   0   0 0)


ijks :: [(Word, Word, Word)]
ijks =
    [ (0, 0, 0)
    , (1, 1, 1)
    , (2, 2, 2)
    , (3, 2, 2)
    , (4, 3, 2)
    , (5, 4, 2)
    , (6, 5, 3)
    , (7, 6, 3)
    , (8, 6, 3)
    , (9, 6, 3)
    ]



pAlign =
      [ ('𝓐', '?','?')
      , ('𝓘', '?','?')
      , ('𝓐', '?','?')
      , ('𝓓', '?','?')
      , ('𝓘', '?','?')
      , ('𝓐', '?','?')
      , ('𝓘', '?','?')
      , ('𝓐', '?','?')
      , ('𝓓', '?','?')
      , ('𝓐', '?','?')
      ]


pContext =
      [ ('𝓐', '?','?')
      , ('𝓘', '?','?')
      , ('𝓘', '?','?')
      , ('𝓓', '?','?')
      , ('𝓓', '?','?')
      , ('𝓐', '?','?')
      , ('𝓐', '?','?')
      ]


cContext =
      [ ('𝓐', 'A','A')
      , ('𝓐', 'C','C')
      , ('𝓐', 'C','C')
      , ('𝓐', 'A','A')
      ]

