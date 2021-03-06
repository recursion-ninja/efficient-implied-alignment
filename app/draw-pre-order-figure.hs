{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           Data.Data
import           Data.Key
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude             hiding (trace)
import           Prelude                      hiding (zip)


main :: IO ()
main = mainWith
     . scale 7
     $ canvas # connectOutside' arrowBent "frame 0" "frame 1"
              # connectOutside' arrowLine "frame 1" "frame 2"
              # connectOutside' arrowBent "frame 2" "frame 3"
              # connectOutside' arrowLine "frame 3" "frame 4"
              # connectOutside' arrowBent "frame 4" "frame 5"
              # connectOutside' arrowLine "frame 5" "frame 6"
              # connectOutside' arrowBent "frame 6" "frame 7"
              # connectOutside' arrowLine "frame 7" "frame 8"
              # connectOutside' arrowBent "frame 8" "frame 9"
              # connectOutside' arrowLine "frame 9" "frame 10"


canvas :: Diagram B
canvas = position (zip fPoints frames <> zip lPoints labels)


arrowLine :: (Typeable n, RealFloat n) => ArrowOpts n
arrowLine = with & shaftStyle %~ lw 3
                 & headLength .~ 12


arrowBent :: (Typeable n, RealFloat n) => ArrowOpts n
arrowBent =
    let shaft = trailFromVertices $ p2 <$> [ (0, 0), (0, 1.5), (9, 1.5), (9, 3) ]
    in  with & arrowShaft .~ shaft
             & headLength .~ 12
             & shaftStyle %~ lw 3


data  AlignCell
    = Align
    | Delete
    | Insert
    | Gapped
    | Spacing
    | Question
    deriving (Eq, Show)


toSymbol :: AlignCell -> Char
toSymbol Align    = '𝗕'-- '𝗔' -- '𝓐'
toSymbol Delete   = '𝗟' -- '𝗗' -- '𝓓'
toSymbol Insert   = '𝗥' -- '𝗜' -- '𝓘'
toSymbol Gapped   = '𝗚'
toSymbol Spacing  = ' '
toSymbol Question = '？'


frames :: [Diagram B]
frames = f <#$> ijks
  where
    f k = (# named ("frame " <> show k)) . pad 1.4 . (<>box) . centerXY . makeAlignments
    box = phantom (rect 28 11 :: Diagram B) :: Diagram B


makeAlignments :: (Word, Word, Word) -> Diagram B
makeAlignments (i,j,k) = stackVertical
    [ makeIndexLabel "i" i ||| alignmentAt i pAlign
    , makeIndexLabel "j" j ||| alignmentAt i pContext
    , makeIndexLabel "k" k ||| alignmentAt i cContext
    , makeIndexPad         ||| derivedAt   i cAlign
    ]
  where
    f   = withEnvelope box
    box = phantom (rect 1 2 :: Diagram B) :: Diagram B
    makeIndexPad = box ||| box ||| box ||| box
    makeIndexLabel :: String -> Word -> Diagram B
    makeIndexLabel idx val = f smb ||| f eqs ||| f num ||| box
      where
        -- We make a different cell for each symbol to ensure "monospacing."
        smb = txt idx
        eqs = txt "="
        num = txt $ show val
        txt = scale 1.5 . bold . text


stackVertical
  :: ( Juxtaposable c
     , Num (N c)
     , Monoid c
     , V c ~ V2
     )
  => [c]
  -> c
stackVertical = foldr underneath mempty . reverse


underneath
  :: ( Juxtaposable a
     , Semigroup a
     , Num (N a)
     , V a ~ V2
     )
  => a
  -> a
  -> a
underneath = beside (r2 (0, 180))


alignmentAt :: Word -> [AlignCell] -> Diagram B
alignmentAt i xs = foldlWithKey makeCell mempty cells
  where
    (h,t) = splitAt (fromEnum i) xs
    cells
      | all (==Spacing) t = h <> [Spacing]
      | otherwise         = h <> filter (/=Spacing) t

    cursorStop :: Word
    cursorStop = toEnum . length . dropWhile (==Spacing) $ reverse cells

    makeCell :: Diagram B -> Int -> AlignCell -> Diagram B
    makeCell a k e
      | atCursorStop = a ||| (stp <> sqr)
      | e == Spacing = a |||         sqr
      | otherwise    = a ||| (txt <> sqr)
      where
        atCursorStop = e == Spacing && k' == cursorStop
        k'  = toEnum k
        txt = cellText [toSymbol e]
        sqr = clr cellBox
        clr | atCursorStop = lineColor (sRGB 196 0 0)
            | e == Spacing = lineColor (sRGB   0 0 0)
            | otherwise    = case k' `compare` i of
                               EQ -> redLine
                               LT -> grnLine
                               GT -> blkLine


cellText :: String -> Diagram B
cellText = alignT . scale (5/3) . (<> phantom box) . bold . text
  where
    box = square 0.25 :: Diagram B


cellBox :: Diagram B
cellBox = lineWidth 2 . pad 1.2 $ square 2


derivedAt :: Word -> [AlignCell] -> Diagram B
derivedAt i xs = foldl makeCell mempty cells
  where
    cells = take (fromEnum i) xs <> [Question]

    makeCell :: Diagram B -> AlignCell -> Diagram B
    makeCell a e
      | i == 10 && e == Question = a ||| (stp <> sqr)
      | otherwise                = a ||| (txt <> sqr)
      where
        txt = cellText [toSymbol e]
        sqr = clr cellBox
        clr | e == Question = redLine
            | e == Spacing  = redLine
            | otherwise     = bluLine


blkLine, bluLine, redLine, grnLine :: (Typeable (N a), Floating (N a), HasStyle a, V a ~ V2) => a -> a
blkLine = lineColor (sRGB   0   0   0)
bluLine = lineColor (sRGB   0   0 128)
redLine = lineColor (sRGB 196   0   0)
grnLine = lineColor (sRGB   0 128   0)


stp :: Diagram B
stp = upper <> lower
  where
    upper  = mkLine [origin, sqrt 2 ^& sqrt 2]
    lower  = mkLine [0 ^& sqrt 2, sqrt 2 ^& 0]
    mkLine = centerXY . lineWidth 2 . strokeLine . lineFromVertices


fPoints :: [P2 Double]
fPoints = p2 <$>
    [  (40, 90)
    ,  ( 0, 72)
    ,  (40, 72)
    ,  ( 0, 54)
    ,  (40, 54)
    ,  ( 0, 36)
    ,  (40, 36)
    ,  ( 0, 18)
    ,  (40, 18)
    ,  ( 0,  0)
    ,  (41.25,  0)
    ]


lPoints :: [P2 Double]
lPoints = p2 <$>
    [ (20,  84 )
    , (20,  74 )
    , (20,  66 )
    , (20,  56 )
    , (20,  48 )
    , (20,  38 )
    , (20,  30 )
    , (20,  20 )
    , (20,  12 )
    , (20,  2  )
    , ( 1.5  ,  3.5 + 90)--, (45    ,  3.5), (53.125,  3.5)
    , ( 3.875,  1   + 90)--, (45.375,  1  ), (53.125,  1  )
    , ( 3    , -1.5 + 90)--, (45.375, -1.5), (53.125, -1.5)
    , ( 0.625, -4   + 90)--, (45    , -4  ), (53.125, -4  )
    ]


labels :: [Diagram B]
labels =
    let lab = centerXY . scale 1.8 . pad 1.5 . bold . text
    in  [ lab "Case 2"
        , lab "Case 3"
        , lab "Case 0"
        , lab "Case 2"
        , lab "Case 5"
        , lab "Case 3"
        , lab "Case 2"
        , lab "Case 1"
        , lab "Case 0"
        , lab "Case 0"
        , lab "Parent's final       alignment"--, lab "(preorder" , lab "result)"
        , lab "Parent's preliminary context"  --, lab "(postorder", lab "result)"
        , lab "Child's  preliminary context"  --, lab "(postorder", lab "result)"
        , lab "Child's  final       alignment"--, lab "(preorder" , lab "result)"
        ]


{-
lPoints :: [P2 Double]
lPoints = map p2  . take (length frames) . fmap swap . reverse . sort $ do
    x <- [90, 72 .. 0]
    y <- [ 0, 40]
    pure (x, y)
-}


ijks :: [(Word, Word, Word)]
ijks =
    [ ( 0, 0, 0)
    , ( 1, 1, 1)
    , ( 2, 2, 2)
    , ( 3, 2, 2)
    , ( 4, 3, 3)
    , ( 5, 4, 3)
    , ( 6, 5, 4)
    , ( 7, 6, 5)
    , ( 8, 6, 5)
    , ( 9, 6, 5)
    , (10, 6, 5)
    ]


pAlign :: [AlignCell]
pAlign =
    [ Align
    , Delete
    , Gapped
    , Align
    , Insert
    , Delete
    , Align
    , Insert
    , Gapped
    , Gapped
    ]


pContext :: [AlignCell]
pContext =
    [ Align
    , Delete
    , Spacing
    , Align
    , Insert
    , Delete
    , Align
    , Insert
    , Spacing
    , Spacing
    ]


cContext :: [AlignCell]
cContext =
    [ Align
    , Align
    , Spacing
    , Insert
    , Spacing
    , Align
    , Delete
    , Spacing
    , Spacing
    , Spacing
    ]


cAlign :: [AlignCell]
cAlign =
    [ Align
    , Align
    , Gapped
    , Insert
    , Gapped
    , Align
    , Delete
    , Gapped
    , Gapped
    , Gapped
    ]
