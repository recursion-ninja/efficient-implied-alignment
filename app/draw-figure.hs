{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Data.Data
import Data.Key
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude hiding (trace)
import Prelude hiding (zip)


main :: IO ()
main = mainWith $ canvas # connectOutside' arrowLine "frame 0" "frame 1"
                         # connectOutside' arrowBent "frame 1" "frame 2"
                         # connectOutside' arrowLine "frame 2" "frame 3"
                         # connectOutside' arrowBent "frame 3" "frame 4"
                         # connectOutside' arrowLine "frame 4" "frame 5"
                         # connectOutside' arrowBent "frame 5" "frame 6"
                         # connectOutside' arrowLine "frame 6" "frame 7"
                         # connectOutside' arrowBent "frame 7" "frame 8"
                         # connectOutside' arrowLine "frame 8" "frame 9"
                         # connectOutside' arrowBent "frame 9" "frame 10"
--                         # connectOutside' arrowSquiggle "frame 1" "frame 2"
--main = print points *> mainWith (position (zip points frames))
{-
main = do
    print points
    mainWith $ (frames !! 0 ||| frames !! 1) # connectOutside "frame 0" "frame 1"
-}
--main = mainWith $ (frames !! 0 ||| frames !! 1) # connectOutside "frame 0" "frame 1"
--main = mainWith $ stackVertical frames


canvas :: Diagram B
canvas = position (zip fPoints frames <> zip lPoints labels)


arrowLine :: (Typeable n, RealFloat n) => ArrowOpts n
arrowLine = with & shaftStyle %~ lw 0.5
                 & headLength .~ 1


arrowBent :: (Typeable n, RealFloat n) => ArrowOpts n
arrowBent =
    let shaft = trailFromVertices $ map p2 [(0, 0), (0, 0.5), (3, 0.5), (3, 1) ]
    in  with & arrowShaft .~ shaft
             & headLength .~ 0.5
             & shaftStyle %~ lw 0.5


data  AlignCell
    = Align
    | Delete
    | Insert
    | Spacing
    | Question
    deriving (Eq, Show)


toSymbol :: AlignCell -> Char
toSymbol Align    = '𝗔' -- '𝓐'
toSymbol Delete   = '𝗗' -- '𝓓'
toSymbol Insert   = '𝗜' -- '𝓘'
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
    box = phantom (rect 0.75 2 :: Diagram B) :: Diagram B
    makeIndexPad = box ||| box ||| box ||| box
    makeIndexLabel :: String -> Word -> Diagram B
    makeIndexLabel idx val = f smb ||| f eqs ||| f num ||| box
      where
        -- We make a different cell for each symbol to ensure "monospacing."
        smb = txt idx
        eqs = txt "="
        num = txt $ show val
        txt = scale 1.25 . bold . text


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
        txt = cellText $ [toSymbol e]
        sqr = clr . lineWidth 0.25 . pad 1.2 $ square 2
        clr | atCursorStop = lineColor (sRGB 196 0 0)
            | e == Spacing = lineColor (sRGB   0 0 0)
            | otherwise    = case k' `compare` i of
                               EQ -> redLine
                               LT -> grnLine
                               GT -> blkLine


cellText :: String -> Diagram B
cellText = alignT . scale (5/3) . (<> phantom box) . bold . text
  where
    box = square 0.65 :: Diagram B



derivedAt :: Word -> [AlignCell] -> Diagram B
derivedAt i xs = foldl makeCell mempty cells 
  where
    cells = take (fromEnum i) xs <> [Question]
    
    makeCell :: Diagram B -> AlignCell -> Diagram B
    makeCell a e
      | i == 10 && e == Question = a ||| (stp <> sqr)
      | otherwise                = a ||| (txt <> sqr)
      where
        txt = cellText $ [toSymbol e]
        sqr = clr . lineWidth 0.25 . pad 1.2 $ square 2
        clr | e == Question = redLine
            | e == Spacing  = redLine
            | otherwise     = bluLine


blkLine, bluLine, redLine, grnLine :: (Typeable (N a), Floating (N a), HasStyle a, V a ~ V2) => a -> a
blkLine = lineColor (sRGB   0   0  0)
bluLine = lineColor (sRGB   0   0 128)
redLine = lineColor (sRGB 196   0   0)
grnLine = lineColor (sRGB   0 128   0)


stp :: Diagram B
stp = upper <> lower
  where
    upper  = mkLine [origin, (sqrt 2) ^& sqrt 2]
    lower  = mkLine [0 ^& (sqrt 2), sqrt 2 ^& 0]
    mkLine = centerXY . lineWidth 0.25 . strokeLine . lineFromVertices


fPoints :: [P2 Double]
fPoints = map p2 
    [  ( 0, 90)
    ,  (40, 90)
    ,  ( 0, 72)
    ,  (40, 72)
    ,  ( 0, 54)
    ,  (40, 54)
    ,  ( 0, 36)
    ,  (40, 36)
    ,  ( 0, 18)
    ,  (40, 18)
    ,  ( 0,  0)
    ]


lPoints :: [P2 Double]
lPoints = map p2
    [ (20, 92)
    , (20, 84)
    , (20, 74)
    , (20, 66)
    , (20, 56)
    , (20, 48)
    , (20, 38)
    , (20, 30)
    , (20, 20)
    , (20, 12)
    ]


labels :: [Diagram B]
labels =
    let lab = centerXY . scale 2 . pad 1.5 . bold . text
    in  [ lab "Case 9"
        , lab "Case 5"
        , lab "Case 8"
        , lab "Case 3"
        , lab "Case 4"
        , lab "Case 7"
        , lab "Case 6"
        , lab "Case 1"
        , lab "Case 2"
        , lab "Case 0"
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
    , ( 3, 3, 3)
    , ( 4, 3, 3)
    , ( 5, 4, 3)
    , ( 6, 5, 4)
    , ( 7, 6, 4)
    , ( 8, 6, 4)
    , ( 9, 7, 4)
    , (10, 7, 4)
    ]


pAlign :: [AlignCell]
pAlign =
    [ Align
    , Insert
    , Align
    , Delete
    , Insert
    , Align
    , Insert
    , Delete
    , Align
    , Align
    , Spacing
    ]


pContext :: [AlignCell]
pContext =
    [ Align
    , Insert
    , Insert
    , Spacing
    , Delete
    , Delete
    , Align
    , Spacing 
    , Insert
    , Spacing
    , Spacing
    , Spacing    
    ]


cContext :: [AlignCell]
cContext =
    [ Align
    , Align
    , Align
    , Spacing
    , Spacing
    , Spacing
    , Align
    , Spacing
    , Spacing
    , Spacing
    , Spacing
    ]


cAlign :: [AlignCell]
cAlign =
    [ Align
    , Align
    , Align
    , Delete
    , Delete
    , Delete
    , Align
    , Delete
    , Delete
    , Delete
    , Spacing
    ]
