{- |
Module      :  reduceFasta
Description :  inputs an aligned fasta file and outputs reduced file intaxon number and/or sequence length
Copyright   :  (c) 2018 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
License     :


Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:


1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.


THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


The views and conclusions contained in the software and documentation are those
of the authors and should not be interpreted as representing official policies,
either expressed or implied, of the FreeBSD Project.


Maintainer  :  Ward Wheeler <wheeler@amnh.org>
Stability   :  unstable
Portability :  portable (I hope)


-}


{- Need to add
    1) generate tcm files with alphabets and cost matrices
    2) optinos for alternate cost regimes
-}

{-# Language Safe #-}

module Main
    ( main
    ) where

import Data.Key
import Data.List (intersperse)
import System.Environment
import System.IO


-- D/RNA + Amino Acid no gaps
symbols :: String
symbols = "ACGTUBHRNDQEHILKMFPSTWYVX?"


-- |
-- getSequence extracts sequence data till empty line or '>'
getSequence :: String -> [String] -> (String, [String])
getSequence sequenceSoFar inLines = case inLines of
    [] -> (sequenceSoFar, inLines)
    x : xs ->
        let firstLine = x
        in  case firstLine of
                []    -> getSequence sequenceSoFar xs
                y : _ -> if y == '>' then (sequenceSoFar, inLines) else getSequence (sequenceSoFar <> x) xs


-- |
-- getNameSequencePairs takes input file and returns list of name sequence pairs
getNameSequencePairs :: [String] -> [(String, String)]
getNameSequencePairs inLines = if null inLines
    then []
    else
        let firstLine = head inLines
        in  if head firstLine == ';'
                then getNameSequencePairs (tail inLines) -- comment line
                else if head firstLine == '>'
                    then --new taxon
                        let taxonName                  = firstLine
                            (taxonSequence, remainder) = getSequence [] (tail inLines)
                        in  (taxonName, taxonSequence) : getNameSequencePairs remainder
                    else error ("Should be new sequence: " <> firstLine)


-- |
-- checkSeqLength takes [(name, sequence)] and verifies that all are same length
checkSeqLength :: Int -> [(String, String)] -> Bool
checkSeqLength prevLength pairList =
    null pairList
        || let
            (_, firstSequence) = head pairList
            firstLength        = length firstSequence
           in  ((prevLength == (-1)) || (firstLength == prevLength))
            && checkSeqLength firstLength (tail pairList)


-- |
-- cutLength cuts down length of sequence in pair
cutLength :: Int -> (String, String) -> (String, String)
cutLength newLength (taxName, taxSeq) = if null taxSeq
    then error "Sequence zero length"
    else (taxName, filter (`elem` symbols) (take newLength . drop startPoint $ taxSeq))
    where
        midPoint   = length taxSeq `div` 2
        startPoint = midPoint - (newLength `div` 2)


-- |
-- getNewPairs cuts number of pairs down to number taxa and cuts length to new length
getNewPairs :: Int -> Int -> [(String, String)] -> [(String, String)]
getNewPairs numTax lengthSeq inPairs = if null inPairs
    then error "No input pairs to cut"
    else let newPairList = take numTax inPairs in cutLength lengthSeq <$> newPairList


-- |
-- printPair takes a Taxon sequence pairs and prints to channel
printPair :: Handle -> (String, String) -> IO ()
printPair channel (taxName, taxSequence) = do
    hPutStrLn channel taxName
    hPutStrLn channel taxSequence


-- |
-- 'main' Main Function
main :: IO ()
main = do
        --get input command filename
    args <- getArgs
    if length args /= 4
        then error
            (  "Four arguments--one input aligned fasta file, number taxa wanted "
            <> "integer, if larger than tax number all included), fraction of bases wanted (0.0,1.0], and a stub for output of deleted taxa"
            )
        else hPutStr stderr "Arguments: "
    mapM_ (hPutStrLn stderr) args
    hPutStrLn stderr ""
    let numTaxa  = read (args ! 1) :: Int
    let fraction = read (args ! 2) :: Double
    deletedTaxaHandle <- openFile (last args <> ".deleted") WriteMode
    inFileHandle      <- openFile (head args) ReadMode
    inContents        <- hGetContents inFileHandle
    -- get taxon data
    let sequencePairList = getNameSequencePairs (lines inContents)
    -- check lengths
    let sameLength       = checkSeqLength (-1) sequencePairList
    let originalBases    = length . snd $ head sequencePairList
    if not sameLength
        then error "Sequences not same length"
        else hPutStrLn stderr $ unwords
            ["There are", show $ length sequencePairList, "sequences of length", show originalBases]
    let numBases = ceiling . (fraction *) . fromIntegral $ originalBases
    if numBases == 0
        then error "Increase fraction--this results in zero bases to output"
        else if numTaxa == 0
            then error "Increase num taxa--this results in zero taxa to output"
            else hPutStrLn stderr $ unwords ["Outputting", show numTaxa, "with length", show numBases]
    let newPairs     = getNewPairs numTaxa numBases sequencePairList
    -- let deletedTaxa = intersperse " " $ fmap fst (drop numTaxa sequencePairList)
    let deletedPairs = drop numTaxa sequencePairList
    let deletedTaxa  = intersperse " " $ fmap (tail . fst) deletedPairs
    hPutStrLn stderr $ show (length deletedTaxa) <> " deleted taxa"
    mapM_ (hPutStr deletedTaxaHandle) deletedTaxa
    hClose deletedTaxaHandle
    mapM_ (printPair stdout) newPairs
    hPutStrLn stderr "Done"

