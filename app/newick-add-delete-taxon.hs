{- |
Module      :  newickDeleteTaxon
Description :  Program to delete an individual taxon from a dichotomous newick file
                modifies to take taxon delete list from file.
Copyright   :  (c) 2017 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
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

{-# Language Safe #-}

module Main
    ( main
    ) where

import Data.Foldable
import Data.Key
import Data.List (isPrefixOf)
import Data.List.Split
import Data.Maybe
import System.Environment
import System.IO


newickControlChars :: String
newickControlChars = ")(:; ,"


otherNewickChars :: String
otherNewickChars = "\r\n ;"


-- | removeBranchLengths takes Newick/ENeweick and strips out branch lengths
removeBranchLengths :: String -> String
removeBranchLengths inString = if null inString
    then error "Null input string"
    else
        let firstSplit  = splitOn "," inString
            secondSplit = splitParen firstSplit
            outString   = reassemble "," secondSplit
        in  outString


-- | reassemble takes [String] and adds arg between elements and returns String
reassemble :: String -> [String] -> String
reassemble joinString inList
    | null joinString          = error "Join string is empty"
    | null inList              = []
    | not (null $ tail inList) = fold [head inList, joinString, reassemble joinString $ tail inList]
    | otherwise                = head inList


-- | splitParen takes list of String and splits each one on ')' and reassmebles
splitParen :: [String] -> [String]
splitParen inList = if null inList
    then []
    else
        let firstSplit  = splitOn ")" (head inList)
            secondSplit = splitColon firstSplit
            outList     = reassemble ")" secondSplit
        in  outList : splitParen (tail inList)


-- | splitColon takes list of String and splits each one on ':'deletes branch
-- length after it
splitColon :: [String] -> [String]
splitColon inList = if null inList
    then []
    else
        let firstSplit = splitOn ":" (head inList)
            outList    = head firstSplit
        in  outList : splitColon (tail inList)


-- | find substring in string
findNameInString :: String -> String -> Int -> Maybe Int
findNameInString subString fullString pos
    | null subString = error "Null name to find"
    | null fullString = Nothing
    | subString `isPrefixOf` fullString = --ensures full name (e.g seq1 and seq10)
                                          if (fullString ! length subString) `elem` newickControlChars
        then Just pos
        else findNameInString subString (tail fullString) (pos + 1)
    | otherwise = findNameInString subString (tail fullString) (pos + 1)


-- | makeNewTree Takes input parts of tree and checks for ',' before deleitng and gluing together
makeNewTree :: String -> String -> String
makeNewTree firstPart secondPart
    | null firstPart                                      = error "Empty tree beginning"
    | null secondPart                                     = error "Empty tree ending"
    | (last firstPart == ',') && (head secondPart == ',') = deleteAndGlue (init firstPart) (tail secondPart)
    | (last firstPart /= ',') && (head secondPart == ',') = deleteAndGlue firstPart (tail secondPart)
    | (last firstPart == ',') && (head secondPart /= ',') = deleteAndGlue (init firstPart) secondPart
    | otherwise                                           = deleteAndGlue firstPart secondPart


-- | deleteAndGlue Takes appropriate firs and second parts deletes and glues together
deleteAndGlue :: String -> String -> String
deleteAndGlue firstPart secondPart
    | null firstPart                                      = error "Empty tree (2) beginning"
    | null secondPart                                     = error "Empty tree (2) ending"
    | (last firstPart == '(') && (head secondPart /= ')') = deleteRightParen (init firstPart) secondPart
    | (last firstPart /= '(') && (head secondPart == ')') = deleteLeftParen firstPart (tail secondPart)
    | otherwise                                           = error "Error in tree format--perhaps not dichotomous"


-- | getRightPos finds corresponding right paren ')'
getRightPos :: String -> Int -> Int -> Int
getRightPos curString pos counter
    | counter < 0                           = error "Error in getRightPos"
    | head curString == ')' && counter == 0 = pos
    | head curString == ')'                 = getRightPos (tail curString) (pos + 1) (counter - 1)
    | head curString == '('                 = getRightPos (tail curString) (pos + 1) (counter + 1)
    | otherwise                             = getRightPos (tail curString) (pos + 1) counter


-- | getLeftPos finds corresponding Left paren ')'
getLeftPos :: String -> Int -> Int -> Int
getLeftPos curString pos counter
    | counter < 0                           = error "Error in getLeftPos"
    | head curString == '(' && counter == 0 = pos
    | head curString == '('                 = getLeftPos (tail curString) (pos + 1) (counter - 1)
    | head curString == ')'                 = getLeftPos (tail curString) (pos + 1) (counter + 1)
    | otherwise                             = getLeftPos (tail curString) (pos + 1) counter


-- | deleteRightParen deletes right paren ')' corresponding to the zeroth left paren
deleteRightParen :: String -> String -> String
deleteRightParen firstPart secondPart =
  -- These two condition don't seem to be needed and fail when basalmost taxon is deleted
  -- if null firstPart then error "Error in deleteRightParen"
  -- else if null secondPart then error "Error (2) in deleteRightParen"
  -- else
    let otherParenPos = getRightPos secondPart 0 0
        newFirst      = take otherParenPos secondPart
        newSecond     = drop (otherParenPos + 1) secondPart
    in  firstPart <> newFirst <> newSecond


-- | deleteLeftParen deletes left paren ')' corresponding to the zeroth left paren
deleteLeftParen :: String -> String -> String
deleteLeftParen firstPart secondPart =
  -- These two condition don't seem to be needed and fail when basalmost taxon is deleted
  -- if null firstPart then error "Error in deleteLeftParen"
  -- else if null secondPart then error "Error (2) in deleteLeftParen"
  -- else
    let revFirstPart  = reverse firstPart
        otherParenPos = getLeftPos revFirstPart 0 0
        newSecond     = take otherParenPos revFirstPart
        newFirst      = drop (otherParenPos + 1) revFirstPart
    in  fold [reverse newFirst, reverse newSecond, secondPart]


-- | removeTaxonFromNewick recursively deletes taxa from list till all removed from tree,
--then returns newick
removeTaxonFromNewick :: [String] -> String -> String
removeTaxonFromNewick taxaToGo origNewick = if null taxaToGo
    then origNewick
    else
        let taxonToGo   = head taxaToGo
            taxLocation = findNameInString taxonToGo origNewick 0
        in  if isNothing taxLocation
            then error ("Error: Taxon " <> taxonToGo <> " not in tree")
            else
                let firstPart  = take (fromJust taxLocation) origNewick
                    secondPart = drop (fromJust taxLocation + length taxonToGo) origNewick
                in  removeTaxonFromNewick (tail taxaToGo) (makeNewTree firstPart secondPart)


-- | addTaxon2Newick adds a single taxon name as sister to one in the tree,
--then returns newick
addTaxon2Newick :: String -> String -> String -> String
addTaxon2Newick taxon2Add taxonPlace origNewick =
    let taxLocation     = findNameInString taxonPlace origNewick 0
        tax2AddLocation = findNameInString taxon2Add origNewick 0
    in  if isNothing taxLocation
        then error $ unwords ["Error: Taxon", taxonPlace, "not in tree"]
        else if Data.Maybe.isJust tax2AddLocation
            then unwords ["Error: Taxon", taxon2Add, "already in tree"]
            else
                let firstPart  = take (fromJust taxLocation) origNewick
                    secondPart = drop (fromJust taxLocation + length taxonPlace) origNewick
                in  fold [firstPart, "(", taxonPlace, ",", taxon2Add, ")", secondPart]


-- | appendString appends first string to end of second (for mapping purposes)
appendString :: String -> String -> String
appendString stringToAppend stringToBeAppended
    | null stringToAppend     = error "Nothing to append"
    | null stringToBeAppended = error "Nothing to append to"
    | otherwise               = stringToBeAppended <> stringToAppend


-- | printTrees takes trees, process, then adds ";\n" to each


-- | main driver
main :: IO ()
main = do
    args <- getArgs
    if length args /= 3
        then error $ fold
            [ "Three arguments--add or delete, newick tree file (binary),"
            , " and file of add/delete taxon names\n If taxa are to be added the file should only contain two Strings, the taxon name to add,"
            , " and the place where (as sister) it should be added."
            ]
        else hPutStr stderr "Arguments: "
    mapM_ (hPutStrLn stderr) args
    hPutStrLn stderr ""
    let operation = head args
    let inNewick  = args ! 1
    if operation == "add" || operation == "delete"
        then hPutStrLn stderr $ unwords ["Newick will", operation, "taxon/a"]
        else error $ unwords ["Operation", operation, "is unrecognized, must be 'add' or 'delete'"]
    hPutStrLn stderr $ unwords
        ["Openning newick  treefile", inNewick, "and taxon/a to be added/deleted from file", args ! 2]
    hPutStrLn stderr "Warning--Tree must be dichotomous for deletion"
    treeFileHandle   <- openFile inNewick ReadMode
    deleteTaxaHandle <- openFile (args ! 2) ReadMode
    rawTreeStuff     <- hGetContents treeFileHandle --init so remove last empty String
    let rawTreeFile = init $ splitOn ";" rawTreeStuff
    hPutStrLn stderr $ unwords ["Input of", show $ length rawTreeFile, "trees"]
    let treeFile      = fmap (filter (`notElem` otherNewickChars)) rawTreeFile
    --removes branch lengths and terminal comment (ie POY tree cost)
    let cleanTreeFile = fmap (takeWhile (/= '[') . removeBranchLengths) treeFile
     --let cleanTreeFile = (takeWhile (/= '[' ) $ removeBranchLengths treeFile) ++ [';']
    deleteTaxaRaw <- hGetContents deleteTaxaHandle
    let deleteTaxa = words deleteTaxaRaw
    let op = if operation == "delete"
            then removeTaxonFromNewick deleteTaxa
            else addTaxon2Newick (head deleteTaxa) (deleteTaxa ! 1)
    mapM_ putStrLn $ appendString ";\n" . op <$> cleanTreeFile

        --let newTree = removeTaxonFromNewick cleanTreeFile (tail $ tail args)
       --let taxToGo = last args
       --let taxLocation = findNameInString taxToGo cleanTreeFile 0
       --if taxLocation == Nothing then error ("Error: Taxon " ++ taxToGo ++ " not in tree")
       --else hPutStrLn stderr ("Found " ++ taxToGo ++ " at position " ++ show (fromJust taxLocation))
       --let firstPart = take (fromJust taxLocation) cleanTreeFile
       --let secondPart = drop ((fromJust taxLocation) + (length taxToGo)) cleanTreeFile
       --hPutStrLn stderr (firstPart ++ " " ++ secondPart)
       --let newTree = makeNewTree firstPart secondPart
       --hPutStrLn stdout newTree
