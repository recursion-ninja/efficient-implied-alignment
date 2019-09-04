{- |
Module      :  newickDeleteTaxon
Description :  Progam to delete an individual taxon from a dichotomous newick file
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

module Main where

import System.IO
import System.Environment
import Data.List
import Data.List.Split
import Data.Maybe

newickControlChars :: String
newickControlChars = ")(:; ,"

otherNewickChars :: String
otherNewickChars = "\r\n ;"

-- | removeBranchLengths takes Newick/ENeweick and strips out branch lengths
removeBranchLengths :: String -> String
removeBranchLengths inString =
    if null inString then error "Null input string"
    else 
        let firstSplit = splitOn "," inString
            secondSplit =  splitParen firstSplit
            outString = reassemble "," secondSplit
        in
        outString

-- | reassemble takes [String] and adds arg between elements and returns String
reassemble :: String -> [String] -> String
reassemble joinString inList =
    if null joinString  then error "Join string is empty"
    else if null inList then []
    else
         if not (null $ tail inList) then (head inList) ++ (joinString ++ (reassemble joinString $ tail inList))
         else head inList

-- | splitParen takes list of String and splits each one on ')' and reassmebles
splitParen :: [String] -> [String]
splitParen inList =
    if null inList then []
    else 
        let firstSplit = splitOn ")" (head inList)
            secondSplit  = splitColon firstSplit
            outList = reassemble ")" secondSplit
        in
            outList : (splitParen $ tail inList)

-- | splitColon takes list of String and splits each one on ':'deletes branch
-- length after it 
splitColon ::[String] -> [String]
splitColon inList =
    if null inList then []
    else 
        let firstSplit = splitOn ":" (head inList)
            outList = head firstSplit
        in
            outList : (splitColon $ tail inList)

-- | find substring in string
findNameInString :: String -> String -> Int -> Maybe Int
findNameInString subString fullString pos =
  if null subString then error "Null name to find"
  else if null fullString then Nothing
  else 
      if isPrefixOf subString fullString then --ensures full name (e.g seq1 and seq10)
        if (fullString !! (length subString))  `elem` newickControlChars then (Just pos)
        else findNameInString subString (tail fullString) (pos + 1)
      else findNameInString subString (tail fullString) (pos + 1)

-- | makeNewTree Takes input parts of tree and checks for ',' before deleitng and gluing together
makeNewTree :: String -> String -> String
makeNewTree firstPart secondPart =
    if null firstPart then error "Empty tree beginning"
    else if null secondPart then error "Empty tree ending"
    else 
        if ((last firstPart) == ',') && ((head secondPart) == ',') then deleteAndGlue (init firstPart) (tail secondPart)
        else if ((last firstPart) /= ',') && ((head secondPart) == ',') then deleteAndGlue firstPart (tail secondPart)
        else if ((last firstPart) == ',') && ((head secondPart) /= ',') then deleteAndGlue (init firstPart) secondPart
        else deleteAndGlue firstPart secondPart

-- | deleteAndGlue Takes appropriate firs and second parts deletes and glues together
deleteAndGlue :: String -> String -> String
deleteAndGlue firstPart secondPart =
  if null firstPart then error "Empty tree (2) beginning"
    else if null secondPart then error "Empty tree (2) ending"
    else 
        if ((last firstPart) == '(') && ((head secondPart) /= ')') then deleteRightParen (init firstPart) secondPart
        else if ((last firstPart) /= '(') && ((head secondPart) == ')') then deleteLeftParen firstPart (tail secondPart)
        else error "Error in tree format--perhaps not dichotomous"

-- | getRightPos finds corresponding right paren ')'
getRightPos :: String -> Int -> Int -> Int
getRightPos curString pos counter =
  if (counter < 0) then error "Error in getRightPos"
  else if head curString == ')' && counter == 0 then pos
  else if head curString == ')' then getRightPos (tail curString) (pos + 1) (counter - 1)
  else if head curString == '(' then getRightPos (tail curString) (pos + 1) (counter + 1)
  else getRightPos (tail curString) (pos + 1) counter

-- | getLeftPos finds corresponding Left paren ')'
getLeftPos :: String -> Int -> Int -> Int
getLeftPos curString pos counter =
  if (counter < 0) then error "Error in getLeftPos"
  else if head curString == '(' && counter == 0 then pos
  else if head curString == '(' then getLeftPos (tail curString) (pos + 1) (counter - 1)
  else if head curString == ')' then getLeftPos (tail curString) (pos + 1) (counter + 1)
  else getLeftPos (tail curString) (pos + 1) counter


-- | deleteRightParen deletes right paren ')' corresponding to the zeroth left paren
deleteRightParen :: String -> String -> String
deleteRightParen firstPart secondPart =
  -- These two condition don't seem to be needed and fail when basalmost taxon is deleted
  -- if null firstPart then error "Error in deleteRightParen"
  -- else if null secondPart then error "Error (2) in deleteRightParen"
  -- else 
      let otherParenPos = getRightPos secondPart 0 0
          newFirst = take otherParenPos secondPart
          newSecond = drop (otherParenPos + 1) secondPart
      in
        firstPart ++ newFirst ++ newSecond

-- | deleteLeftParen deletes left paren ')' corresponding to the zeroth left paren
deleteLeftParen :: String -> String -> String
deleteLeftParen firstPart secondPart =
  -- These two condition don't seem to be needed and fail when basalmost taxon is deleted
  -- if null firstPart then error "Error in deleteLeftParen"
  -- else if null secondPart then error "Error (2) in deleteLeftParen"
  -- else 
      let revFirstPart = reverse firstPart
          otherParenPos = getLeftPos revFirstPart 0 0
          newSecond = take otherParenPos revFirstPart
          newFirst = drop (otherParenPos + 1) revFirstPart
      in
        (reverse newFirst) ++ (reverse newSecond) ++ secondPart


-- | removeTaxonFromNewick recursively deletes taxa from list till all removed from tree,
--then returns newick
removeTaxonFromNewick :: [String] -> String -> String
removeTaxonFromNewick taxaToGo origNewick =
  if null taxaToGo then origNewick
  else 
      let taxonToGo = head taxaToGo
          taxLocation = findNameInString taxonToGo origNewick 0
      in
      if taxLocation == Nothing then error ("Error: Taxon " ++ taxonToGo ++ " not in tree")
      else 
        let firstPart = take (fromJust taxLocation) origNewick
            secondPart = drop ((fromJust taxLocation) + (length taxonToGo)) origNewick
        in
        removeTaxonFromNewick (tail taxaToGo) (makeNewTree firstPart secondPart)

-- | addTaxon2Newick adds a single taxon name as sister to one in the tree,
--then returns newick
addTaxon2Newick :: String -> String -> String -> String
addTaxon2Newick taxon2Add taxonPlace origNewick =
      let taxLocation = findNameInString taxonPlace origNewick 0
          tax2AddLocation = findNameInString taxon2Add origNewick 0
      in
      if taxLocation == Nothing then error ("Error: Taxon " ++ taxonPlace ++ " not in tree")
      else if tax2AddLocation /= Nothing then ("Error: Taxon " ++ taxon2Add ++ " already in tree")
      else 
        let firstPart = take (fromJust taxLocation) origNewick
            secondPart = drop ((fromJust taxLocation) + (length taxonPlace)) origNewick
        in
        (firstPart ++ "(" ++ taxonPlace ++ "," ++ taxon2Add ++ ")" ++ secondPart)

-- | appendString appends first string to end of second (for mapping purposes)
appendString :: String -> String -> String
appendString stringToAppend stringToBeAppended =
  if null stringToAppend then error "Nothing to append"
  else if null stringToBeAppended then error "Nothing to append to"
  else stringToBeAppended ++ stringToAppend

-- | printTrees takes trees, process, then adds ";\n" to each


-- | main driver
main :: IO ()
main = 
  do 
       args <- getArgs
       if (length args /= 3) then error ("Three arguments--add or delete, newick tree file (binary)," ++
            " and file of add/delete taxon names\n If taxa are to be added the file should only contain two Strings, the taxon name to add," ++
            " and the place where (as sister) it should be added.")
       else hPutStr stderr "Arguments: "
       mapM_ (hPutStrLn stderr) args
       hPutStrLn stderr ""
       let operation = head args
       let inNewick = (args !! 1 )
       if ((operation == "add") || (operation == "delete")) then hPutStrLn stderr ("Newick will " ++ operation ++ " taxon/a")
       else error ("Operation " ++ operation ++ "is unrecognized, must be 'add' or 'delete'")
       hPutStrLn stderr ("Openning newick  treefile " ++ inNewick ++ " and taxon/a to be added/deleted from file " ++ (args !! 2))
       hPutStrLn stderr "Warning--Tree must be dichotomous for deletion"
       treeFileHandle <- openFile inNewick ReadMode
       deleteTaxaHandle <- openFile (args !! 2) ReadMode
       rawTreeStuff <- hGetContents treeFileHandle --init so remove last empty String
       let rawTreeFile = init $ splitOn ";" rawTreeStuff
       hPutStrLn stderr ("Input of " ++ show (length rawTreeFile) ++ " trees")
       let treeFile = fmap (filter (`notElem` otherNewickChars)) rawTreeFile
       --removes branch lengths and terminal comment (ie POY tree cost)
       let cleanTreeFile = fmap (takeWhile (/= '[' ) . removeBranchLengths) treeFile
        --let cleanTreeFile = (takeWhile (/= '[' ) $ removeBranchLengths treeFile) ++ [';']
       deleteTaxaRaw <- hGetContents deleteTaxaHandle
       let deleteTaxa = words deleteTaxaRaw
       if operation == "delete" then mapM_ (hPutStrLn stdout) $ fmap (appendString ";\n") (fmap (removeTaxonFromNewick deleteTaxa) cleanTreeFile)
       else mapM_ (hPutStrLn stdout) $ fmap (appendString ";\n") (fmap (addTaxon2Newick (deleteTaxa !! 0) (deleteTaxa !! 1)) cleanTreeFile)
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
