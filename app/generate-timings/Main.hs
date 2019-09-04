{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE Strict            #-}

module Main where

import Control.Applicative
import Control.Arrow ((***))
import Control.DeepSeq
import Control.Monad
import Data.Bifunctor (first)
import Data.Decimal
import Data.Either
import Data.Foldable
import Data.IORef
import Data.Key
import Data.List               (intercalate, nub, sort)
import Data.List.NonEmpty      (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map                (Map, insertWith)
import Data.Ord
import Data.Semigroup          ((<>))
import Data.Semigroup.Foldable
import Data.Void
import InputParser
import GHC.Natural
import GHC.Generics (Generic)
import System.Directory
import System.Exit
import System.FilePath.Posix
import System.Process
import System.Timing
import TimingParameters
import Text.Megaparsec (Parsec, anySingleBut, choice, errorBundlePretty, lookAhead, parse, single, try)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)


data  DataFileReceipt
    = DataFileReceipt
    { receiptOfFilePath     :: FilePath
    , receiptOfDeletedTaxa  :: FilePath
    , receiptOfTaxaCount    :: Word
    , receiptOfStringLength :: Word
    } deriving (Generic, NFData, Show)


data  FilePoint
    = FilePoint
    { fileDataPath :: FilePath
    , fileTreePath :: FilePath
    , taxaCount    :: Word
    , stringLength :: Word
    } deriving (Eq, Show)


instance Ord FilePoint where

    compare lhs rhs =
      case comparing taxaCount lhs rhs of
        EQ -> comparing stringLength lhs rhs
        x  -> x


main :: IO ()
main = do
    opts <- force <$> parseTimingParameters >>= validateTimingParameters
    
    let alignedFile = dataFile      opts
        newick      = treeFile      opts

    coefficient  <- toEnum . length . head . tail       . lines <$> readFile alignedFile
    numFoundTaxa <- toEnum . length . filter isTaxaLine . lines <$> readFile alignedFile

    let strLens     = sort $ stringLengths opts
        taxaSizes   = nub . sort $ min numFoundTaxa <$> leafSetSizes opts
        taxaNumPad  = let n = length $ show numFoundTaxa
                      in  \x -> let s = show x in replicate (n - length s) ' ' <> s
        strLenPad   = let n = length $ show coefficient
                      in  \x -> let s = show x in replicate (n - length s) ' ' <> s

    createDirectoryIfMissing True replicationDataDirectory
    createDirectoryIfMissing True replicationTaxaDirectory
    createDirectoryIfMissing True replicationTreeDirectory

    counter <- newIORef (0, toEnum $ length strLens * length taxaSizes)

    -- Check if we need we told not to generate files
    filePoints <- if noGenerate opts
                  then getFilePoints alignedFile taxaSizes strLens coefficient
                  else do
      let reduceFastaFile = generateTruncatedDataFile taxaNumPad strLenPad alignedFile coefficient counter
      putStrLn "Creating reduced FASTA files with the following taxa counts and string lengths:"
      receipts <- sequenceA $ reduceFastaFile <$> taxaSizes <*> strLens
      let receiptMap = gatherReceiptsByTaxaCount $ NE.fromList receipts

      writeIORef counter (0, toEnum $ length taxaSizes)
      let reduceNewickFile = generateTruncatedTreeFile taxaNumPad newick counter
      putStrLn "Creating reduced Newick files with the following taxa counts:"
      NE.sort . foldl1 (<>) <$> traverseWithKey reduceNewickFile receiptMap  
    
    writeIORef counter (0, toEnum $ length filePoints)
    let timeFile = timeFilePoint taxaNumPad strLenPad counter (tcmFile opts)
    putStrLn "Timing alignment postorder & preorder with the following taxa counts and string lengths:"
    pointTimes <- traverse timeFile filePoints

    let (postorder, preorder) = (pointsToCSV *** pointsToCSV) $ colatePoints pointTimes
        prefix = getFileName alignedFile
    writeFile (prefix <> "-postorder-timing.csv")  postorder
    writeFile (prefix <>  "-preorder-timing.csv")   preorder


isTaxaLine :: String -> Bool
isTaxaLine v = headMay v == Just '>'
  where
    headMay xs = case xs of
                   []  -> Nothing
                   x:_ -> Just x


gatherReceiptsByTaxaCount :: NonEmpty DataFileReceipt -> Map (Word, FilePath) (NonEmpty DataFileReceipt)
gatherReceiptsByTaxaCount = foldr f mempty
  where
    f e = insertWith (<>) (receiptOfTaxaCount e, receiptOfDeletedTaxa e) $ pure e


getFilePoints :: FilePath -> [Word] -> [Rational] -> Word -> IO (NonEmpty FilePoint)
getFilePoints filePath taxaCounts strFractions coefficient = do
    fps <- foundFilePoints
    either reportMissingFiles pure $ collectErrors fps
  where
    reportMissingFiles = die . ("Could not find the following files:\n" <>) . unlines . toList

    foundFilePoints :: IO (NonEmpty (Either (NonEmpty FilePath) FilePoint))
    foundFilePoints = sequenceA $ do
        taxaNum <- NE.fromList taxaCounts
        strLen  <- NE.fromList $ truncate . (toRational coefficient *) <$> strFractions
        pure $ getFilePoint taxaNum strLen
        
    collectErrors
      :: NonEmpty (Either (NonEmpty FilePath) FilePoint)
      -> Either (NonEmpty FilePath) (NonEmpty FilePoint)
    collectErrors es =
        case partitionEithers $ toList es of
          ([], xs) -> Right . NE.sort $ NE.fromList xs
          (xs,  _) -> Left  . fold1   $ NE.fromList xs

    getFilePoint :: Word -> Word -> IO (Either (NonEmpty FilePath) FilePoint)
    getFilePoint taxaSize strLength = do 
        let onlyFileName = getFileName filePath
            treeFilePath = replicationTreeDirectory </> onlyFileName <.> show taxaSize <.> "tree"
            dataFilePath = replicationDataDirectory </> onlyFileName <.> show taxaSize <.> show strLength <.> "fasta"
        dataFileExists <- doesFileExist dataFilePath
        treeFileExists <- doesFileExist treeFilePath
        pure $ case (dataFileExists, treeFileExists) of
                (False, False) -> Left $ pure dataFilePath <> pure treeFilePath
                (False, True ) -> Left $ pure treeFilePath
                (True , False) -> Left $ pure dataFilePath
                (True , True ) -> Right $ FilePoint
                                  { fileDataPath = dataFilePath
                                  , fileTreePath = treeFilePath
                                  , taxaCount    = taxaSize
                                  , stringLength = strLength
                                  }


printCounter :: IORef (Word, Word) -> IO ()
printCounter counter = do 
    modifyIORef counter (first succ)
    (count, total) <- readIORef counter
    let sTotal = show total
        sCount = show count
        pCount = replicate (length sTotal - length sCount) ' ' <> sCount
    putStr $ mconcat ["[", pCount, "/", sTotal, "] "]


deleteFileIfExists :: FilePath -> IO ()
deleteFileIfExists p = do
    fileExists <- doesFileExist p
    when fileExists $ removeFile p


pointsToCSV :: Foldable f => f (Word, Word, Word) -> String
pointsToCSV points = unlines $ intercalate "," <$> dataRows
  where
--    headerRow = ["Leaf Count", "Sequence Length", "Runtime"]
    dataRows  = toRow <$> toList points
    toRow (x,y,z) = [show x, show y, show z]


colatePoints :: NonEmpty (Word, Word, CPUTime, CPUTime) -> (NonEmpty (Word, Word, Word), NonEmpty (Word, Word, Word))
colatePoints = foldMap1 f
  where
    f (x,y,z1,z2) = (pure (x,y, timeToWord z1), pure (x,y, timeToWord z2))

    timeToWord = naturalToWord . toMicroseconds


parseRuntimes :: String -> (CPUTime, CPUTime)
parseRuntimes str =
    case parse traversalRuntimes "STDOUT" str of
      Left  e -> error $ errorBundlePretty e
      Right v -> v


traversalRuntimes :: Parsec Void String (CPUTime, CPUTime)
traversalRuntimes = (,) <$> postT <*> preT
  where
    postT = space *> (lookAhead postorderRuntime <|> (many (anySingleBut '\n') *> postT))
    preT  = space *> (lookAhead  preorderRuntime <|> (many (anySingleBut '\n') *>  preT))
      
    postorderRuntime = string' "postorder:" *> space *> parseCPUTime 
    preorderRuntime  = string' "preorder:"  *> space *> parseCPUTime 


parseCPUTime :: Parsec Void String CPUTime
parseCPUTime = fmap fromPicoseconds . choice $ try <$> [days, hours, mins, secs, msecs, μsecs, nsecs, psecs]
  where
    days  = (+) <$> ((day     *) <$> (decimal <* single 'd')) <*> ((hour    *) <$> (decimal <* string "hrs"))
    hours = (+) <$> ((hour    *) <$> (decimal <* single 'h')) <*> ((minute  *) <$> (decimal <* string "min"))
    mins  = (+) <$> ((minute  *) <$> (decimal <* single 'm')) <*> ((second  *) <$> (decimal <* string "sec"))
    secs  = (+) <$> ((second  *) <$> (decimal <* single '.')) <*> ((mSecond *) <$> (decimal <* string "s"))
    msecs = (+) <$> ((mSecond *) <$> (decimal <* single '.')) <*> ((μSecond *) <$> (decimal <* string "ms"))
    μsecs = (+) <$> ((μSecond *) <$> (decimal <* single '.')) <*> ((nSecond *) <$> (decimal <* string "μs"))
    nsecs = (+) <$> ((nSecond *) <$> (decimal <* single '.')) <*> ((1       *) <$> (decimal <* string "ns"))
    psecs = (+) <$> ((1       *) <$> (decimal <* single '.')) <*> ( 0          <$              string "???ps")

    nSecond = 1000
    μSecond = 1000 * nSecond
    mSecond = 1000 * μSecond
    second  = 1000 * mSecond
    minute  = 60   *  second
    hour    = 60   *  minute
    day     = 24   *  hour


binaryDirectory :: FilePath
binaryDirectory = "./bin/"


replicationDirectory :: FilePath
replicationDirectory = "./replicate-results"


replicationDataDirectory :: FilePath
replicationDataDirectory = replicationDirectory </> "data"


replicationTaxaDirectory :: FilePath
replicationTaxaDirectory = replicationDirectory </> "taxa"


replicationTreeDirectory :: FilePath
replicationTreeDirectory = replicationDirectory </> "tree"


getFileName :: FilePath -> FilePath
getFileName = takeFileName . dropExtensions


generateTruncatedDataFile
  :: (Word -> String)
  -> (Word -> String)
  -> FilePath
  -> Word
  -> IORef (Word, Word)
  -> Word
  -> Rational
  -> IO DataFileReceipt
generateTruncatedDataFile taxaNumPadder strLenPadder filePath coefficient counter taxaSize fraction = do
    prefix <- makeAbsolute "."
    binDir <- makeAbsolute binaryDirectory
    let strLength    = truncate $ toRational coefficient * fraction
        onlyFileName = getFileName filePath
        binFilePath  = binDir </> "reduce-fasta"
        dataFilePath = prefix </> filePath
        taxaFilePath = prefix </> replicationTaxaDirectory </> onlyFileName <.> show taxaSize
        lessFilePath = prefix </> replicationDataDirectory </> onlyFileName <.> show taxaSize <.> show strLength <.> "fasta"
        decimalVer   =
          case normalizeDecimal <$> (eitherFromRational fraction :: Either String (DecimalRaw Natural)) of
            Left    _ -> show (fromRational fraction :: Double)
            Right dec -> show dec
        commandStr   = unwords
            [ binFilePath
            , dataFilePath
            , show taxaSize
            , decimalVer
            , taxaFilePath
            ]

    _ <- deleteFileIfExists taxaFilePath
    _ <- deleteFileIfExists lessFilePath

    let p = CreateProcess
            { cmdspec            = ShellCommand commandStr
            , cwd                = Nothing
            , env                = Nothing
            , std_in             = NoStream
            , std_out            = Inherit
            , std_err            = NoStream
            , close_fds          = True
            , create_group       = False
            , delegate_ctlc      = False
            , detach_console     = False
            , create_new_console = False
            , new_session        = False
            , child_group        = Nothing
            , child_user         = Nothing
            , use_process_jobs   = False
            }

    printCounter counter 
    putStrLn $ unwords [ taxaNumPadder taxaSize, strLenPadder strLength ]

    (_exitCode, stdOut, _stdErr) <- readCreateProcessWithExitCode p ""

    writeFile lessFilePath stdOut
    
    pure . force $ DataFileReceipt
        { receiptOfFilePath     = lessFilePath
        , receiptOfDeletedTaxa  = taxaFilePath
        , receiptOfTaxaCount    = taxaSize
        , receiptOfStringLength = strLength
        }


generateTruncatedTreeFile
  :: (Word -> String)
  -> FilePath
  -> IORef (Word, Word)
  -> (Word, FilePath)
  -> NonEmpty DataFileReceipt
  -> IO (NonEmpty FilePoint)
generateTruncatedTreeFile taxaNumPadder filePath counter (taxaSize, taxaFilePath) receipts = do
    prefix <- makeAbsolute "."
    binDir <- makeAbsolute binaryDirectory
    let onlyFileName = getFileName filePath
        binFilePath  = binDir </> "newick-add-delete-taxon"
        treeFilePath = prefix </> filePath
        lessFilePath = prefix </> replicationTreeDirectory </> onlyFileName <.> show taxaSize <.> "tree"
        commandStr   = unwords
            [ binFilePath
            , "delete"
            , treeFilePath
            , taxaFilePath <.> "deleted"
            ]

    _ <- deleteFileIfExists lessFilePath

    let p = CreateProcess
            { cmdspec            = ShellCommand commandStr
            , cwd                = Nothing
            , env                = Nothing
            , std_in             = NoStream
            , std_out            = Inherit
            , std_err            = NoStream
            , close_fds          = True
            , create_group       = False
            , delegate_ctlc      = False
            , detach_console     = False
            , create_new_console = False
            , new_session        = False
            , child_group        = Nothing
            , child_user         = Nothing
            , use_process_jobs   = False
            }

    printCounter counter 
    putStrLn $ taxaNumPadder taxaSize

    (_exitCode, stdOut, _stdErr) <- readCreateProcessWithExitCode p ""

    writeFile lessFilePath stdOut

    let makeFilePoint dfr = FilePoint
          { fileDataPath = receiptOfFilePath dfr
          , fileTreePath = lessFilePath
          , taxaCount    = taxaSize
          , stringLength = receiptOfStringLength dfr
          }
    
    pure $ makeFilePoint <$> receipts


timeFilePoint
  :: (Word -> String)
  -> (Word -> String)
  -> IORef (Word, Word)
  -> FilePath
  -> FilePoint
  -> IO (Word, Word, CPUTime, CPUTime)
timeFilePoint taxaNumPadder strLenPadder counter tcmPath fp = do
    prefix <- makeAbsolute "."
    binDir <- makeAbsolute binaryDirectory
    let binFilePath  = binDir </> "tree-align"
        tcmFilePath  = prefix </> tcmPath
        dataFilePath = prefix </> fileDataPath fp
        treeFilePath = prefix </> fileTreePath fp
        commandStr   = unwords
            [ binFilePath
            , "--dna"
            , "--timing"
            , "--data"
            , dataFilePath
            , "--tree"
            , treeFilePath
            , "--tcm"
            , tcmFilePath
            , "--output"
            , "/dev/null"
            ]

    let p = CreateProcess
            { cmdspec            = ShellCommand commandStr
            , cwd                = Nothing
            , env                = Nothing
            , std_in             = NoStream
            , std_out            = Inherit
            , std_err            = NoStream
            , close_fds          = True
            , create_group       = False
            , delegate_ctlc      = False
            , detach_console     = False
            , create_new_console = False
            , new_session        = False
            , child_group        = Nothing
            , child_user         = Nothing
            , use_process_jobs   = False
            }

    printCounter counter
--    putStrLn $ fileDataPath fp
    putStrLn $ unwords [ taxaNumPadder $ taxaCount fp
                       ,  strLenPadder $ stringLength fp
                       ]

    (_exitCode, stdOut, _stdErr) <- readCreateProcessWithExitCode p ""

    let (postOrder, preOrder) = parseRuntimes stdOut
    pure $ force (taxaCount fp, stringLength fp, postOrder, preOrder)
