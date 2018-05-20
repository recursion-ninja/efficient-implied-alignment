{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module File.Format.TransitionCostMatrix.Parser
  ( TCM(..)
  , tcmStreamParser
  ) where


import           Data.Char                     (isSpace)
import           Data.Foldable                 (toList)
import           Data.List                     (elemIndex)
import           Data.List.NonEmpty            (NonEmpty)
import qualified Data.List.NonEmpty      as NE
import           Data.List.Utility             (duplicates, mostCommon)
import           Data.Matrix.ZeroIndexed       (Matrix, ncols, nrows)
import qualified Data.Matrix.ZeroIndexed as M
import           Data.Maybe                    (catMaybes, fromJust)
import           Data.Scientific               (toRealFloat)
import           Data.Semigroup
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Custom
import           Text.Megaparsec.Char.Lexer    (scientific)


-- |
--Intermediate parse result prior to consistancy validation
data TCMParseResult 
   = TCMParseResult (NonEmpty Char) (Matrix Double) deriving (Show)


-- |
-- The results of a TCM file consisting of
--
--   * A custom alphabet of 'Symbol's
--
--   * A matrix consisting of the transition costs between symbols
--
-- The following equality will hold for an 'TCM':
--
-- > (length . customAlphabet) tcm == (nrows . transitionCosts) tcm && (length . customAlphabet) tcm == (ncols . transitionCosts) tcm
--
-- Note that the 'transitionCosts` does not need to be a symetic matrix nor have identity values on the matrix diagonal.
data TCM 
   = TCM
   { -- | The custom alphabet of 'Symbols' for which the TCM matrix is defined
     customAlphabet  :: NonEmpty Char
     -- | The cost to transition between any two symbols, square but not necessarily symetric
   , transitionCosts :: Matrix Double -- n+1 X n+1 matrix where n = length customAlphabet
   } deriving (Show)


-- |
-- Parses the entirety of a stream producing a TCM result.
-- The result will contain an Alphabet with no duplicate elements
-- and a square Matrix with dimension @(n+1) x (n+1)@ where @n@ is
-- the length of the Alphabet.
tcmStreamParser :: (MonadParsec e s m, Token s ~ Char) => m TCM
tcmStreamParser = validateTCMParseResult =<< tcmDefinition <* eof


-- |
-- Parses an intermediary result consisting of an Alphabet and a Matrix.
-- Both the Alphabet and Matrix have been validated independantly for
-- consistencey, but no validation has been performed to ensure that the
-- dimensions of the Matrix and the length of the Alphabet are consistant
-- with each other. 
tcmDefinition :: (MonadParsec e s m, Token s ~ Char) => m TCMParseResult
tcmDefinition = do
    _        <- space
    alphabet <- symbol tcmAlphabet
    matrix   <- symbol tcmMatrix
    pure $ TCMParseResult alphabet matrix


-- |
-- Shorthand for the expected format of the alphabet lin in a TCM file.
-- The same as 'alphabetLine inlineSpace'.
tcmAlphabet :: (MonadParsec e s m, Token s ~ Char) => m (NonEmpty Char)
tcmAlphabet = alphabetLine inlineSpace


-- |
-- Shorthand for the expected format of the matrix block in a TCM file
-- The same as 'matrixBlock inlineSpace'.
tcmMatrix   :: (MonadParsec e s m, Token s ~ Char) => m (Matrix Double)
tcmMatrix   = matrixBlock  inlineSpace


-- |
-- The 'alphabetLine' function takes a combinator to consume delimiters between
-- elements in the alphabet line and returns a list of elements in the alphabet.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> parse (alphabetLine inlineSpace) "" "a b c d\n"
-- Right ['a','b','c','d']
--
-- >>> parse (alphabetLine (inlineSpace *> char '|' <* inlineSpace)) "" "2 | 3 | 5 | 7\n"
-- Right ["2","3","5","7"]
alphabetLine :: (MonadParsec e s m, Token s ~ Char) => m () -> m (NonEmpty Char)
alphabetLine spacing = validateAlphabet =<< NE.fromList <$> ((alphabetSymbol <* spacing) `someTill` endOfLine)
  where
    alphabetSymbol :: (MonadParsec e s m, Token s ~ Char) => m Char
    alphabetSymbol = satisfy (not . isSpace)


-- |
-- The 'matrixBlock' function takes a combinator to consume delimiters between
-- entries in a line of the matrix and returns a square 'Matrix Double'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> parse (matrixBlock inlineSpace) "" "1 2 3 \n 4 5 6\n7 8 9\n"
-- Right (( 1 2 3 )
--        ( 4 5 6 )
--        ( 7 8 9 ))
--
-- >>> parse (matrixBlock (char ':') "" "1.0:1.0\n1.0:0.0"
-- Right (( 1 2 )
--        ( 3 4 ))
matrixBlock :: (MonadParsec e s m, Token s ~ Char) => m () -> m (Matrix Double)
matrixBlock spacing = validateMatrix =<< many (symbol matrixRow)
  where
    matrixRow   = (spacing *> matrixEntry <* spacing) `manyTill` endOfLine
    matrixEntry = double


-- |
-- Validates that the dimensions of the Matrix are @(n+1) x (n+1)@
-- where @n@ is the length of the Alphabet.
validateTCMParseResult :: (MonadParsec e s m {- , Token s ~ Char -}) => TCMParseResult -> m TCM
validateTCMParseResult (TCMParseResult alphabet matrix)
  | dimMismatch  = fail errorMessage
  | otherwise    = pure result
  where
    result =
        case '-' `elemIndex` toList alphabet of
          Nothing -> TCM (alphabet <> pure '-') matrix
          Just k  -> let permuted = NE.fromList (NE.filter (/='-') alphabet) <> pure '-'
                         g (i,j)  = let i' = if i < k then i else if i == size - 1 then k else i + 1
                                        j' = if j < k then i else if j == size - 1 then k else j + 1
                                    in  M.unsafeGet i' j' matrix
                     in  TCM permuted $ M.matrix rows cols g

    
    size         = length alphabet + if '-' `elem` alphabet then 0 else 1
    rows         = nrows matrix
    cols         = ncols matrix
    dimMismatch  = size /= rows || size /= cols
    errorMessage = concat
        [ "The alphabet length is "
        , show size
        , " but the matrix dimensions are "
        , show rows
        , " x "
        , show cols
        , ". The expected matrix dimensions were "
        , show $ size + 1
        , " x "
        , show $ size + 1
        , "."
        ]


-- |
-- Validates the information contained in the Alphabet.
--
-- Ensures that the Alphabet:
--
--   * Is not empty
--
--   * Contains no duplicate elements
--
validateAlphabet :: (MonadParsec e s m, Token s ~ Char) => NonEmpty Char -> m (NonEmpty Char)
validateAlphabet alphabet
  | duplicatesExist = fail $ "The following symbols were listed multiple times in the custom alphabet: " <> show dupes
  | tooManySymbols  = fail $ "The alphabet has more than 32 symbols (inluding gap). This is a technical limitation for efficiency."
  | otherwise       = pure alphabet 
  where
    tooManySymbols  = if '-' `elem` alphabet then length alphabet > 32 else length alphabet > 31
    duplicatesExist = not $ null dupes
    dupes           = duplicates $ toList alphabet


-- |
-- Validates the information contained in the Matrix constitutes a square matrix.
--
-- Ensures that the Matrix:
--
--   * Is not empty
--
--   * Each row has the same number of columns
--
--   * The number of rows match the number of columns
--
validateMatrix :: (MonadParsec e s m {- , Token s ~ Char -}) => [[Double]] -> m (Matrix Double)
validateMatrix matrix
  | null matrix        = fail "No matrix specified"
  | null matrixErrors  = pure . M.fromList rows cols $ concat matrix
  | otherwise          = fails matrixErrors
  where
    rows               = length matrix
    cols               = fromJust . mostCommon $ length <$> matrix
    badCols            = foldr getBadCols [] $ zip [(1::Int)..] matrix
    getBadCols (n,e) a = let x = length e in if x /= cols then (n,x):a else a  
    colMsg (x,y)       = (:) (Just $ mconcat [ "Matrix row ", show x, " has ", show y, " columns but ", show cols, " columns were expected"])
    matrixErrors       = catMaybes $ badRowCount : badColCount
    badColCount        = foldr colMsg [] badCols
    badRowCount        = if   rows == cols
                         then Nothing
                         else Just $ concat
                             [ "The matrix is not a square matrix. The matrix has "
                             , show rows
                             , " rows but "
                             , show cols
                             , " rows were expected"
                             ]


-- |
-- Whitespace consuming combinator wrapper
symbol  :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
symbol  x = x <* space
