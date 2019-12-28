{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Text.Megaparsec.Custom
  ( (<:>)
  , double
  , endOfLine
  , fails
  , inlineSpace
  ) where

import           Data.Char                  (isSpace)
import           Data.Functor               (($>))
import           Data.List.NonEmpty         (NonEmpty (..), nonEmpty)
import           Data.Maybe                 (mapMaybe)
import qualified Data.Set                   as S
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as LEX


-- |
-- Prepend a single combinator result element to the combinator result of a list
-- of elements.
(<:>)  :: Applicative f => f a -> f [a] -> f [a]
(<:>)  a b = (:)  <$> a <*> b


-- |
-- Flexibly parses a 'Double' value represented in a variety of forms.
double :: (MonadParsec e s m, Token s ~ Char) => m Double
double = try real <|> fromIntegral <$> int
  where
     int  :: (MonadParsec e s m, Token s ~ Char) => m Integer
     int  = LEX.signed space LEX.decimal
     real = LEX.signed space LEX.float


-- |
-- Custom 'eol' combinator to account for /very/ old Mac file formats ending
-- lines in a single @\'\\r\'@.
endOfLine :: (Enum (Token s), MonadParsec e s m) => m (Token s)
endOfLine = choice (try <$> [ nl, cr *> nl, cr ]) $> newLineChar
  where
    newLineChar  = enumCoerce '\n'
    carriageChar = enumCoerce '\r'
    nl = tokenMatch newLineChar  $> ()
    cr = tokenMatch carriageChar $> ()


-- |
-- Accepts zero or more Failure messages.
fails :: MonadParsec e s m => [String] -> m a
fails = failure Nothing . S.fromList . fmap Label . mapMaybe nonEmpty


-- |
-- Consumes a whitespace character that is not a newline character.
inlineSpaceChar :: (Enum (Token s), MonadParsec e s m) => m (Token s)
inlineSpaceChar = token captureToken errItems
  where
    captureToken x
      | isInlineSpace x = Just x
      | otherwise       = Nothing

    errItems = S.singleton (Label ('I':|"line space character"))

    isInlineSpace x = and $
        [ isSpace . enumCoerce
        , (newLineChar  /=)
        , (carriageChar /=)
        ] <*> [x]

    newLineChar  = enumCoerce '\n'
    carriageChar = enumCoerce '\r'


-- |
-- Consumes zero or more whitespace characters that are not newline characters.
inlineSpace :: (Enum (Token s), MonadParsec e s m) => m ()
inlineSpace = skipMany inlineSpaceChar


-- |
-- Convert one Enum to another through the Int value.
enumCoerce :: (Enum a, Enum b) => a -> b
enumCoerce = toEnum . fromEnum


-- |
-- Matches a single token.
tokenMatch :: (MonadParsec e s m) => Token s -> m (Token s)
tokenMatch tok = satisfy (== tok)
