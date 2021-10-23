{-# LANGUAGE RecordWildCards #-}

module Palantype.Tools.Syllables where

import           Control.Category (Category ((.)))
import           Control.Monad    (Monad ((>>), (>>=)), MonadPlus (mzero), when, unless)
import           Data.Char        (Char, isLetter)
import           Data.Either      (Either (..))
import           Data.Function    (($))
import           Data.Functor     (Functor (fmap), void, (<$>))
import           Data.List        (intersperse)
import           Data.Semigroup   (Semigroup ((<>)))
import           Data.String      (String)
import           Data.Text        (Text, intercalate, replace)
import qualified Data.Text        as Text
import           Prelude          (Applicative (pure, (<*), (*>)), Foldable (null),
                                   Monoid (mconcat))
import           Text.Parsec      (ParseError, Parsec, anyChar, char, getState,
                                   letter, many1, manyTill, notFollowedBy,
                                   runParser, sepBy1, setState, string, try, evalParser)
import           TextShow         (TextShow (..), fromText)
import Control.Applicative (Alternative((<|>)))
import Data.Eq ((==))
import Data.Maybe (Maybe (..), isNothing)
import Control.Exception.Base (mapException)
import Data.Bool ((&&), not)

data SyllableData = SyllableData
  { sdWord      :: Text
  , sdSyllables :: [Text]
  }

instance TextShow SyllableData where
  showb SyllableData {..} =
    fromText sdWord <> " " <> fromText (mconcat $ intersperse "|" sdSyllables)

data Result
  = Success SyllableData
  | Failure ParseError
  | Exception Exception

data Exception
  = ExceptionAbbreviation
  | ExceptionMultiple
  | ExceptionSpecialChar Char

parseSyllables :: Text -> [Result]
parseSyllables = fmap parseSyllables' . parseOptionalChar
  where
    parseSyllables' :: Text -> Result
    parseSyllables' str =
      case evalParser word Nothing "" str of
        Left err                -> Failure err
        Right (Just exc, _)     -> Exception exc
        Right (Nothing, sdWord) ->
          case runParser syllables (Text.unpack sdWord) "" sdWord of
            Right sdSyllables -> Success $ SyllableData {..}
            Left  err         -> Failure err

    word :: Parsec Text (Maybe Exception) Text
    word = do
      void $ many1 (anyChar *> notFollowedBy letter)
      Text.pack <$> manyTill someChar (try $ string " >>> ")

    someChar :: Parsec Text (Maybe Exception) Char
    someChar = do
      c <- anyChar
      mExc <- getState
      when (isNothing mExc) $ do
        when (c == ' ') $ setState $ Just ExceptionMultiple
        when (c == '.') $ setState $ Just ExceptionAbbreviation
        unless (isLetter c) $ setState $ Just $ ExceptionSpecialChar c
      pure c

      -- let sdWord = Text.pack w
    syllables :: Parsec Text String [Text]
    syllables = sepBy1 (Text.pack . mconcat <$> many1 character) (char '|')

    character :: Parsec Text String String
    character = do
      getState >>= \case
        []     -> mzero
        (x:xs) -> do
          setState xs
          c <- char x
          pure $ if c == '-' then "|-|" else [c]

parseOptionalChar :: Text -> [Text]
parseOptionalChar str =
  case runParser optionalChar () "" str of
    Left  _ -> [str]
    Right c ->
      let c' = Text.singleton c
      in  [ replace ("[" <> c' <> "]") "" str
          , replace ("[" <> c' <> "]") "c" str
          ]

optionalChar :: Parsec Text () Char
optionalChar = do
  void $ manyTill (anyChar <* notFollowedBy (try $ string ">>>")) $ char '['
  c <- letter
  void $ char ']'
  pure c
