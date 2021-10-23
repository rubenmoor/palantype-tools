{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Palantype.Tools.Syllables where

import           Control.Category (Category ((.)))
import           Control.Monad    (Monad ((>>), (>>=)), MonadPlus (mzero), when, unless, guard)
import           Data.Char        (Char, isLetter)
import           Data.Either      (Either (..))
import           Data.Function    (($))
import           Data.Functor     (Functor (fmap), void, (<$>))
import           Data.List        (intersperse, intercalate)
import           Data.Semigroup   (Semigroup ((<>)))
import           Data.String      (String)
import           Data.Text        (Text, replace)
import qualified Data.Text        as Text
import           Prelude          (Applicative (pure, (<*), (*>)), Foldable (null),
                                   Monoid (mconcat), Eq ((/=)))
import           Text.Parsec      (ParseError, Parsec, anyChar, char, getState,
                                   letter, many1, manyTill, notFollowedBy,
                                   runParser, sepBy1, setState, string, try, evalParser, many, getInput, noneOf, parserTrace, spaces, space, satisfy)
import           TextShow         (TextShow (..), fromText)
import Control.Applicative (Alternative((<|>)))
import Data.Eq ((==))
import Data.Maybe (Maybe (..), isNothing)
import Control.Exception.Base (mapException)
import Data.Bool ((&&), not, (||))
import TextShow.Generic (genericShowbPrec)
import GHC.Generics (Generic)
import Text.Show (Show(show))
import Data.Foldable (Foldable(elem))
import Debug.Trace (traceShow)

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
  deriving (Generic)

instance TextShow Result where
  showb = \case
    Success sd  -> fromText "Success: " <> showb sd
    Failure err -> fromText $ "Failure: " <> Text.pack (show err)
    Exception exc -> fromText "Exception: " <> showb exc

data Exception
  = ExceptionAbbreviation
  | ExceptionMultiple
  | ExceptionSpecialChar Char
  deriving (Generic)

instance TextShow Exception where
  showbPrec = genericShowbPrec

parseSyllables :: Text -> [Result]
parseSyllables = fmap parseSyllables' . parseOptionalChar
  where
    parseSyllables' :: Text -> Result
    parseSyllables' str =
      case evalParser word Nothing "" str of
        Left err                -> Failure err
        Right (Just exc, _)     -> Exception exc
        Right (Nothing, (sdWord, rem)) ->
          let st = Text.unpack $ replace "-" "" sdWord
          in case runParser syllables st "" rem of
               Right sdSyllables -> Success $ SyllableData {..}
               Left  err         -> Failure err

    word :: Parsec Text (Maybe Exception) (Text, Text)
    word = do
        void $ many (try $ anyChar *> notFollowedBy letter)
        result <- Text.pack <$> manyTill someChar (try sep)
        rem <- getInput
        pure (result, rem)
      where
        sep = void $ many1 space *> string ">>>" *> many1 space

    someChar :: Parsec Text (Maybe Exception) Char
    someChar = do
      c <- anyChar
      mExc <- getState
      when (isNothing mExc) $
        case c of
          '.' -> setState $ Just ExceptionAbbreviation
          ' ' -> setState $ Just ExceptionMultiple
          c' | not (isLetter c || c == '-') -> setState $ Just $ ExceptionSpecialChar c
          _ -> pure ()
      pure c

    syllables :: Parsec Text String [Text]
    syllables = do
        (x:_) <- getState
        void $ many $ satisfy (/= x)
        mconcat <$> sepBy1 (intersperse "-" <$> sepByHyphen) (char '|')
      where
        sepByHyphen =
          sepBy1 (Text.pack <$> many1 character) $ char '-'

    character :: Parsec Text String Char
    character =
      getState >>= \case
        []     -> mzero
        (x:xs) -> do
          setState xs
          char x

parseOptionalChar :: Text -> [Text]
parseOptionalChar str =
  case runParser optionalChar () "" str of
    Left  _ -> [str]
    Right c ->
      let c' = Text.singleton c
      in  [ replace ("[" <> c' <> "]") "" str
          , replace ("[" <> c' <> "]") c' str
          ]

optionalChar :: Parsec Text () Char
optionalChar = do
  void $ manyTill (anyChar <* notFollowedBy (try $ string ">>>")) $ char '['
  c <- letter
  void $ char ']'
  pure c
