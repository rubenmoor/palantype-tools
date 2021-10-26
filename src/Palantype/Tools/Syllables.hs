{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Palantype.Tools.Syllables where

import           Control.Applicative    (Alternative ((<|>)))
import           Control.Category       (Category ((.)))
import           Control.Exception.Base (mapException)
import           Control.Monad          (Monad ((>>), (>>=)), MonadPlus (mzero),
                                         guard, unless, when)
import           Data.Bool              (not, otherwise, (&&), (||))
import           Data.Char              (Char, isLetter)
import           Data.Either            (Either (..), isRight)
import           Data.Eq                ((==))
import           Data.Foldable          (Foldable (elem))
import           Data.Function          (($))
import           Data.Functor           (Functor (fmap), void, (<$>))
import           Data.List              (intercalate, intersperse)
import           Data.Maybe             (Maybe (..), catMaybes, isNothing)
import           Data.Semigroup         (Semigroup ((<>)))
import           Data.String            (String)
import           Data.Text              (Text, replace)
import qualified Data.Text              as Text
import           Debug.Trace            (traceShow)
import           GHC.Generics           (Generic)
import           Prelude                (Applicative (pure, (*>), (<*)),
                                         Eq ((/=)), Foldable (null),
                                         Monoid (mconcat))
import           Text.Parsec            (ParseError, Parsec, anyChar, char,
                                         evalParser, getInput, getState, letter,
                                         many, many1, manyTill, noneOf,
                                         notFollowedBy, oneOf, parse,
                                         parserTrace, runParser, satisfy,
                                         sepBy1, setState, space, spaces,
                                         string, try)
import           Text.Show              (Show (show))
import           TextShow               (TextShow (..), fromText)
import           TextShow.Generic       (genericShowbPrec)
import Data.Bool (Bool(False))

data SyllableData = SyllableData
  { sdWord      :: Text
  , sdSyllables :: [Text]
  }
  deriving (Eq)

instance TextShow SyllableData where
  showb SyllableData {..} =
    fromText sdWord <> " " <> fromText (mconcat $ intersperse "|" sdSyllables)

data Result
  = Success SyllableData
  | Failure ParseError
  | Exception Exception
  deriving (Generic)

instance Eq Result where
  Success sd1 == Success sd2 = sd1 == sd2
  _ == _ = False

instance TextShow Result where
  showb = \case
    Success sd    -> fromText "Success: " <> showb sd
    Failure err   -> fromText $ "Failure: " <> Text.pack (show err)
    Exception exc -> fromText "Exception: " <> showb exc

data Exception
  = ExceptionAbbreviation
  | ExceptionMultiple
  | ExceptionSpecialChar Char
  | ExceptionSingleLetter
  | ExceptionEllipsis
  deriving (Generic)

instance TextShow Exception where
  showbPrec = genericShowbPrec

parseSyllables :: Text -> [Result]
parseSyllables =
    fmap parseSyllables' . parseOptionalChars
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
        void $ many $ try (satisfy (not . isLetter) *> notFollowedBy sep)
        result <- Text.pack . catMaybes <$> manyTill someChar (try sep)
        when (Text.length result == 1) $ setState $ Just ExceptionSingleLetter
        when (isRight $ parse ellipsis "" result) $
          setState $ Just ExceptionEllipsis
        rem <- getInput
        pure (result, rem)
      where
        sep = void $ many1 space *> string ">>>" *> many1 space
        ellipsis = void $ many (noneOf ".") *> string "..."

    someChar :: Parsec Text (Maybe Exception) (Maybe Char)
    someChar = do
      c <- anyChar
      mExc <- getState
      when (isNothing mExc) $
        case c of
          '.' -> setState $ Just ExceptionAbbreviation
          ' ' -> setState $ Just ExceptionMultiple
          c' | not (isLetter c || c `elem` ("-!®" :: String)) -> setState $ Just $ ExceptionSpecialChar c
          _ -> pure ()
      pure $ case c of
               '!' -> Nothing
               '®' -> Nothing
               _   -> Just c

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

parseOptionalChars :: Text -> [Text]
parseOptionalChars str =
  case runParser optionalChars () "" str of
    Left  _ -> [str]
    Right (cs, cts) ->
      [ replaceFirst ("[" <> cts <> "]") "" $ replaceFirst ("[" <> cs <> "]") "" str
      , replaceFirst ("[" <> cts <> "]") cts $ replaceFirst ("[" <> cs <> "]") cs str
      ]

optionalChars :: Parsec Text () (Text, Text)
optionalChars = do
    void $ manyTill anyChar (char '[')
    cs <- Text.pack <$> many1 letter
    void $ char ']'
    void $ manyTill anyChar (try $ string ">>>")
    void $ manyTill anyChar $ char '['
    cts <- Text.pack <$> many1 (letter <|> char '|')
    void $ char ']'
    pure (cs, cts)


replaceFirst :: Text -> Text -> Text -> Text
replaceFirst needle replacement haystack
  | Text.null back = haystack    -- pattern doesn't occur
  | otherwise = Text.concat [front, replacement, Text.drop (Text.length needle) back]
    where
      (front, back) = Text.breakOn needle haystack
