{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Palantype.Tools.Syllables where

import           Control.Applicative    (Alternative ((<|>)), Applicative ((<*>)), optional)
import           Control.Category       (Category ((.)))
import           Control.Exception.Base (mapException)
import           Control.Monad          (Monad ((>>), (>>=)), MonadPlus (mzero),
                                         guard, unless, when)
import           Data.Bool              (not, otherwise, (&&), (||))
import           Data.Char              (Char, isLetter)
import           Data.Either            (Either (..), isRight)
import           Data.Eq                ((==))
import           Data.Foldable          (Foldable (elem), notElem)
import           Data.Function          (($))
import           Data.Functor           (Functor (fmap), void, (<$>), ($>))
import           Data.List              (intercalate, intersperse, tail, (++))
import           Data.Maybe             (Maybe (..), catMaybes, isNothing, fromMaybe)
import           Data.Semigroup         (Semigroup ((<>)))
import           Data.String            (String)
import           Data.Text              (Text, replace)
import qualified Data.Text              as Text
import           Debug.Trace            (traceShow)
import           GHC.Generics           (Generic)
import           Prelude                (Applicative (pure, (*>), (<*)),
                                         Eq ((/=)), Foldable (null),
                                         Monoid (mconcat), error)
import           Text.Parsec            (ParseError, Parsec, anyChar, char,
                                         evalParser, getInput, getState, letter,
                                         many, many1, manyTill, noneOf,
                                         notFollowedBy, oneOf, parse,
                                         parserTrace, runParser, satisfy,
                                         sepBy1, setState, space, spaces,
                                         string, try, updateState, lookAhead)
import           Text.Show              (Show (show))
import           TextShow               (TextShow (..), fromText)
import           TextShow.Generic       (genericShowbPrec)
import Data.Bool (Bool(False))
import Text.ParserCombinators.Parsec.Error (newErrorMessage)
import Text.Parsec.Error (Message(Message))
import Text.ParserCombinators.Parsec.Pos (initialPos)
import GHC.Base (undefined)

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

-- | turn "zusammenhang[s]los >>> zu|sam|men|hang[s]|los ..." into
--   [ Success (SyllableData "zusammenhanglos" ["zu", "sam", "men", "hang", "los"])
--   , Success (SyllableData "zusammenhangslos" ["zu", "sam", "men", "hangs", "los"])
--   ]
parseSyllables :: Text -> [Result]
parseSyllables =
    fmap parseSyllables' . parseOptionalChars
  where
    parseSyllables' :: Text -> Result
    parseSyllables' str =
      case evalParser word Nothing "" str of
        Left  err               -> Failure err
        Right (Just exc, _)     -> Exception exc
        Right (Nothing, (sdWord, rem)) ->
          let st = if Text.null sdWord
                      then error $ Text.unpack $ "Empty state: " <> str
                      else Text.unpack sdWord
          -- let st = Text.unpack $ replace "-" "" sdWord
          in case evalParser syllables st "" rem of
               Right ("", sdSyllables) -> Success $ SyllableData {..}
               Right (str, _)          -> Failure $ newErrorMessage (Message $ "Failed to parse syllalbes; remaining state: " <> str) (initialPos "")
               Left  err               -> Failure err

    -- | parse "zusammenhangslos >>> zu|sam|men|hangs|los ..." into
    --   ("zusammenhangslos", "zu|sam|men|hangs|los ...")
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

    -- | parse "zu|sam|men|hangs|los" into ["zu", "sam", "men", "hangs", "los"]
    syllables :: Parsec Text String [Text]
    syllables = do
        (x:_) <- getState
        void $ many $ satisfy (/= x)
        mconcat <$>
          sepBy1 ((++) <$> optionalHyphen <*> syllable)
                 (try (char '|') <|> lookAhead (next '-'))

      where
        syllable =
          try bio <|> try pseudoSyllable <|> realSyllable

        pseudoSyllable = do
            v1 <- vowel
            (v1 :) <$> (try bio <|> pseudoSyllable')
          where
            pseudoSyllable' = do
              c1 <- Text.pack <$> many1 consonant
              v2 <- vowel
              rem <- Text.pack <$> many nextChar
              pure [c1 <> v2 <> rem]

        bio = do
          b <- next 'b'
          i <- next 'i'
          o <- next 'o'
          pure [Text.pack [b, i], Text.singleton o]

        realSyllable = do
          pure . Text.pack <$> many1 nextChar

        optionalHyphen =
          try (pure . Text.singleton <$> next '-') <|> pure []

        nextChar :: Parsec Text String Char
        nextChar =
          getState >>= \case
            (x:xs) | x /= '-' -> do
                       setState xs
                       char x
            _ -> mzero

        next
          :: Char
          -> Parsec Text String Char
        next c =
          getState >>= \case
            (x:xs) | x == c -> do
              setState xs
              char x
            _      -> mzero

        vowel =
          getState >>= \case
            (x:xs) | x `elem` vowels -> do
                       setState xs
                       Text.singleton <$> char x
            _ -> mzero

        consonant =
          getState >>= \case
            (x:xs) | x `notElem` ('-' : vowels) -> do
                       setState xs
                       char x
            _ -> mzero

vowels :: String
vowels = "AEIOUYÄÖÜÁÀÂÉÈÊÍÓÔÚaeiouyäöüáàâåéèêëíóôøû"

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
