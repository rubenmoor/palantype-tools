{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DerivingStrategies #-}

module Palantype.Tools.Prepare where

import           Control.Applicative            ( Alternative((<|>))
                                                , Applicative((<*>))
                                                )
import           Control.Category               ( Category((.)) )
import           Control.Monad                  ( Monad((>>=))
                                                , MonadPlus(mzero)
                                                , when
                                                )
import           Data.Bool                      ( (&&)
                                                , Bool(False)
                                                , bool
                                                , not
                                                , otherwise
                                                , (||)
                                                )
import           Data.Char                      ( Char
                                                , isLetter
                                                , isLower
                                                )
import           Data.Either                    ( Either(..)
                                                )
import           Data.Eq                        ( (==) )
import           Data.Foldable                  ( Foldable(elem)
                                                , notElem
                                                )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>)
                                                , void
                                                )
import           Data.List                      ( (++)
                                                )
import           Data.Maybe                     ( Maybe(..)
                                                , catMaybes
                                                , isNothing, isJust
                                                )
import           Data.Semigroup                 ( Semigroup((<>)) )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.String                    ( String )
import           Data.Text                      ( Text, isInfixOf )
import qualified Data.Text                     as Text
import           GHC.Generics                   ( Generic )
import           Prelude                        ( Applicative((*>), (<*), pure)
                                                , Eq((/=))
                                                , Foldable(null)
                                                , Monoid(mconcat)
                                                , error
                                                )
import           Text.Parsec                    ( ParseError
                                                , Parsec
                                                , anyChar
                                                , char
                                                , evalParser
                                                , getInput
                                                , getState
                                                , letter
                                                , lookAhead
                                                , many
                                                , many1
                                                , manyTill

                                                , notFollowedBy
                                                , oneOf

                                                , runParser
                                                , satisfy
                                                , sepBy1
                                                , setState
                                                , space
                                                , string
                                                , try, optionMaybe
                                                )
import           Text.Parsec.Error              ( Message(Message) )
import           Text.ParserCombinators.Parsec.Error
                                                ( newErrorMessage )
import           Text.ParserCombinators.Parsec.Pos
                                                ( initialPos )
import           Text.Show                      ( Show(show) )
import           TextShow                       ( TextShow(..)
                                                , fromText
                                                )
import           TextShow.Generic               ( genericShowbPrec )
import Palantype.Tools.Hyphenate (Hyphenated (..))

data Result
  = Success Hyphenated
  | Failure ParseError
  | Exception Exception
  deriving stock (Generic)

instance Eq Result where
    Success h1 == Success h2 = h1 == h2
    _          == _          = False

instance TextShow Result where
    showb = \case
        Success   hyph -> fromText "Success: " <> showb hyph
        Failure   err  -> fromText $ "Failure: " <> Text.pack (show err)
        Exception exc  -> fromText "Exception: " <> showb exc

data Exception
  = ExceptionAbbreviation
  | ExceptionMultiple
  | ExceptionSpecialChar Char
  | ExceptionSingleLetter
  | ExceptionEllipsis
  | ExceptionAcronym
  | ExceptionExplicit
  | ExceptionMisspelling
  deriving stock (Generic)

instance TextShow Exception where
    showbPrec = genericShowbPrec

-- | these cause problems later on when detecting duplicates
--   w/o adding too much value
--   thus they are explicitly excluded
setExplicitExceptions :: Set Text
setExplicitExceptions = Set.fromList ["AStA", "IGeL", "kN"]

-- | turn "zusammenhang[s]los >>> zu|sam|men|hang[s]|los ..." into
--   [ Success (SyllableData "zusammenhanglos" ["zu", "sam", "men", "hang", "los"])
--   , Success (SyllableData "zusammenhangslos" ["zu", "sam", "men", "hangs", "los"])
--   ]
parseEntry :: Text -> [Result]
parseEntry str | "falsche Schreibung für" `isInfixOf` str =
    [Exception ExceptionMisspelling]
parseEntry str | "alte Schreibung für" `isInfixOf` str =
    [Exception ExceptionMisspelling]
parseEntry txt = parseEntry' <$> parseOptionalChars txt
  where
    parseEntry' :: Text -> Result
    parseEntry' str = case evalParser word Nothing "" str of
        Left  err           -> Failure err
        Right (Just exc, _) -> Exception exc
        Right (Nothing, (w, rem)) ->
            let st = if Text.null w
                    then error $ Text.unpack $ "Empty state: " <> str
                    else Text.unpack w
            in
                case evalParser syllables st "" rem of
                    Right ("", ls) -> Success $ Hyphenated ls
                    Right (st', _) ->
                        let
                            msg =
                                Message
                                    $ "Failed to parse syllalbes; remaining state: "
                                    <> st'
                            err = newErrorMessage msg $ initialPos ""
                        in
                            Failure err
                    Left err -> Failure err

    -- | parse "zusammenhangslos >>> zu|sam|men|hangs|los ..." into
    --   ("zusammenhangslos", "zu|sam|men|hangs|los ...")
    word :: Parsec Text (Maybe Exception) (Text, Text)
    word = do
        isSuffix <- isJust <$> optionMaybe (try $ string "...")
        when isSuffix $ setState $ Just ExceptionEllipsis
        void $ many $ try (satisfy (not . isLetter) *> notFollowedBy sep)
        result <- Text.pack . catMaybes <$> manyTill someChar (try sep)
        when (Text.length result == 1) $ setState $ Just ExceptionSingleLetter
        when (Text.toUpper result == result) $ setState $ Just ExceptionAcronym
        when (result `Set.member` setExplicitExceptions) $ setState $ Just
            ExceptionExplicit
        rem <- getInput
        pure (result, rem)
      where
        sep      = void $ many1 space *> string ">>>" *> many1 space

    someChar :: Parsec Text (Maybe Exception) (Maybe Char)
    someChar = do
        c    <- anyChar
        mExc <- getState
        when (isNothing mExc) $ case c of
            '.' -> do
                isEllipsis <- isJust <$> optionMaybe (try $ string "..")
                setState $ if isEllipsis
                    then Just ExceptionEllipsis
                    else Just ExceptionAbbreviation
            ' ' -> setState $ Just ExceptionMultiple
            _ | not (isLetter c || c `elem` ("-!®" :: String)) ->
                setState $ Just $ ExceptionSpecialChar c
            _ -> pure ()
        pure $ case c of
            '!' -> Nothing
            '®' -> Nothing
            _   -> Just c

    -- | parse "zu|sam|men|hangs|los" into ["zu", "sam", "men", "hangs", "los"]
    syllables :: Parsec Text String [Text]
    syllables = do
        (x : _) <- getState
        void $ many $ satisfy (/= x)
        mconcat <$> sepBy1 ((++) <$> optionalHyphen <*> syllable)
                           (try (char '|') <|> lookAhead (next '-'))

      where
        syllable       = try bmio <|> try pseudoSyllable <|> realSyllable

        pseudoSyllable = do
            v1 <- vowel
            (v1 :) <$> (try bmio <|> pseudoSyllable')
          where
            pseudoSyllable' = do
                c1  <- Text.pack <$> many1 lcConsonantWOY
                v2  <- vowel
                rem <- Text.pack <$> many nextChar
                pure [c1 <> v2 <> rem]

        bmio = do
            bm <- try (next 'b') <|> next 'm'
            i  <- next 'i'
            o  <- next 'o'
            pure [Text.pack [bm, i], Text.singleton o]

        realSyllable = try hendl <|> (pure . Text.pack <$> many1 nextChar)
          where
            hendl :: Parsec Text String [Text]
            hendl = do

                let syllableEnd =
                        void (oneOf "|-")
                            <|> (getState >>= bool mzero (pure ()) . null)

                    consonantL = do
                        c <- Text.singleton <$> consonant
                        l <- Text.singleton <$> next 'l'
                        lookAhead syllableEnd
                        pure (c, l)

                chrs <-
                    Text.pack
                        <$> many (try $ nextChar <* notFollowedBy consonantL)
                c1      <- Text.singleton <$> nextChar
                (c2, l) <- consonantL
                pure [chrs <> c1 <> c2, l]

        optionalHyphen = try (pure . Text.singleton <$> next '-') <|> pure []

        nextChar :: Parsec Text String Char
        nextChar = getState >>= \case
            (x : xs) | x /= '-' -> do
                setState xs
                char x
            _ -> mzero

        next :: Char -> Parsec Text String Char
        next c = getState >>= \case
            (x : xs) | x == c -> do
                setState xs
                char x
            _ -> mzero

        vowel = getState >>= \case
            (x : xs) | x `elem` ('y' : 'Y' : vowels) -> do
                setState xs
                Text.singleton <$> char x
            _ -> mzero

        lcConsonantWOY = getState >>= \case
            (x : xs) | isLower x && x `notElem` ('y' : 'Y' : '-' : vowels) -> do
                setState xs
                char x
            _ -> mzero

        consonant = getState >>= \case
            (x : xs) | x `notElem` ('-' : vowels) -> do
                setState xs
                char x
            _ -> mzero

vowels :: String
vowels = "AEIOUÄÖÜÁÀÂÅÉÈÊÍÓÔÚaeiouäöüáàâåéèêëíóôøû"

parseOptionalChars :: Text -> [Text]
parseOptionalChars str = case runParser optionalChars () "" str of
    Left _ -> [str]
    Right (cs, cts) ->
        [ replaceFirst ("[" <> cts <> "]") ""
            $ replaceFirst ("[" <> cs <> "]") "" str
        , replaceFirst ("[" <> cts <> "]") cts
            $ replaceFirst ("[" <> cs <> "]") cs str
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
    | Text.null back = haystack
    |    -- pattern doesn't occur
      otherwise = Text.concat
        [front, replacement, Text.drop (Text.length needle) back]
    where (front, back) = Text.breakOn needle haystack
