{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Palantype.Tools.Steno where

import           Control.Applicative            ( Applicative
                                                    ( (*>)
                                                    , (<*)
                                                    , (<*>)
                                                    , pure
                                                    )
                                                )
import           Control.Category               ( (<<<)
                                                , Category((.))
                                                )
import           Control.Lens                   ( view )
import           Control.Lens.Tuple             ( _2 )
import           Data.Bifunctor                 ( Bifunctor(first, second) )
import           Data.Bool                      ( Bool(False, True)
                                                , not, otherwise
                                                )
import           Data.ByteString                ( ByteString
                                                , isInfixOf
                                                )
import qualified Data.ByteString               as BS
import           Data.Char                      ( isUpper, isLower
                                                )
import           Data.Either                    ( Either(Left, Right)

                                                )
import           Data.Eq                        ( Eq((/=), (==)) )
import           Data.Foldable                  ( Foldable
                                                    ( length
                                                    , sum

                                                    )
                                                , any
                                                , maximumBy
                                                )
import           Data.Function                  ( ($)
                                                , const
                                                )
import           Data.Functor                   ( (<$>)
                                                , (<&>)
                                                , Functor(fmap)
                                                )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Int                       ( Int )
import           Data.List                      ( (++)
                                                , filter

                                                , intersperse
                                                , sortOn
                                                )
import           Data.Maybe                     ( Maybe(Just, Nothing)
                                                , catMaybes
                                                )
import           Data.Monoid                    ( (<>)

                                                , mconcat
                                                )
import           Data.Ord                       ( Down(Down)
                                                , Ord((<=))
                                                , comparing
                                                )
import           Data.Ratio                     ( Rational )
import           Data.Text                      ( Text
                                                , replace
                                                , toLower
                                                )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Data.Traversable               ( Traversable(sequence) )
import           Data.Tuple                     ( uncurry )
import           Data.Word                      ( Word8 )
import           GHC.Err                        ( error )
import           GHC.Float                      ( Double )
import           GHC.Num                        ( (+)
                                                , Num(negate)
                                                )
import           GHC.Real                       ( Fractional((/), fromRational)
                                                , fromIntegral
                                                )
import           Palantype.Common               ( Finger
                                                , Palantype
                                                )
import           Palantype.Common.Dictionary    ( kiCapNext, kiAcronym )
import qualified Palantype.Common.Indices      as KI
import           Palantype.Common.RawSteno      ( RawSteno(RawSteno, unRawSteno)
                                                )
import qualified Palantype.Common.RawSteno     as Raw
import qualified Text.Parsec                    as Parsec
import           Text.Parsec                    ( Parsec
                                                , char
                                                , eof
                                                , evalParser
                                                , sepBy1
                                                , setState, many1, many, runParser, getInput
                                                )
import           Text.Parsec.Pos                ( initialPos )
import qualified Text.ParserCombinators.Parsec.Error
                                               as Parsec
import           Text.Printf                    ( printf )
import           Text.Show                      ( Show(show) )
import           TextShow                       ( TextShow(showb, showt)
                                                , fromString
                                                , fromText
                                                )
import Palantype.DE (Greediness, mapExceptions, triePrimitives, lsPatterns)
import qualified Data.Trie as Trie

data Score = Score
    { scorePrimary   :: Rational
    , scoreSecondary :: Int
    }
    deriving stock (Eq, Ord)

instance TextShow Score where
    showb Score {..} =
        fromString (printf "%.1f" $ fromRational @Double scorePrimary)
            <> fromText "("
            <> showb scoreSecondary
            <> fromText ")"

instance Show Score where
    show = Text.unpack . showt

data ParseError
  = PEParsec RawSteno Parsec.ParseError
  | PEExceptionTable Text
  deriving stock (Show)

instance TextShow ParseError where
    showb = fromString . show

isCapitalized :: Text -> Bool
isCapitalized "" = error "isCapitalized: empty string"
isCapitalized str = isUpper $ Text.head str

{-|
Add the "capitalize next word" chord in front
-}
addCapChord :: forall key . Palantype key => State key -> State key
addCapChord st@State {..} = st
    { stStrRawSteno = showt (KI.toRaw @key kiCapNext) <> "/" <> stStrRawSteno
    , stNChords     = stNChords + 1
    }

addAcronymChord :: forall key. Palantype key => State key -> State key
addAcronymChord st@State {..} = st
    { stStrRawSteno = showt (KI.toRaw @key kiAcronym) <> "/" <> stStrRawSteno
    , stNChords = stNChords + 1
    }

-- | Find steno chords for a given, hyphenated word, e.g. "Ge|sund|heit"
--   or return a parser error.
--   In case of success, provide the most efficient steno chords along with a
--   list of alternatives. E.g. steno that uses more chords or more letters,
--   or steno that uses the same number of chords or letters, but requires
--   higher greediness
parseSeries
    :: forall key . Palantype key => Text -> Either ParseError [RawSteno]
parseSeries hyphenated = case HashMap.lookup unhyphenated mapExceptions of
    Just raws -> sequence $ checkException <$> raws
    Nothing   -> makeSteno
  where
    makeSteno =
        let
            -- calculate result for lower case word

            eAcronym = runParser ((,) <$> acronym <*> getInput) () "" hyphenated
            (hyphenated', isAcronym) = case eAcronym of
                Right (syls, rem) -> (Text.intercalate "|" syls <> rem, True)
                Left _ -> (hyphenated, False)

            str = Text.encodeUtf8 $ toLower hyphenated'

            st                = State { stStrRawSteno = ""
                                      , stNLetters    = 0
                                      , stNChords     = 1
                                      , stMFinger     = Nothing
                                      , stMLastKey    = Nothing
                                      }

            levels =
                catMaybes
                    $   lsPatterns
                    <&> \(g, patterns) -> if any (`isInfixOf` str) patterns
                            then Just g
                            else Nothing

            lsResultLc =
                levels <&> \maxG ->
                    ((maxG, False), ) $ optimizeStenoSeries maxG st str

            lsResult
              | isAcronym = second (mapSuccess $ addAcronymChord @key) <$> lsResultLc
              | isCapitalized hyphenated =
                 let
                     lsNoCOpt =
                         second (mapSuccess $ addCapChord @key)
                             <$> lsResultLc
                     lsYesCOpt = first (second $ const True) <$> lsResultLc
                 in
                     lsNoCOpt ++ lsYesCOpt
              | otherwise = lsResultLc
        in
            case sortOn (Down . uncurry scoreWithG) lsResult of
                (_, Failure raw err) : _ -> Left $ PEParsec raw err
                []                       -> error "impossible"
                ls                       -> Right $ filterAlts ls

    checkException raw = case Raw.parseWord @key raw of
        Right chords -> Right $ Raw.unparts $ Raw.fromChord <$> chords
        Left err ->
            Left
                $  PEExceptionTable
                $  unhyphenated
                <> ": "
                <> unRawSteno raw
                <> "; "
                <> Text.pack (show err)


    unhyphenated = replace "|" "" hyphenated

    filterAlts []                      = []
    filterAlts ((_, Failure _ _) : as) = filterAlts as
    filterAlts ((_, Success state) : as) =
        let rawSteno = stStrRawSteno state
            distinct (_, Failure _ _) = False
            distinct (_, Success st2) = rawSteno /= stStrRawSteno st2
        in  RawSteno rawSteno : filterAlts (filter distinct as)

-- | Scoring "with greediness"
--   This scoring serves to sort the result, no result is discarded
--   Given the efficiency of a steno code, lower greediness is preferred
scoreWithG
    :: forall k
     . (Greediness, Bool)
    -> Result (State k)
    -> (Rational, Greediness, Bool, Int)
scoreWithG (g, cOpt) result =
    let Score {..} = score result
    in  (scorePrimary, negate g, not cOpt, scoreSecondary)

newtype CountLetters = CountLetters { unCountLetters :: Int }
  deriving newtype (Num, Eq, TextShow)

countLetters :: ByteString -> CountLetters
countLetters str = CountLetters $ sum $ BS.length <$> BS.split bsPipe str

newtype CountChords = CountChords { unCountChords :: Int }
  deriving newtype (Num, TextShow)

countChords :: Text -> CountChords
countChords = CountChords <<< length <<< Text.splitOn "/"

-- -- | Optimize steno series
--
-- -- | a series is a list of steno keys, interspersed with slashes ('/') to mark
-- --   a new chord
-- data KeysOrSlash key
--   = KoSKeys Text [key]
--   | KoSSlash

countKeys :: Text -> Int
countKeys = Text.length <<< Text.replace "/" ""

-- {-|
-- Convert the list [KeysOrSlash] from the optimization algo into a list
-- of word parts. The list [KeysOrSlash] is in reverse order,
-- the keys between slashes are grouped along with the original text that
-- they encode.
-- -}
-- toParts :: forall key . Palantype key => [KeysOrSlash key] -> [(Text, Chord key)]
-- toParts ls =
--     let (lsParts, (str, keys)) = foldl' acc ([], ("", [])) ls
--     in  (str, mkChord keys) : lsParts
--   where
--     acc
--         :: ([(Text, Chord key)], (Text, [key]))
--         -> KeysOrSlash key
--         -> ([(Text, Chord key)], (Text, [key]))
--     acc (chords, (str, keys)) = \case
--         (KoSKeys s ks) -> (chords, (s <> str, ks ++ keys))
--         KoSSlash       -> ((str, mkChord keys) : chords, ("", []))

data Result a
  = Success a
  | Failure RawSteno Parsec.ParseError

instance TextShow a => TextShow (Result a) where
    showb (Success x  ) = fromText "Success " <> showb x
    -- showb (Failure rs err) = fromText "Failure " <> showb rs <> fromText (" " <> Text.pack (show err))
    showb (Failure r _) = fromText "Failure " <> showb r <> fromText " ..."

mapSuccess :: (a -> a) -> Result a -> Result a
mapSuccess _ r@(Failure _ _) = r
mapSuccess f (  Success x  ) = Success $ f x

data State key = State
    -- { stPartsSteno :: [KeysOrSlash key]
    { stStrRawSteno :: Text
    , stNLetters    :: CountLetters
    , stNChords     :: CountChords
    , stMFinger     :: Maybe Finger
    -- | for compatibility with original palantype that relies on key order
    --   rather than on finger, because several keys per finger are allowed
    , stMLastKey    :: Maybe key
    }

instance Palantype key => TextShow (State key) where
    showb State {..} = showb stStrRawSteno


bsPipe :: Word8
bsPipe = 0x7C

-- | The score has two values, a primary score and a secondary score.
--
--   The primary score is the number of letters per chord.
--   A high number of letters per chords means high typing efficiency.
--
--   The secondary score is the number of steno keys.
--   A lower number is preferred.
--   The secondary score is used, when two series achieve the same primary score.
score :: forall k . Result (State k) -> Score
score (Success st ) = score' st
score (Failure _ _) = Score 0 0

score' :: forall k . State k -> Score
score' State {..} =
    let scorePrimary = fromIntegral (unCountLetters stNLetters)
            / fromIntegral (unCountChords stNChords)
        scoreSecondary = negate $ countKeys stStrRawSteno
    in  Score { .. }

-- | Try to fit as many letters as possible into a steno
--   chord (a chord contains keys that can be typed all at once).
--   The score of a chord is the number of letters it successfully
--   encoded, w/o the hyphenation symbol.
--
--   look at next character:
--     '|' -> do
--       consume character
--       (steno1, score1) = append '/', recursion with increased chord count and
--       (steno2, score2) = recursion
--       return steno with highest score
--     otherwise -> get matches from primtive trie
--       for every match:
--         consume and recursion with remaining string ...
--         ... increase letter count by match length
--       return steno with highest score
optimizeStenoSeries
    :: forall key
     . Palantype key
    => Greediness
    -> State key
    -> ByteString
    -> Result (State key)
optimizeStenoSeries _ st "" = Success st
optimizeStenoSeries g st str | BS.head str == bsPipe =
    let newState = State { stStrRawSteno = stStrRawSteno st <> "/"
                         , stNLetters    = stNLetters st
                         , stNChords     = stNChords st + 1
                         , stMFinger     = Nothing
                         , stMLastKey    = Nothing
                         }
        str' = BS.tail str
        r1   = optimizeStenoSeries g newState str'
        r2   = optimizeStenoSeries g st str'
    in  maximumBy (comparing score) [r1, r2]
optimizeStenoSeries g st str =
    let matches = filterGreediness $ flatten $ Trie.matches triePrimitives str

        matchToResult (consumed, result, rem) =
            case parseKey result (stMFinger st) (stMLastKey st) of
                Left err -> Failure (view _2 result) err
                Right ((mFinger, mLK), strRaw) ->
                    let newState = State
                            { stStrRawSteno = stStrRawSteno st <> strRaw
                            , stNLetters = stNLetters st + countLetters consumed
                            , stNChords     = stNChords st + countChords strRaw
                            , stMFinger     = mFinger
                            , stMLastKey    = mLK
                            }
                    in  optimizeStenoSeries g newState rem

        results = case matches of
            [] -> [Failure "" $ Parsec.newErrorUnknown (initialPos "")]
            ms -> matchToResult <$> ms
    in  maximumBy (comparing score) results
  where
    filterGreediness
        :: [(ByteString, (Greediness, RawSteno), ByteString)]
        -> [(ByteString, (Greediness, RawSteno), ByteString)]
    filterGreediness = filter (\(_, (g', _), _) -> g' <= g)

    flatten
        :: [(ByteString, [(Greediness, RawSteno)], ByteString)]
        -> [(ByteString, (Greediness, RawSteno), ByteString)]
    flatten = mconcat . fmap expand
      where
        expand
            :: (ByteString, [(Greediness, RawSteno)], ByteString)
            -> [(ByteString, (Greediness, RawSteno), ByteString)]
        expand (c, rs, rem) = (c, , rem) <$> rs

    parseKey
        :: (Greediness, RawSteno)
        -> Maybe Finger
        -> Maybe key
        -> Either Parsec.ParseError ((Maybe Finger, Maybe key), Text)
    parseKey (_, RawSteno s) mFinger mLastKey =
        let
            ePair = evalParser keysWithSlash (mFinger, mLastKey) "" s
            makeRawStr =
                mconcat <<< intersperse "/" <<< fmap (mconcat <<< fmap showt)
        in
            second makeRawStr <$> ePair

    keysWithSlash :: Parsec Text (Maybe Finger, Maybe key) [[key]]
    keysWithSlash =
        sepBy1 Raw.keys (char '/' *> setState (Nothing, Nothing)) <* eof

acronym :: Parsec Text () [Text]
acronym = do
    s1 <- acronymSyllable
    -- an acronym requires two "acronym syllables" at minimum
    -- i.e. it must start with an uppercase letter and it must contain
    -- at least one additional uppercase letter later
    ss <- many1 acronymSyllable
    pure $ s1 : ss
  where

    -- an "acronym syllable" is an uppercase letter, optionally
    -- followed by lowercase letters
    acronymSyllable = do
      ucl <- Parsec.satisfy isUpper
      lcls <- many $ Parsec.satisfy isLower
      pure $ Text.pack $ ucl : lcls
