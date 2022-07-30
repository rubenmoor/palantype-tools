{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module Palantype.Tools.Steno where

import           Control.Applicative            ( Applicative
                                                    ( (*>)
                                                    , (<*)
                                                    , (<*>)
                                                    , pure
                                                    ), optional
                                                )
import           Control.Category               ( (<<<)
                                                , Category((.))
                                                )
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
                                                    , sum, foldr, foldl'

                                                    )
                                                , any
                                                , maximumBy
                                                )
import           Data.Function                  ( ($)

                                                )
import           Data.Functor                   ( (<$>)
                                                , (<&>)
                                                , Functor(fmap)
                                                )
import           Data.Int                       ( Int )
import           Data.List                      ( filter

                                                , intersperse
                                                , sortOn
                                                )
import           Data.Monoid                    ( (<>)

                                                , mconcat
                                                )
import           Data.Ord                       ( Down(Down)
                                                , Ord((<=), max)
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
import           Data.Tuple                     ( uncurry, snd )
import           Data.Word                      ( Word8 )
import           GHC.Err                        ( error )
import           GHC.Float                      ( Double )
import           GHC.Num                        ( (+)
                                                , Num(negate)
                                                )
import           GHC.Real                       ( Fractional((/), fromRational)
                                                , fromIntegral
                                                )
import Palantype.Common
    ( Finger,
      Greediness,
      Palantype(PatternGroup, patSimpleMulti, patAcronym),
      lsPatterns,
      parseWord,
      mapExceptions, kiAcronym, fromChord, unparts, keys, Chord (Chord) )
import qualified Palantype.Common.Indices      as KI
import qualified Text.Parsec                    as Parsec
import           Text.Parsec                    ( Parsec
                                                , char
                                                , eof

                                                , sepBy1
                                                , setState, many1, many, runParser, getInput, getState
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
import qualified Data.Trie as Trie
import qualified Data.Map.Strict as Map
import Data.Maybe (Maybe(..), catMaybes)
import Data.Trie (Trie)
import Palantype.Common.RawSteno.Type (RawSteno(RawSteno))
import qualified Palantype.Common.RawSteno as Raw

data Score = Score
    { -- first criterion: make use of the maximum allowed greediness
      scoreGreediness :: Int

      -- second criterion: maximize number of real-language letters per chord
    , scoreEfficiency   :: Rational

      -- third criterion: minimize number of steno letters
    , scoreBrevity :: Int
    }
    deriving stock (Eq, Ord)

instance TextShow Score where
    showb Score {..} =
        showb scoreGreediness
            <> fromText "-"
            <> fromString (printf "%.1f" $ fromRational @Double scoreEfficiency)
            <> fromText "("
            <> showb scoreBrevity
            <> fromText ")"

instance Show Score where
    show = Text.unpack . showt

data ParseError
  = PEParsec RawSteno Parsec.ParseError
  | PEExceptionTable Text
  | PEImpossible Text
  deriving stock (Show)

instance TextShow ParseError where
    showb = fromString . show

isCapitalized :: Text -> Bool
isCapitalized "" = error "isCapitalized: empty string"
isCapitalized str = isUpper $ Text.head str

addAcronymChord :: forall key . Palantype key => State key -> State key
addAcronymChord st@State {..} = st
    { stProtoSteno = ProtoChord (KI.toKeys kiAcronym) : ProtoSlash : stProtoSteno
    , stNChords = stNChords + 1
    , stMPatternGroup = max stMPatternGroup $ Just (0, patAcronym)
    }

data Verbosity
  = VSilent
  | VDebug
  deriving stock (Eq, Ord)

-- | Find steno chords for a given, hyphenated word, e.g. "Ge|sund|heit"
--   or return a parser error.
--   In case of success, provide the most efficient steno chords along with a
--   list of alternatives. E.g. steno that uses more chords or more letters,
--   or steno that uses the same number of chords or letters, but requires
--   higher greediness
parseSeries
    :: forall key
    . Palantype key
    => Trie [(Greediness, RawSteno, PatternGroup key)]
    -> Text
    -> Either ParseError [(RawSteno, (Greediness, PatternGroup key))]
parseSeries trie hyphenated =
  case Map.lookup unhyphenated (mapExceptions @key) of
    Just raws -> sequence
                   ( checkException . second (0, ) <$> raws
                   )
    Nothing   ->
        let
            -- calculate result for lower case word

            eAcronym = runParser ((,) <$> acronym <*> getInput) () "" hyphenated
            (hyphenated', isAcronym) = case eAcronym of
                Right (syls, rem) -> (Text.intercalate "|" syls <> rem, True)
                Left _ -> (hyphenated, False)

            str = Text.encodeUtf8 $ toLower hyphenated'

            st = State
              { stProtoSteno    = []
              , stNLetters      = 0
              , stNChords       = 1
              , stMFinger       = Nothing
              , stMLastKey      = Nothing
              , stMPatternGroup = Nothing
              , stMaxGreediness = 0
              }

            levels =
                catMaybes
                    $   lsPatterns @key
                    <&> \(g, patterns) -> if any (`isInfixOf` str) patterns
                            then Just g
                            else Nothing

            lsResultLc =
                levels <&> \maxG ->
                    ((maxG, False), ) $ optimizeStenoSeries trie maxG st str

            lsResult
              | isAcronym =
                  second (mapSuccess $ addAcronymChord @key) <$> lsResultLc
              | otherwise = lsResultLc

        in
            case sortOn (Down . uncurry scoreWithG) lsResult of
                (_, Failure raw err) : _ -> Left $ PEParsec raw err
                []                       -> Left $ PEImpossible $ "Empty list for: " <> hyphenated
                ls                       -> Right $ filterAlts $ snd <$> ls

  where
    checkException (raw, patG) = case parseWord @key raw of
        Right chords -> Right (unparts (fromChord <$> chords), patG)
        Left err -> Left $ PEExceptionTable
                $  unhyphenated
                <> ": "
                <> showt raw
                <> "; "
                <> Text.pack (show err)

    unhyphenated = replace "|" "" hyphenated

    filterAlts
      :: [Result (State key)]
      -> [(RawSteno, (Greediness, PatternGroup key))]
    filterAlts []                   = []
    filterAlts (Failure _ _   : as) = filterAlts as
    filterAlts (Success state : as) =
        let rawSteno = protoToSteno $ stProtoSteno state
            pg = case stMPatternGroup state of
              Just patG -> patG
              Nothing -> error "impossible: pattern group Nothing"
            distinct (Failure _ _) = False
            distinct (Success st2) = rawSteno /= protoToSteno (stProtoSteno st2)
            as'= filter distinct as
        in  (rawSteno, pg) : filterAlts as'

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
    in  (scoreEfficiency, negate g, not cOpt, scoreBrevity)

newtype CountLetters = CountLetters { unCountLetters :: Int }
  deriving newtype (Num, Eq, TextShow)

countLetters :: Text -> CountLetters
countLetters str =
    CountLetters $ sum $ Text.length <$> Text.splitOn "|" str

newtype CountChords = CountChords { unCountChords :: Int }
  deriving newtype (Num, TextShow)

countChords :: [ProtoSteno k] -> CountChords
countChords = CountChords <<< foldl' (\s p -> s + count p) 0
  where
    count ProtoSlash = 1
    count (ProtoChord _) = 0

-- -- | Optimize steno series
--
-- -- | a series is a list of steno keys, interspersed with slashes ('/') to mark
-- --   a new chord
-- data KeysOrSlash key
--   = KoSKeys Text [key]
--   | KoSSlash

countKeys :: [ProtoSteno k] -> Int
countKeys = foldl' (\s p -> s + count p) 0
  where
    count ProtoSlash = 0
    count (ProtoChord ks) = length ks

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
  = Success !a
  | Failure RawSteno Parsec.ParseError

instance TextShow a => TextShow (Result a) where
    showb (Success x  ) = fromText "Success " <> showb x
    -- showb (Failure rs err) = fromText "Failure " <> showb rs <> fromText (" " <> Text.pack (show err))
    showb (Failure r _) = fromText "Failure " <> showb r <> fromText " ..."

mapSuccess :: (a -> a) -> Result a -> Result a
mapSuccess _ r@(Failure _ _) = r
mapSuccess f (  Success x  ) = Success $ f x

data ProtoSteno k
    = ProtoSlash
    | ProtoChord [k]
    deriving stock Eq

groupProto :: [ProtoSteno k] -> [Chord k]
groupProto ls =
    let (cs, rem) = foldr acc ([], []) ls
    in  Chord rem : cs
  where
    acc :: ProtoSteno k -> ([Chord k], [k]) -> ([Chord k], [k])
    acc ProtoSlash      (cs, current) = (Chord current : cs, []           )
    acc (ProtoChord ks) (cs, current) = (cs                , ks <> current)

protoToSteno :: Palantype k => [ProtoSteno k] -> RawSteno
protoToSteno ls =
    mconcat $ intersperse (Raw.fromText "/") $ Raw.fromChord <$> groupProto ls

data State key = State
  { stProtoSteno  :: [ProtoSteno key]
  , stNLetters    :: CountLetters
  , stNChords     :: CountChords
  , stMFinger     :: Maybe Finger
  -- | for compatibility with original palantype that relies on key order
  --   rather than on finger, because several keys per finger are allowed
  , stMLastKey    :: Maybe key
  , stMPatternGroup :: Maybe (Greediness, PatternGroup key)
  , stMaxGreediness :: Greediness
  }

instance Palantype key => TextShow (State key) where
    showb State {..} = showb $ protoToSteno stProtoSteno

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
score (Failure _ _) = Score 0 0 0

score' :: forall k . State k -> Score
score' State {..} =
    -- let scoreGreediness = 0
    let scoreGreediness = stMaxGreediness
        scoreEfficiency = fromIntegral (unCountLetters stNLetters)
            / fromIntegral (unCountChords stNChords)
        scoreBrevity = negate $ countKeys stProtoSteno
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
    => Trie [(Greediness, RawSteno, PatternGroup key)]
    -> Greediness
    -> State key
    -> ByteString
    -> Result (State key)
optimizeStenoSeries _ _ st "" = Success st
optimizeStenoSeries trie g st str | BS.head str == bsPipe =
    let newState =
          st { stProtoSteno  = stProtoSteno st <> [ProtoSlash]
             , stNChords     = stNChords st + 1
             , stMFinger     = Nothing
             , stMLastKey    = Nothing
             , stMPatternGroup =
                 max (Just (0, patSimpleMulti)) $ stMPatternGroup st
             }
        str' = BS.tail str
        r1   = optimizeStenoSeries trie g newState str'
        r2   = optimizeStenoSeries trie g st str'
    in  maximumBy (comparing score) [r1, r2]
optimizeStenoSeries trie g st str =
    let
        matches = filterGreediness $ flatten $ Trie.matches trie str

        matchToResult (consumed, (greediness, raw, pg), rem) =
            case parseKey (greediness, raw)
                          (stMFinger st)
                          (stMLastKey st) of
                Left err -> Failure raw err
                Right (proto, (mFinger, mLK)) ->
                    let newState = State
                            { stProtoSteno = stProtoSteno st <> proto
                            , stNLetters    =
                                stNLetters st + countLetters (Text.decodeUtf8 consumed)
                            , stNChords     = stNChords st + countChords proto
                            , stMFinger     = mFinger
                            , stMLastKey    = mLK
                            , stMPatternGroup =
                                max (Just (greediness, pg)) $ stMPatternGroup st
                            , stMaxGreediness = max greediness $ stMaxGreediness st
                            }
                    in  optimizeStenoSeries trie g newState rem

        results = case matches of
            [] -> [Failure "" $ Parsec.newErrorUnknown (initialPos "")]
            ms -> matchToResult <$> ms
    in  maximumBy (comparing score) results
  where
    filterGreediness
        :: [(ByteString, (Greediness, RawSteno, PatternGroup key), ByteString)]
        -> [(ByteString, (Greediness, RawSteno, PatternGroup key), ByteString)]
    filterGreediness = filter (\(_, (g', _, _), _) -> g' <= g)

    flatten
        :: [(ByteString, [(Greediness, RawSteno, PatternGroup key)], ByteString)]
        -> [(ByteString, (Greediness, RawSteno, PatternGroup key), ByteString)]
    flatten = mconcat . fmap expand
      where
        expand
            :: (ByteString, [(Greediness, RawSteno, PatternGroup key)], ByteString)
            -> [(ByteString, (Greediness, RawSteno, PatternGroup key), ByteString)]
        expand (c, rs, rem) = (c, , rem) <$> rs

    parseKey
        :: (Greediness, RawSteno)
        -> Maybe Finger
        -> Maybe key
        -> Either Parsec.ParseError
                  ([ProtoSteno key], (Maybe Finger, Maybe key))
    parseKey (_, RawSteno s) mFinger' mLastKey' =
        let
            mFinger = mFinger'
            mLastKey = mLastKey'
            ePair = runParser ((,) <$> keysWithSlash <*> getState)
                              (mFinger, mLastKey) "" s
            -- makeRawStr = mconcat <<< intersperse "/" <<< fmap (mconcat <<< fmap showt)
        in
            first (intersperse ProtoSlash <<< fmap ProtoChord) <$> ePair

    keysWithSlash :: Parsec Text (Maybe Finger, Maybe key) [[key]]
    keysWithSlash =
        sepBy1 keys (char '/' *> setState (Nothing, Nothing)) <* eof

{-|
an acronym requires two "acronym syllables" at minimum
i.e. it must start with an uppercase letter and it must contain
at least one additional uppercase letter later
-}
acronym :: Parsec Text () [Text]
acronym = do
    s1 <- acronymSyllable
    ss <- many1 acronymSyllable
    pure $ s1 : ss
  where
    -- | an "acronym syllable" is an uppercase letter, optionally
    --   followed by lowercase letters
    acronymSyllable = do
      ucl <- Parsec.satisfy isUpper
      lcls <- many $ Parsec.satisfy isLower
      _ <- optional $ char '|'
      pure $ Text.pack $ ucl : lcls
