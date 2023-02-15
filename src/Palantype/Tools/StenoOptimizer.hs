{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StandaloneDeriving #-}

module Palantype.Tools.StenoOptimizer where

import           Control.Applicative            ( Applicative
                                                    ( (*>)
                                                    , (<*)
                                                    , (<*>)
                                                    , pure
                                                    )
                                                , optional
                                                )
import           Control.Category               ( (<<<)
                                                , Category((.))
                                                )
import           Data.Bifunctor                 ( Bifunctor(first) )
import           Data.Bool                      ( Bool(False, True) , otherwise
                                                )
import           Data.ByteString                ( ByteString
                                                , isInfixOf
                                                )
import qualified Data.ByteString               as BS
import           Data.Char                      ( isLower
                                                , isUpper
                                                )
import           Data.Either                    ( Either(Left, Right) )
import           Data.Eq                        ( Eq((/=), (==)) )
import           Data.Foldable                  ( Foldable
                                                    ( foldl'
                                                    , foldr
                                                    , length
                                                    , sum
                                                    )
                                                , any
                                                , maximumBy
                                                )
import           Data.Function                  ( ($), on )
import           Data.Functor                   ( (<$>)
                                                , (<&>)
                                                , Functor(fmap)
                                                )
import           Data.Int                       ( Int )
import           Data.List                      ( filter
                                                , intersperse
                                                , sortOn, zip
                                                )
import           Data.Maybe                     ( Maybe(..)
                                                , catMaybes
                                                )
import           Data.Monoid                    ( (<>)
                                                , mconcat
                                                )
import           Data.Ord                       ( Down(Down)
                                                , Ord((<=), max, compare)
                                                , comparing
                                                )
import           Data.Ratio                     ( Rational )
import           Data.Text                      ( Text
                                                , toLower
                                                )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import qualified Data.Trie                     as Trie
import           Data.Trie                      ( Trie )
import           Data.Word                      ( Word8 )
import           GHC.Err                        ( error )
import           GHC.Float                      ( Double )
import           GHC.Num                        ( (+)
                                                , Num(negate)
                                                )
import           GHC.Real                       ( Fractional((/), fromRational)
                                                , fromIntegral
                                                )
import           Palantype.Common               ( Chord(Chord)
                                                , Finger
                                                , Greediness
                                                , Palantype
                                                    ( PatternGroup
                                                    , patAcronym
                                                    , patZero, patCapitalize
                                                    )
                                                , keys
                                                , kiAcronym
                                                , lsPatterns, kiCapNext
                                                )
import qualified Palantype.Common.Indices      as KI
import qualified Palantype.Common.RawSteno     as Raw
import           Palantype.Common.RawSteno.Type ( RawSteno(RawSteno) )
import qualified Text.Parsec                   as Parsec
import           Text.Parsec                    ( Parsec
                                                , char
                                                , eof
                                                , getInput
                                                , getState
                                                , many
                                                , many1
                                                , runParser
                                                , sepBy1
                                                , setState
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
import Control.Monad (when)
import Palantype.Tools.TraceWords (TraceWords, traceSample)
import Palantype.Tools.Collision (StenoCodeInfo, toStenoCodeInfo)

data Score key = Score
    { -- | first criterion: make use of the maximum allowed level,
      --   i.e. the highest pattern group/greediness
      scoreLevel :: (PatternGroup key, Greediness)

      -- second criterion: maximize number of real-language letters per chord
    , scoreEfficiency :: Rational

      -- third criterion: minimize number of steno letters
    , scoreBrevity    :: Int
    }

deriving stock instance Palantype key => Eq  (Score key)
-- deriving stock instance Palantype key => Ord (Score key)

newtype ScoreLevelFirst key = ScoreLevelFirst { unScoreLevelFirst :: Score key }

deriving stock instance Palantype key => Eq (ScoreLevelFirst key)

instance Palantype key => Ord (ScoreLevelFirst key) where
  compare slf1 slf2 =
      -- highest level scores higher
         compare (scoreLevel      s1) (scoreLevel      s2)
      <> compare (scoreEfficiency s1) (scoreEfficiency s2)
      <> compare (scoreBrevity    s1) (scoreBrevity    s2)
    where
      s1 = unScoreLevelFirst slf1
      s2 = unScoreLevelFirst slf2

newtype ScoreEfficiencyFirst key = ScoreEfficiencyFirst { unScoreEfficiencyFirst :: Score key }

deriving stock instance Palantype key => Eq (ScoreEfficiencyFirst key)

instance Palantype key => Ord (ScoreEfficiencyFirst key) where
  compare sef1 sef2 =
         compare (scoreEfficiency s1) (scoreEfficiency s2)
      <> compare (scoreBrevity    s1) (scoreBrevity    s2)
      -- lowest level scores higher
      <> compare (scoreLevel      s2) (scoreLevel      s1)
    where
      s1 = unScoreEfficiencyFirst sef1
      s2 = unScoreEfficiencyFirst sef2


instance Palantype key => TextShow (Score key) where
    showb Score {..} =
        showb scoreLevel
            <> fromText "-"
            <> fromString (printf "%.1f" $ fromRational @Double scoreEfficiency)
            <> fromText "("
            <> showb scoreBrevity
            <> fromText ")"

instance Palantype key => Show (Score key) where
    show = Text.unpack . showt

data ParseError
  = PEParsec RawSteno Parsec.ParseError
  | PEImpossible Text
  deriving stock (Show)

instance TextShow ParseError where
    showb = fromString . show

isCapitalized :: Text -> Bool
isCapitalized ""  = error "isCapitalized: empty string"
isCapitalized str = isUpper $ Text.head str

addAcronymChord :: forall key . Palantype key => State key -> State key
addAcronymChord st@State {..} = st
    { stProtoSteno = ProtoChord (KI.toKeys kiAcronym) : ProtoSlash : stProtoSteno
    , stNChords    = stNChords + 1
    , stLevel      = (patAcronym, 0)
    }

addCapNextChord :: forall key. Palantype key => State key -> State key
addCapNextChord st@State{..} = st
    { stProtoSteno = ProtoChord (KI.toKeys kiCapNext) : ProtoSlash : stProtoSteno
    , stNChords    = stNChords + 1
    , stLevel      = (patCapitalize, 0)
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
    -> TraceWords (Either ParseError [StenoCodeInfo key])
parseSeries trie hyphenated =
    let
        -- calculate result for lower case word

        eAcronym =
            runParser ((,) <$> acronym <*> getInput) () "" hyphenated
        (hyphenated', isAcronym) = case eAcronym of
            Right (syls, rem) ->
                (Text.intercalate "|" syls <> rem, True)
            Left _ -> (hyphenated, False)

        str = Text.encodeUtf8 $ toLower hyphenated'

        st  = State { stProtoSteno = []
                    , stNLetters   = 0
                    , stNChords    = 1
                    , stMFinger    = Nothing
                    , stMLastKey   = Nothing
                    , stLevel      = (patZero, 0)
                    }

        levels =
            catMaybes
                $   lsPatterns @key
                <&> \(level, patterns) -> if any (`isInfixOf` str) patterns
                        then Just level
                        else Nothing

        lsResultLc =
            levels <&> \level -> optimizeStenoSeries trie level st str

        lsResult
            | isAcronym                 = mapSuccess (addAcronymChord @key) <$> lsResultLc
            | isCapitalized hyphenated' = (mapSuccess (addCapNextChord @key) <$> lsResultLc) <> lsResultLc
            | otherwise                 = lsResultLc
    in  do

          traceSample (Text.replace "|" "" hyphenated) $
            "Recognized greediness levels: " <> showt levels

          case sortOn (Down . ScoreEfficiencyFirst . score) lsResult of
              Failure raw err : _ -> pure $ Left $ PEParsec raw err
              [] -> pure $ Left $ PEImpossible $ "Empty list for: " <> hyphenated
              ls -> do
                traceSample (Text.replace "|" "" hyphenated) $
                  "Before filtering the alternatives for duplicates:\n" <> showt ls
                pure $ Right $ toStenoCodeInfo <$> zip [0..] (filterAlts ls)

  where
    filterAlts
        :: [Result (State key)]
        -> [(RawSteno, (PatternGroup key, Greediness))]
    filterAlts []                 = []
    filterAlts (Failure _ _ : as) = filterAlts as
    filterAlts (Success state : as) =
        let
            rawSteno = protoToSteno $ stProtoSteno state

            distinct = \case
              Failure _ _ -> False
              Success st2 -> rawSteno /= protoToSteno (stProtoSteno st2)

            as' = filter distinct as
        in
            (rawSteno, stLevel state) : filterAlts as'

newtype CountLetters = CountLetters { unCountLetters :: Int }
  deriving newtype (Num, Eq, TextShow)

countLetters :: Text -> CountLetters
countLetters str = CountLetters $ sum $ Text.length <$> Text.splitOn "|" str

newtype CountChords = CountChords { unCountChords :: Int }
  deriving newtype (Num, TextShow)

countChords :: [ProtoSteno k] -> CountChords
countChords = CountChords <<< foldl' (\s p -> s + count p) 0
  where
    count ProtoSlash     = 1
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
    count ProtoSlash      = 0
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
groupProto ls = let (cs, rem) = foldr acc ([], []) ls in Chord rem : cs
  where
    acc :: ProtoSteno k -> ([Chord k], [k]) -> ([Chord k], [k])
    acc ProtoSlash      (cs, current) = (Chord current : cs, [])
    acc (ProtoChord ks) (cs, current) = (cs, ks <> current)

protoToSteno :: Palantype k => [ProtoSteno k] -> RawSteno
protoToSteno ls =
    mconcat $ intersperse (Raw.fromText "/") $ Raw.fromChord <$> groupProto ls

data State key = State
    { stProtoSteno    :: [ProtoSteno key]
    , stNLetters      :: CountLetters
    , stNChords       :: CountChords
    , stMFinger       :: Maybe Finger
  -- | for compatibility with original palantype that relies on key order
  --   rather than on finger, because several keys per finger are allowed
    , stMLastKey      :: Maybe key
    , stLevel        :: (PatternGroup key, Greediness)
    }

instance Palantype key => TextShow (State key) where
    showb State {..} = showb $ protoToSteno stProtoSteno

bsPipe :: Word8
bsPipe = 0x7C

score :: forall k . Palantype k => Result (State k) -> Score k
score (Failure _ _) = Score (patZero, 0) 0 0
score (Success State{..} ) =
  let scoreLevel = stLevel
      scoreEfficiency =
            fromIntegral (unCountLetters stNLetters)
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
    -> (PatternGroup key, Greediness)
    -> State key
    -> ByteString
    -> Result (State key)
optimizeStenoSeries _ _ st "" = Success st
optimizeStenoSeries trie level st str | BS.head str == bsPipe =
    let
        newState = st
            { stProtoSteno  = stProtoSteno st <> [ProtoSlash]
            , stNChords     = stNChords st + 1
            , stMFinger     = Nothing
            , stMLastKey    = Nothing
            , stLevel       = max (patZero, 0) $ stLevel st
            }
        str' = BS.tail str
        r1   = optimizeStenoSeries trie level newState str'
        r2   = optimizeStenoSeries trie level st str'
    in
        maximumBy (comparing $ ScoreLevelFirst . score) [r1, r2]
optimizeStenoSeries trie level st str =
    let
        matches = filterGreediness $ flatten $ Trie.matches trie str

        matchToResult (consumed, (greediness, raw, pg), rem) =
            case parseKey (greediness, raw) (stMFinger st) (stMLastKey st) of
                Left err -> Failure raw err
                Right (proto, (mFinger, mLK)) ->
                    let
                        newState = State
                            { stProtoSteno = stProtoSteno st <> proto
                            , stNLetters   =
                                  stNLetters st
                                + countLetters (Text.decodeUtf8 consumed)
                            , stNChords    = stNChords st + countChords proto
                            , stMFinger    = mFinger
                            , stMLastKey   = mLK
                            , stLevel     = max (pg, greediness) $ stLevel st
                            }
                    in  optimizeStenoSeries trie level newState rem

        results = case matches of
            [] -> [Failure "" $ Parsec.newErrorUnknown (initialPos "")]
            ms -> matchToResult <$> ms
    in
        maximumBy (comparing $ ScoreLevelFirst . score) results
  where
    filterGreediness
        :: [(ByteString, (Greediness, RawSteno, PatternGroup key), ByteString)]
        -> [ ( ByteString
             , (Greediness, RawSteno, PatternGroup key)
             , ByteString
             )
           ]
    filterGreediness = filter (\(_, (g', _, pg'), _) -> (pg', g') <= level)

    flatten
        :: [ ( ByteString
             , [(Greediness, RawSteno, PatternGroup key)]
             , ByteString
             )
           ]
        -> [ ( ByteString
             , (Greediness, RawSteno, PatternGroup key)
             , ByteString
             )
           ]
    flatten = mconcat . fmap expand
      where
        expand
            :: ( ByteString
               , [(Greediness, RawSteno, PatternGroup key)]
               , ByteString
               )
            -> [ ( ByteString
                 , (Greediness, RawSteno, PatternGroup key)
                 , ByteString
                 )
               ]
        expand (c, rs, rem) = (c, , rem) <$> rs

    parseKey
        :: (Greediness, RawSteno)
        -> Maybe Finger
        -> Maybe key
        -> Either
               Parsec.ParseError
               ([ProtoSteno key], (Maybe Finger, Maybe key))
    parseKey (_, RawSteno s) mFinger' mLastKey' =
        let
            mFinger  = mFinger'
            mLastKey = mLastKey'
            ePair    = runParser ((,) <$> keysWithSlash <*> getState)
                                 (mFinger, mLastKey)
                                 ""
                                 s
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
        ucl  <- Parsec.satisfy isUpper
        lcls <- many $ Parsec.satisfy isLower
        _    <- optional $ char '|'
        pure $ Text.pack $ ucl : lcls

whenCIEqTraceWord
    :: forall m
    .  Applicative m
    => Maybe Text
    -> Text
    -> m ()
    -> m ()
whenCIEqTraceWord mTraceWord word a =
    whenJust mTraceWord \traceWord ->
        when (((==) `on` Text.toCaseFold) traceWord word) a

whenJust
    :: forall m a
    .  Applicative m
    => Maybe a
    -> (a -> m ())
    -> m ()
whenJust (Just x) func = func x
whenJust Nothing  _    = pure ()
