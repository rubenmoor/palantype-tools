{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Palantype.Tools.Steno where

import           Control.Applicative            ( Applicative((*>), (<*), pure, (<*>))
                                                )
import           Control.Category               ( (<<<)
                                                , Category((.))
                                                )
import           Control.Lens                   ( view )
import           Control.Lens.Tuple             ( _2 )
import           Control.Monad                  ( MonadPlus(mzero)
                                                , foldM
                                                , when
                                                )
import           Control.Monad.Fail             ( MonadFail(fail) )
import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , Value(Array)
                                                )
import qualified Data.Aeson                    as Aeson
import           Data.Aeson.Types               ( Parser )
import           Data.Bifunctor                 ( Bifunctor(second, first) )
import           Data.Bool                      ( Bool(False, True), (&&), not )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import           Data.Char                      ( digitToInt
                                                , isDigit, isUpper
                                                )
import           Data.Either                    ( Either(Left, Right)
                                                , isLeft
                                                )
import           Data.Eq                        ( Eq((/=), (==)) )
import           Data.FileEmbed                 ( embedFile )
import           Data.Foldable                  ( Foldable
                                                    ( length
                                                    , sum
                                                    , toList
                                                    )
                                                , maximumBy
                                                )
import           Data.Function                  ( ($)
                                                , const
                                                )
import           Data.Functor                   ( ($>)
                                                , (<$>)
                                                , (<&>)
                                                , Functor(fmap)
                                                )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Hashable                  ( Hashable(hashWithSalt) )
import           Data.Int                       ( Int )
import           Data.List                      ( (++)
                                                , filter
                                                , head
                                                , intersperse

                                                , sortOn

                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( Maybe(Just, Nothing) )
import           Data.Monoid                    ( (<>)
                                                , mconcat, Monoid (mempty)
                                                )
import           Data.Ord                       ( Down(Down)
                                                , Ord((<=))
                                                , comparing
                                                )
import           Data.Ratio                     ( Rational
                                                )
import           Data.Text                      ( Text

                                                , replace
                                                , toLower
                                                )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Data.Trie                      ( Trie )
import qualified Data.Trie                     as Trie
import           Data.Tuple                     ( uncurry
                                                )
import           Data.Word                      ( Word8 )
import           GHC.Err                        ( error )
import           GHC.Float                      ( Double )
import           GHC.Generics                   ( Generic )
import           GHC.Num                        ( (+)
                                                , Num(negate)
                                                )
import           GHC.Real                       ( Fractional((/), fromRational)
                                                , fromIntegral
                                                )
import           Palantype.Common               ( Finger
                                                , Palantype

                                                )
import           Palantype.Common.RawSteno      ( RawSteno(RawSteno, unRawSteno)
                                                )
import qualified Palantype.Common.RawSteno     as Raw
import           Text.Parsec                    ( Parsec
                                                , char
                                                , eof
                                                , evalParser
                                                , sepBy1
                                                , setState
                                                )
import           Text.Parsec.Pos                ( initialPos )
import qualified Text.ParserCombinators.Parsec.Error
                                               as Parsec
import           Text.ParserCombinators.ReadP   ( (+++)
                                                , satisfy
                                                , string, option
                                                )
import           Text.Printf                    ( printf )
import           Text.Read                      ( Read(readListPrec, readPrec)
                                                , readListPrecDefault
                                                , readP_to_Prec
                                                )
import           Text.Show                      ( Show(show) )
import           TextShow                       ( TextShow(showb, showt)
                                                , fromString
                                                , fromText
                                                )
import qualified Palantype.Common.Indices as KI
import Palantype.Common.Dictionary (kiCapNext)
import qualified Palantype.DE.Keys as DE

type Greediness = Int

data SeriesData = SeriesData
    { sdHyphenated :: Text
    , sdRawSteno      :: RawSteno
    , sdScore      :: Score
    , sdPath       :: Path
    }

instance TextShow SeriesData where
    showb SeriesData {..} =
        fromText
            $  sdHyphenated
            <> " "
            <> showt sdScore
            <> " "
            <> showt sdPath
            <> " "
            <> showt sdRawSteno

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

data Path
  -- | Optimized by algo, given greediness, and True
  --   in case of a capitalized word w/o added capitalization chord
  = PathOptimize Greediness Bool
  | PathException
  deriving stock (Eq, Ord, Generic)

instance Hashable Path where
    hashWithSalt s = hashWithSalt s . showt

instance TextShow Path where
    showb PathException    = fromText "Exception"
    showb (PathOptimize g oC) =
      fromText "Opt" <> showb g <> if oC then fromText "C" else mempty

instance Read Path where
    readPrec = readP_to_Prec $ \_ -> exception +++ optimize
      where
        exception = string "Exception" $> PathException
        optimize =
            string "Opt" *> (PathOptimize <$> digitGreediness <*> isOptCap)
        digitGreediness = digitToInt <$> satisfy isDigit
        isOptCap        = option False $ satisfy (== 'C') $> True
    readListPrec = readListPrecDefault

data ParseError
  = PEParsec RawSteno Parsec.ParseError
  | PEExceptionTable Text
  deriving stock (Show)

instance TextShow ParseError where
    showb = fromString . show

-- data PartsData key = PartsData
--     { pdOrig  :: Text
--     , pdSteno :: Chord key
--     , pdPath  :: Path
--     }
--     deriving stock (Eq, Ord)

-- instance Palantype key => Hashable (PartsData key) where
--     hashWithSalt s PartsData {..} =
--         hashWithSalt s (pdOrig, showt pdSteno, showt pdPath)
--
-- instance Palantype key => TextShow (PartsData key) where
--     showb PartsData {..} =
--         fromText pdOrig <> " " <> showb pdSteno <> " " <> showb pdPath

isCapitalized :: Text -> Bool
isCapitalized "" = error "isCapitalized: empty string"
isCapitalized str =
  let (h, rest) = Text.splitAt 1 str
      c = head $ Text.unpack h
  in  isUpper c && Text.toLower rest == rest

{-|
Add the "capitalize next word" chord in front
-}
addCapChord :: forall key. Palantype key => State key -> State key
addCapChord st@State {..} = st
    { stStrRawSteno = showt (KI.toRaw @key kiCapNext) <> "/" <> stStrRawSteno
    , stNChords    = stNChords + 1
    }

-- | Find steno chords for a given, hyphenated word, e.g. "Ge|sund|heit"
--   or return a parser error.
--   In case of success, provide the most efficient steno chords along with a
--   list of alternatives. E.g. steno that uses more chords or more letters,
--   or steno that uses the same number of chords or letters, but requires
--   higher greediness
parseSeries
    :: forall key
     . Palantype key
    => Text
    -> Either ParseError [RawSteno]
parseSeries hyphenated =
    case HashMap.lookup unhyphenated mapExceptions of
        Just raw -> case Raw.parseWord @key raw of
            Right chords -> Right [Raw.unparts $ Raw.fromChord <$> chords]
            Left err ->
                Left
                    $  PEExceptionTable
                    $  unhyphenated
                    <> ": "
                    <> unRawSteno raw
                    <> "; "
                    <> Text.pack (show err)
        Nothing ->
            let
                -- calculate result for lower case word
                str = Text.encodeUtf8 $ toLower hyphenated
                st  = State { stStrRawSteno = ""
                            , stNLetters   = 0
                            , stNChords    = 1
                            , stMFinger    = Nothing
                            , stMLastKey   = Nothing
                            }

                lsResultLc =
                    [0 .. 4] <&> \maxG ->
                        ((maxG, False), ) $ optimizeStenoSeries maxG st str

                lsResult = if isCapitalized hyphenated

                  -- capitalized words enter as no-optimized with the
                  -- "capitalize next word"-chord added in front
                  -- AND as c-optimized version, w/o extra chord
                  then let lsNoCOpt = second (mapSuccess $ addCapChord @key) <$> lsResultLc
                           lsYesCOpt = first (second $ const True) <$> lsResultLc
                       in  lsNoCOpt ++ lsYesCOpt
                  else lsResultLc
            in
                case sortOn (Down . uncurry scoreWithG) lsResult of
                    (_, Failure raw err) : _ -> Left $ PEParsec raw err
                    [] -> error "impossible"
                    ls -> Right $ filterAlts ls
  where
    unhyphenated = replace "|" "" hyphenated

    filterAlts []                      = []
    filterAlts ((_, Failure _ _) : as) = filterAlts as
    filterAlts ((_, Success state) : as) =
        let
            rawSteno = stStrRawSteno state
            distinct (_, Failure _ _) = False
            distinct (_, Success st2) = rawSteno /= stStrRawSteno st2
        in  RawSteno rawSteno : filterAlts (filter distinct as)

-- | Scoring "with greediness"
--   This scoring serves to sort the result, no result is discarded
--   Given the efficiency of a steno code, lower greediness is preferred
scoreWithG
    :: forall k . (Greediness, Bool) -> Result (State k) -> (Rational, Greediness, Bool, Int)
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
mapSuccess f (Success x) = Success $ f x

data State key = State
    -- { stPartsSteno :: [KeysOrSlash key]
    { stStrRawSteno :: Text
    , stNLetters   :: CountLetters
    , stNChords    :: CountChords
    , stMFinger    :: Maybe Finger
    -- | for compatibility with original palantype that relies on key order
    --   rather than on finger, because several keys per finger are allowed
    , stMLastKey   :: Maybe key
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
                         , stNLetters   = stNLetters st
                         , stNChords    = stNChords st + 1
                         , stMFinger    = Nothing
                         , stMLastKey   = Nothing
                         }
        str' = BS.tail str
        r1   = optimizeStenoSeries g newState str'
        r2   = optimizeStenoSeries g st str'
    in  maximumBy (comparing score) [r1, r2]
optimizeStenoSeries g st str =
    let matches =
            filterGreediness $ flatten $ Trie.matches primitives str

        matchToResult (consumed, result, rem) =
            case parseKey result (stMFinger st) (stMLastKey st) of
                Left err -> Failure (view _2 result) err
                Right ((mFinger, mLK), strRaw) ->
                    let
                        newState = State
                            { stStrRawSteno = stStrRawSteno st <> strRaw
                            , stNLetters = stNLetters st + countLetters consumed
                            , stNChords    = stNChords st + countChords strRaw
                            , stMFinger    = mFinger
                            , stMLastKey   = mLK
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
        -> Either
               Parsec.ParseError
               ((Maybe Finger, Maybe key), Text)
    parseKey (_, RawSteno s) mFinger mLastKey =
        let ePair = evalParser keysWithSlash (mFinger, mLastKey) "" s
            makeRawStr =
              mconcat <<< intersperse "/" <<< fmap (mconcat <<< fmap showt)
        in  second makeRawStr <$> ePair

    keysWithSlash :: Parsec Text (Maybe Finger, Maybe key) [[key]]
    keysWithSlash =
        sepBy1 Raw.keys (char '/' *> setState (Nothing, Nothing)) <* eof

stripComments :: ByteString -> ByteString
stripComments content =
    let txt = Text.decodeUtf8 content
    in  Text.encodeUtf8 $ Text.unlines $ stripComment <$> Text.lines txt
  where
    stripComment :: Text -> Text
    stripComment str = head $ Text.splitOn "//" str

-- | full word exceptions
-- exceptions that span several chords go here
mapExceptions :: HashMap Text RawSteno
mapExceptions =
    let str = stripComments $(embedFile "exceptions.json5")
    in  case Aeson.eitherDecodeStrict str of
            Right ls  -> ls
            Left  err -> error $ "Could not decode exceptions.json5: " <> err

newtype PrimMap key = PrimMap { unPrimMap :: Map ByteString [(Greediness, RawSteno)] }

instance Palantype key => FromJSON (PrimMap key) where

    parseJSON (Array vs) = PrimMap <$> foldM acc Map.empty vs

      where

        acc
            :: Map ByteString [(Greediness, RawSteno)]
            -> Value
            -> Parser (Map ByteString [(Greediness, RawSteno)])

        acc m jv@(Array vec) = do
            (k, v) <- case toList vec of
                [k, Array v] -> pure (k, v)
                _            -> fail $ "malformed entry: " <> show jv

            key        <- parseJSON k
            (g, r) <- case toList v of
                [vG, vRaw] -> do
                    g   <- parseJSON vG
                    raw <- parseJSON vRaw
                    pure (g, raw)
                _ ->
                    fail
                        $  "malformed entry: "
                        <> Text.unpack key
                        <> ": "
                        <> show v
            when (isLeft $ Raw.parseSteno @key r)
                $  fail
                $  "malformed raw steno: "
                <> Text.unpack key
                <> ": "
                <> show r
            let keyBs   = Text.encodeUtf8 key

            pure $ Map.insertWith (++) keyBs [(g, r)] m

        acc _ other = fail $ "malformed: " <> show other

    parseJSON _ = mzero

-- | the primitives as defined in "primitives.json" parsed to a TRIE
primitives
    :: Trie [(Greediness, RawSteno)]
primitives =
    let str     = stripComments $(embedFile "primitives.json5")
        primMap = case Aeson.eitherDecodeStrict str of
            Right m   -> m :: PrimMap DE.Key
            Left  err -> error $ "Could not decode primitives.json5: " <> err
    in  Trie.fromList $ Map.toList $ unPrimMap primMap
