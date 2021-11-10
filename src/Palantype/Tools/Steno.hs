{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TupleSections #-}

module Palantype.Tools.Steno where

import           Control.Applicative                 (Applicative (pure, (*>), (<*)))
import           Control.Category                    (Category ((.)), (<<<))
import           Control.Monad                       (MonadPlus (mzero), foldM,
                                                      when)
import           Control.Monad.Fail                  (MonadFail (fail))
import           Data.Aeson                          (FromJSON (parseJSON),
                                                      Value (Array))
import qualified Data.Aeson                          as Aeson
import           Data.Bifunctor                      (Bifunctor (second))
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString                     as BS
import           Data.Char                           (digitToInt, isDigit)
import           Data.Either                         (Either (Left, Right),
                                                      isLeft)
import           Data.Eq                             (Eq ((==)))
import           Data.FileEmbed                      (embedFile)
import           Data.Foldable                       (Foldable (foldl', length, sum, toList),
                                                      maximumBy, minimumBy)
import           Data.Function                       (flip, ($))
import           Data.Functor                        (Functor (fmap), ($>),
                                                      (<$>), (<&>))
import           Data.HashMap.Strict                 (HashMap)
import qualified Data.HashMap.Strict                 as HashMap
import           Data.Int                            (Int)
import           Data.List                           (head, intersperse, repeat,
                                                      zip, (++))
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Maybe                          (Maybe (Just, Nothing))
import           Data.Monoid                         ((<>))
import           Data.Ord                            (Ord ((<=)), comparing)
import           Data.Ratio                          (Rational, (%))
import           Data.Text                           (Text, intercalate,
                                                      replace, toLower)
import qualified Data.Text                           as Text
import qualified Data.Text.Encoding                  as Text
import           Data.Traversable                    (Traversable (sequence), traverse)
import           Data.Trie                           (Trie)
import qualified Data.Trie                           as Trie
import           Data.Tuple                          (fst, snd, uncurry)
import           Data.Word                           (Word8)
import           GHC.Err                             (error)
import           GHC.Float                           (Double)
import           GHC.Generics                        (Generic)
import           GHC.Num                             (Num (negate), (+))
import           GHC.Real                            (Fractional (fromRational, (/)),
                                                      fromIntegral)
import           Palantype.Common                    (Chord, Finger, mkChord)
import           Palantype.Common.RawSteno           (RawSteno (RawSteno, unRawSteno))
import qualified Palantype.Common.RawSteno           as Raw
import           Palantype.DE.Keys                   (Key)
import qualified Palantype.DE.Keys                   as DE
import           Text.Parsec                         (Parsec, char, eof,
                                                      evalParser, sepBy1,
                                                      setState)
import           Text.Parsec.Pos                     (initialPos)
import qualified Text.ParserCombinators.Parsec.Error as Parsec
import           Text.ParserCombinators.ReadP        (satisfy, string, (+++))
import           Text.Printf                         (printf)
import           Text.Read                           (Read (readListPrec, readPrec),
                                                      readListPrecDefault,
                                                      readP_to_Prec)
import           Text.Show                           (Show (show))
import           TextShow                            (TextShow (showb, showt),
                                                      fromString, fromText)
import Data.Aeson.Types (Parser)
import Control.Lens (view)
import Control.Lens.Tuple (_2)
import Data.Hashable (Hashable (hashWithSalt))

type Greediness = Int

data SeriesData = SeriesData
  { sdHyphenated :: Text
  , sdParts      :: [(Text, Chord Key)]
  , sdScore      :: Score
  , sdPath       :: Path
  }

instance TextShow SeriesData where
 showb SeriesData{..} = fromText $
      sdHyphenated
   <> " " <> showt sdScore
   <> " " <> showt sdPath
   <> " " <> intercalate "/" (fmap (showt <<< snd) sdParts)


data Score = Score
  { scorePrimary   :: Rational
  , scoreSecondary :: Int
  } deriving stock (Eq, Ord)

instance TextShow Score where
  showb Score{..} = fromString (printf "%.1f" $ fromRational @Double scorePrimary)

instance Show Score where
  show = Text.unpack . showt

data Path
  = PathException
  | PathOptimize Greediness
  deriving stock (Eq, Ord, Generic)

instance TextShow Path where
  showb PathException    = fromText "Exception"
  showb (PathOptimize g) = fromText "Opt" <> showb g

instance Read Path where
  readPrec = readP_to_Prec $ \_ -> exception +++ optimize
    where
      exception = string "Exception" $> PathException
      optimize  = string "Opt" *> (PathOptimize . digitToInt <$> satisfy isDigit)
  readListPrec = readListPrecDefault

data ParseError
  = PEMissingPrimitives Text
  | PEParsec [RawSteno] Parsec.ParseError
  | PEExceptionTable Text
  deriving stock (Show)

instance TextShow ParseError where
  showb = fromString . show

data PartsData
  = PartsData
    { pdOrig  :: Text
    , pdSteno :: Chord DE.Key
    , pdPath  :: Path
    }
  deriving stock (Eq, Ord)

instance Hashable PartsData where
  hashWithSalt s PartsData{..} =
    hashWithSalt s (pdOrig, showt pdSteno, showt pdPath)

instance TextShow PartsData where
  showb PartsData{..} =
       fromText pdOrig
    <> " " <> showb pdSteno
    <> " " <> showb pdPath

parseSeries :: Greediness -> Text -> Either ParseError SeriesData
parseSeries maxGreediness hyphenated =
  let
    unhyphenated = replace "|" "" hyphenated
  in
    case HashMap.lookup unhyphenated mapExceptions of
    Just raw ->
      case Raw.parseWord raw of
        Right chords ->
          let sdHyphenated = hyphenated
              sdParts = zip (repeat "") chords

              efficiency =
                  fromIntegral (Text.length unhyphenated)
                % fromIntegral (length chords)

              sdScore = Score efficiency $ negate $ sum (length <$> chords)
              sdPath = PathException
          in Right SeriesData{..}
        Left err ->
          Left $ PEExceptionTable $
               unhyphenated
            <> ": " <> unRawSteno raw
            <> "; " <> Text.pack (show err)
    Nothing ->
      let
        str = Text.encodeUtf8 $ toLower hyphenated
        st = State
          { stPartsSteno = []
          , stNLetters = 0
          , stNChords = 1
          , stMFinger = Nothing
          }
        lsEResult =
          [0..maxGreediness] <&> \maxG -> (maxG, ) <$> optimizeStenoSeries maxG st str
      in
        case sequence lsEResult of
          Left  err     -> Left $ PEMissingPrimitives err
          Right pairs ->
            case maximumBy (comparing $ uncurry scoreWithG) pairs of
              (g, result@(Success State{..})) ->
                let
                  sdHyphenated = hyphenated
                  sdScore = score result
                  sdParts = toParts stPartsSteno
                  sdPath = PathOptimize g
                in
                  Right SeriesData{..}
              (_, Failure raw err) -> Left $ PEParsec raw err

scoreWithG
  :: Greediness
  -> Result State
  -> (Rational, Greediness, Int)
scoreWithG g result =
  let Score {..} = score result in (scorePrimary, negate g, scoreSecondary)

newtype CountLetters = CountLetters { unCountLetters :: Int }
  deriving newtype (Num)

countLetters
  :: ByteString
  -> CountLetters
countLetters str =
  CountLetters $ sum $ BS.length <$> BS.split bsPipe str

newtype CountChords = CountChords { unCountChords :: Int }
  deriving newtype (Num)

countChords
  :: [KeysOrSlash]
  -> CountChords
countChords = foldl' acc 0
  where
    acc n = \case
      KoSKeys _ _ -> n
      KoSSlash -> n + 1

-- | Optimize steno series

-- | a series is a list of steno keys, interspersed with slashes ('/') to mark
--   a new chord
data KeysOrSlash
  = KoSKeys Text [Key]
  | KoSSlash

countKeys
  :: [KeysOrSlash]
  -> Int
countKeys =
    foldl' acc 0
  where
    acc n = \case
      KoSKeys _ ks -> n + length ks
      KoSSlash     -> n

toParts
  :: [KeysOrSlash]
  -> [(Text, Chord Key)]
toParts ls =
    let (lsParts, (str, keys)) = foldl' acc ([], ("", [])) ls
    in  (str, mkChord keys) : lsParts
  where
    acc
      :: ([(Text, Chord Key)], (Text, [Key]))
      -> KeysOrSlash
      -> ([(Text, Chord Key)], (Text, [Key]))
    acc (chords, (str, keys)) = \case
      (KoSKeys s ks) -> (chords, (s <> str, ks ++ keys))
      KoSSlash       -> ((str, mkChord keys) : chords, ("", []))

data Result a
  = Success a
  | Failure [RawSteno] Parsec.ParseError

data State = State
  { stPartsSteno :: [KeysOrSlash]
  , stNLetters :: CountLetters
  , stNChords  :: CountChords
  , stMFinger  :: Maybe Finger
  }

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
score
  :: Result State
  -> Score
score (Success State{..}) =
  let
    scorePrimary =
        fromIntegral (unCountLetters stNLetters)
      / fromIntegral (unCountChords stNChords)
    scoreSecondary = negate $ countKeys stPartsSteno
  in
    Score{..}
score (Failure _ _) = Score 0 0

-- | Try to fit as many letters as possible into a steno
--   chord (a chord contains keys that can be typed all at once).
--   The score of a chord is the number of letters it successfully
--   encoded, w/o the hyphenation symbol.
--
-- | look at next character:
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
optimizeStenoSeries :: Greediness -> State -> ByteString -> Either Text (Result State)
optimizeStenoSeries _ st "" = Right $ Success st
optimizeStenoSeries g st str | BS.head str == bsPipe =
  let
    newState = State
      { stPartsSteno = KoSSlash : stPartsSteno st
      , stNLetters = stNLetters st
      , stNChords = stNChords st + 1
      , stMFinger = Nothing
      }
    str' = BS.tail str
    r1 = optimizeStenoSeries g newState str'
    r2 = optimizeStenoSeries g st str'
  in
     maximumBy (comparing score) <$> sequence [r1, r2]
optimizeStenoSeries g st str =
    let
      matches = Trie.matches primitives str

      matchToResult (consumed, result, rem) =
        -- TODO: always run all greediness levels
        --       and check if higher greediness yields different result
        case parseKey consumed (filterGreediness result) (stMFinger st) of
          Left  err -> Right $ Failure (view _2 <$> result) err
          Right (mFinger, lsKoS) ->
            let
              newSteno = case lsKoS of
                [kos] -> kos : stPartsSteno st
                _     -> foldl' (flip (:)) (stPartsSteno st) lsKoS
              newState = State
                { stPartsSteno = newSteno
                , stNLetters = stNLetters st + countLetters consumed
                , stNChords = stNChords st + countChords lsKoS
                , stMFinger = mFinger
                }
            in
              optimizeStenoSeries g newState rem

      eResults = case matches of
        [] -> Left $ Text.decodeUtf8 str
        ms -> sequence $ matchToResult <$> ms
    in
      maximumBy (comparing score) <$> eResults
  where
    filterGreediness
      :: [(Greediness, RawSteno, [Int])]
      -> [(RawSteno, [Int])]
    filterGreediness =
      foldl' (\rs (g', r, is) -> if g' <= g then (r, is):rs else rs) []

    parseKey
      :: ByteString
      -> [(RawSteno, [Int])]
      -> Maybe Finger
      -> Either Parsec.ParseError (Maybe Finger, [KeysOrSlash])
    parseKey orig ls mFinger =
      let
        primitive s is =
          let
            ePair = evalParser keysWithSlash mFinger "" s

            acc' (strs, strOrig) i =
              let (begin, end) = Text.splitAt i strOrig
              in  (strs ++ [begin], end)

            (origs, rem) = foldl' acc' ([], Text.decodeUtf8 orig) is
            lsOrig = replace "|" "" <$> origs ++ [rem]

            toKoS = intersperse KoSSlash . fmap (uncurry KoSKeys) . zip lsOrig
          in
            second toKoS <$> ePair

        acc parser (RawSteno s, is) =
          case (parser, primitive s is) of
            (Right r1, Right r2)   -> Right $ minimumBy (comparing fst) [r1, r2]
            (Right result, Left _) -> Right result
            (Left _, Right result) -> Right result
            (Left _, Left err)     -> Left err

      in
        foldl' acc (Left $ Parsec.newErrorUnknown (initialPos "")) ls

    keysWithSlash :: Parsec Text (Maybe Finger) [[Key]]
    keysWithSlash =
      sepBy1 Raw.keys (char '/' *> setState Nothing) <* eof

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

newtype PrimMap = PrimMap { unPrimMap :: Map ByteString [(Greediness, RawSteno, [Int])] }

instance FromJSON PrimMap where

  parseJSON (Array vs) =
      PrimMap <$> foldM acc Map.empty vs

    where

      acc
        :: Map ByteString [(Greediness, RawSteno, [Int])]
        -> Value
        -> Parser (Map ByteString [(Greediness, RawSteno, [Int])])

      acc m jv@(Array vec) = do
        (k, v) <- case toList vec of
          [k, Array v] -> pure (k, v)
          _      -> fail $ "malformed entry: " <> show jv
        -- (k, v) <- parseJSON pair
        key <- parseJSON k
        (g, r, is) <- case toList v of
              (vG:vRaw:vIs) -> do
                g <- parseJSON vG
                raw <- parseJSON vRaw
                is <- traverse parseJSON vIs
                pure (g, raw, is)
              _        -> fail $ "malformed entry: " <> Text.unpack key <> ": " <> show v
        when (isLeft $ Raw.parseSteno @DE.Key r) $
          fail $ "malformed raw steno: " <> Text.unpack key <> ": " <> show r
        let keyBs = Text.encodeUtf8 key
        pure $ Map.insertWith (++) keyBs [(g, r, is)] m

      acc _ other = fail $ "malformed: " <> show other

  parseJSON _ = mzero

-- | the primitives as defined in "primitives.json" parsed to a TRIE
primitives :: Trie [(Greediness, RawSteno, [Int])]
primitives =
  let str = stripComments $(embedFile "primitives.json5")
      primMap = case Aeson.eitherDecodeStrict str of
        Right m  -> m :: PrimMap
        Left err -> error $ "Could not decode primitives.json5: " <> err
  in  Trie.fromList $ Map.toList $ unPrimMap primMap
