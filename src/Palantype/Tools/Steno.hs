{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module Palantype.Tools.Steno where

import           Control.Applicative                 (Alternative ((<|>)),
                                                      Applicative (pure, (*>), (<*)))
import           Control.Category                    (Category (id, (.)))
import           Control.Monad                       (Monad ((>>=)),
                                                      MonadPlus (mzero), foldM,
                                                      join, when)
import           Control.Monad.Fail                  (MonadFail (fail))
import           Data.Aeson                          (FromJSON (parseJSON),
                                                      Value (Object, Array),
                                                      withObject)
import qualified Data.Aeson                          as Aeson
import           Data.Bifunctor                      (Bifunctor (first, second))
import           Data.Bool                           (Bool (False))
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Lazy                as LBS
import           Data.Char                           (Char)
import           Data.Either                         (Either (..), either)
import           Data.Eq                             (Eq ((==)))
import           Data.FileEmbed                      (embedFile)
import           Data.Foldable                       (Foldable (foldl, foldl', foldr, length, maximum, null, sum),
                                                      maximumBy, minimumBy)
import           Data.Function                       (const, flip, ($))
import           Data.Functor                        (Functor (fmap), void,
                                                      ($>), (<$>), (<&>))
import           Data.HashMap.Strict                 (HashMap)
import qualified Data.HashMap.Strict                 as HashMap
import           Data.Hashable                       (Hashable)
import           Data.Int                            (Int)
import           Data.List                           (concat, dropWhile, filter,
                                                      head, intersperse, last,
                                                      reverse, splitAt, (!!),
                                                      (++))
import           Data.List.NonEmpty                  (NonEmpty)
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Maybe                          (Maybe (..), fromMaybe,
                                                      maybe)
import           Data.Monoid                         (Monoid (mconcat, mempty),
                                                      (<>))
import           Data.Ord                            (Ord ((>=), (<=)), comparing)
import           Data.Ratio                          (Ratio, Rational, (%))
import           Data.String                         (String)
import           Data.Text                           (Text, intercalate,
                                                      replace, splitOn, tail,
                                                      toLower)
import qualified Data.Text                           as Text
import qualified Data.Text.Encoding                  as Text
import           Data.Text.IO                        (interact, putStrLn)
import           Data.Traversable                    (Traversable (sequence),
                                                      for)
import           Data.Trie                           (Trie)
import qualified Data.Trie                           as Trie
import           Data.Tuple                          (fst, snd)
import           Data.Word                           (Word8)
import           GHC.Err                             (error)
import           GHC.Fingerprint.Type                (Fingerprint (Fingerprint))
import           GHC.Float                           (Double)
import           GHC.Generics                        (Generic)
import           GHC.Num                             (Num (fromInteger, (-)),
                                                      (+))
import           GHC.Real                            (Fractional (fromRational, (/)),
                                                      fromIntegral, (^))
import           Palantype.Common                    (Chord (Chord),
                                                      Finger (LeftPinky),
                                                      Palantype (toFinger),
                                                      Series (Series), mkChord)
import           Palantype.Common.RawSteno           (RawSteno (RawSteno, unRawSteno))
import qualified Palantype.Common.RawSteno           as Raw
import           Palantype.DE.Keys                   (Key)
import qualified Palantype.DE.Keys                   as DE
import           Safe                                (headMay, lastMay)
import           System.Environment                  (getArgs)
import           System.IO                           (IO)
import           Text.Parsec                         (Parsec, SourcePos (..),
                                                      anyChar, char, eof,
                                                      evalParser, getInput,
                                                      getState, letter, many1,
                                                      noneOf, oneOf, parse,
                                                      parserFail, runParser,
                                                      sepBy1, setInput,
                                                      setState, string, try)
import           Text.Parsec.Pos                     (initialPos)
import qualified Text.ParserCombinators.Parsec.Error as Parsec
import           Text.Printf                         (printf)
import           Text.Show                           (Show (show))
import           TextShow                            (TextShow (showb, showbPrec, showt),
                                                      fromString, fromText,
                                                      singleton)
import           TextShow.Generic                    (genericShowbPrec)
import Debug.Trace (traceShow)
import Graphics.Rendering.Chart.Easy (levels)

type Greediness = Int

data SeriesData = SeriesData
  { sdHyphenated :: Text
  , sdSeries     :: Series Key
  , sdScore      :: Score
  , sdPath       :: Path
  , sdGreediness :: Greediness
  }

instance TextShow SeriesData where
 showb SeriesData{..} = fromText $
      sdHyphenated
   <> " " <> showt sdScore
   <> " " <> showt sdPath
   <> " G" <> showt sdGreediness
   <> " " <> showt sdSeries


data Score = Score
  { scorePrimary   :: Rational
  , scoreSecondary :: Int
  } deriving (Eq, Ord)

instance TextShow Score where
  showb Score{..} = fromString (printf "%.1f" $ fromRational @Double scorePrimary)

instance Show Score where
  show = Text.unpack . showt

data Path
  = PathException
  | PathOptimize
  deriving (Generic)

instance TextShow Path where
  showbPrec = genericShowbPrec

data ParseError
  = PEMissingPrimitives Text
  | PEParsec [RawSteno] Parsec.ParseError
  | PEExceptionTable Text
  deriving (Show)

instance TextShow ParseError where
  showb = fromString . show

parseSeries :: Greediness -> Text -> Either ParseError SeriesData
parseSeries greediness hyphenated =
  let unhyphenated = replace "|" "" hyphenated
  in case HashMap.lookup unhyphenated mapExceptions of
      Just raw ->
        case Raw.parseWord raw of
          Right chords ->
            let sdHyphenated = hyphenated
                sdSeries = Series chords
                sdScore = Score (fromIntegral (Text.length unhyphenated) % fromIntegral (length chords)) $ sum (length <$> chords)
                sdPath = PathException
                sdGreediness = greediness
            in Right SeriesData{..}
          Left err ->
            Left $ PEExceptionTable $
                 unhyphenated
              <> ": " <> unRawSteno raw
              <> "; " <> Text.pack (show err)
      Nothing ->
        let str = Text.encodeUtf8 $ toLower hyphenated
            st = State
              { stSteno = []
              , stNLetters = 0
              , stNChords = 1
              , stMFinger = Nothing
              }
        in  case optimizeStenoSeries greediness st str of
              Left  err    -> Left $ PEMissingPrimitives err
              Right result -> case result of
                Success State{..} ->
                  let -- the score of a series of chords for a given word is the average
                      -- chord score, i.e. the average number of letters per chord
                      sdHyphenated = hyphenated
                      sdScore = score result
                      sdSeries = Series $ toChords stSteno
                      sdPath = PathOptimize
                      sdGreediness = greediness
                  in  Right SeriesData{..}
                Failure raw err -> Left $ PEParsec raw err

newtype CountLetters = CountLetters { unCountLetters :: Int }
  deriving (Num)

countLetters
  :: ByteString
  -> CountLetters
countLetters str =
  CountLetters $ sum $ BS.length <$> BS.split bsPipe str

newtype CountChords = CountChords { unCountChords :: Int }
  deriving (Num)

countChords
  :: [KeysOrSlash]
  -> CountChords
countChords = foldl' acc 0
  where
    acc n = \case
      KoSKeys _ -> n
      KoSSlash  -> n + 1

-- | Optimize steno series

-- | a series is a list of steno keys, interspersed with slashes ('/') to mark
--   a new chord
data KeysOrSlash
  = KoSKeys [Key]
  | KoSSlash

countKeys
  :: [KeysOrSlash]
  -> Int
countKeys =
    foldl' acc 0
  where
    acc n = \case
      KoSKeys ks -> n + length ks
      KoSSlash   -> n

toChords
  :: [KeysOrSlash]
  -> [Chord Key]
toChords ls =
    let (cs, ks) = foldl' acc ([], []) ls
    in  mkChord ks : cs
  where
    acc
      :: ([Chord Key], [Key])
      -> KeysOrSlash
      -> ([Chord Key], [Key])
    acc (chords, keys) = \case
      (KoSKeys ks) -> (chords, keys ++ ks)
      KoSSlash     -> (mkChord keys : chords, [])

data Result a
  = Success a
  | Failure [RawSteno] Parsec.ParseError

data State = State
  { stSteno    :: [KeysOrSlash]
  , stNLetters :: CountLetters
  , stNChords  :: CountChords
  , stMFinger  :: Maybe Finger
  }

bsPipe :: Word8
bsPipe = 0x7C

-- | The score has two values, a primary score and a secondary score.
--   The primary score is the number of letters per chord.
--   A high number of letters per chords means high typing efficiency.
--   The secondary score is used, when two series achieve the same score.
--   The secondary score is the number of steno keys.
--   A lower number is preferred.
score
  :: Result State
  -> Score
score (Success State{..}) =
  let scorePrimary =
          fromIntegral (unCountLetters stNLetters)
        / fromIntegral (unCountChords stNChords)
      scoreSecondary = countKeys stSteno
  in  Score{..}
score _ = Score 0 0

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
  let newState = State
        { stSteno = KoSSlash : stSteno st
        , stNLetters = stNLetters st
        , stNChords = stNChords st + 1
        , stMFinger = Nothing
        }
      r1 = optimizeStenoSeries g newState $ BS.tail str
      r2 = optimizeStenoSeries g st $ BS.tail str
  in  sequence [r1, r2] <&> \results ->
        maximumBy (comparing score) results
optimizeStenoSeries g st str =
  let ms = Trie.matches primitives str
  in  if null ms
        then  Left $ Text.decodeUtf8 str
        else let lsEResult = ms <&> \(consumed, result, rem) ->
                   -- TODO: always run all greediness levels
                   --       and check if higher greediness yields different result
                   case parseKey (filterGreediness g result) (stMFinger st) of
                     Left  err -> Right $ Failure (snd <$> result) err
                     Right (mFinger, lsKoS) ->
                       let newSteno = case lsKoS of
                             [kos] -> kos : stSteno st
                             _     -> foldl' (flip (:)) (stSteno st) lsKoS
                           newState = State
                             { stSteno = newSteno
                             , stNLetters = stNLetters st + countLetters consumed
                             , stNChords = stNChords st + countChords lsKoS
                             , stMFinger = mFinger
                             }
                       in  optimizeStenoSeries g newState rem
             in  sequence lsEResult <&> \results ->
                   maximumBy (comparing score) results
  where
    filterGreediness
      :: Greediness
      -> [(Greediness, RawSteno)]
      -> [RawSteno]
    filterGreediness maxG =
      foldl' (\rs (g, r) -> if g <= maxG then r:rs else rs) []

    parseKey
      :: [RawSteno]
      -> Maybe Finger
      -> Either Parsec.ParseError (Maybe Finger, [KeysOrSlash])
    parseKey ls mFinger =
      let primitive str =
            let ePair = evalParser keysWithSlash mFinger "" str
            in  second (intersperse KoSSlash . fmap KoSKeys) <$> ePair

          acc parser (RawSteno str) =
            case (parser, primitive str) of
              (Right r1, Right r2)   -> Right $ minimumBy (comparing fst) [r1, r2]
              (Right result, Left _) -> Right result
              (Left _, Right result) -> Right result
              (Left _, Left err)     -> Left err

      in  foldl' acc (Left $ Parsec.newErrorUnknown (initialPos "")) ls

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

newtype PrimMap = PrimMap { unPrimMap :: Map ByteString [(Greediness, RawSteno)] }

instance FromJSON PrimMap where
  parseJSON (Array v) = do
      m <- foldM acc Map.empty v
      pure $ PrimMap m
    where
      acc m pair = do
        (k, v) <- parseJSON pair
        let key = Text.encodeUtf8 k
        pure $ Map.insertWith (++) key [v] m
  parseJSON _ = mzero

-- | the primitives as defined in "primitives.json" parsed to a TRIE
primitives :: Trie [(Greediness, RawSteno)]
primitives =
  let str = stripComments $(embedFile "primitives.json5")
      primMap = case Aeson.eitherDecodeStrict str of
        Right m -> m :: PrimMap
        Left err -> error $ "Could not decode primitives.json5: " <> err
  in  Trie.fromList $ Map.toList $ unPrimMap primMap
