{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module Palantype.Tools.Steno where

import           Control.Applicative       (Alternative ((<|>)),
                                            Applicative (pure, (<*), (*>)))
import           Control.Category          (Category (id, (.)))
import           Control.Monad             (Monad ((>>=)), MonadPlus (mzero),
                                            foldM, join, when)
import           Control.Monad.Fail        (MonadFail (fail))
import qualified Data.Aeson                as Aeson
import           Data.Bifunctor            (Bifunctor (first, second))
import           Data.Bool                 (Bool (False))
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import           Data.Char                 (Char)
import           Data.Either               (Either (..), either)
import           Data.Eq                   (Eq ((==)))
import           Data.FileEmbed            (embedFile)
import           Data.Foldable             (Foldable (foldl, foldl', length, maximum, null, sum, foldr),
                                            maximumBy)
import           Data.Function             (const, ($), flip)
import           Data.Functor              (Functor (fmap), void, ($>), (<$>),
                                            (<&>))
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap
import           Data.Hashable             (Hashable)
import           Data.Int                  (Int)
import           Data.List                 (concat, dropWhile, head,
                                            intersperse, last, reverse, splitAt,
                                            (!!), (++))
import           Data.List.NonEmpty        (NonEmpty)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (Maybe (..), fromMaybe, maybe)
import           Data.Monoid               (Monoid (mconcat, mempty), (<>))
import           Data.Ord                  (Ord ((>=)), comparing)
import           Data.String               (String)
import           Data.Text                 (Text, intercalate, replace, splitOn,
                                            tail, toLower)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Data.Text.IO              (interact, putStrLn)
import           Data.Traversable          (Traversable (sequence), for)
import           Data.Trie                 (Trie)
import qualified Data.Trie                 as Trie
import           Data.Tuple                (fst, snd)
import           Data.Word                 (Word8)
import           GHC.Err                   (error)
import           GHC.Float                 (Double)
import           GHC.Generics              (Generic)
import           GHC.Num                   (Num (fromInteger, (-)), (+))
import           GHC.Real                  (Fractional ((/)), fromIntegral, (^))
import           Palantype.Common          (Chord (Chord), Finger (LeftPinky),
                                            Palantype (toFinger),
                                            Series (Series), mkChord)
import           Palantype.Common.RawSteno (RawSteno (RawSteno))
import qualified Palantype.Common.RawSteno as Raw
import           Palantype.DE.Keys         (Key)
import qualified Palantype.DE.Keys         as DE
import           Safe                      (headMay, lastMay)
import           System.Environment        (getArgs)
import           System.IO                 (IO)
import qualified Text.ParserCombinators.Parsec.Error as Parsec
import           Text.Parsec               (Parsec, anyChar, char, eof,
                                            evalParser, getInput, getState,
                                            letter, many1, noneOf, oneOf, parse,
                                            parserFail, runParser, sepBy1,
                                            setInput, setState, string, try, SourcePos (..))
import           Text.Printf               (printf)
import           Text.Show                 (Show (show))
import           TextShow                  (TextShow (showb, showbPrec, showt),
                                            fromText, singleton, fromString)
import           TextShow.Generic          (genericShowbPrec)
import Text.Parsec.Pos (initialPos)
import GHC.Fingerprint.Type (Fingerprint(Fingerprint))
import Data.List (filter)

data SeriesData = SeriesData
  { sdHyphenated :: Text
  , sdSeries     :: Series Key
  , sdScore      :: Double
  , sdPath       :: Path
  }

instance TextShow SeriesData where
 showb SeriesData{..} = fromText $
      sdHyphenated <> " "
   <> Text.pack (printf "%.1f" sdScore) <> " "
   <> showt sdPath <> " "
   <> showt sdSeries

data Path
  = PathException
  | PathOptimize
  deriving (Generic)

instance TextShow Path where
  showbPrec = genericShowbPrec

data ParseError
  = PEMissingPrimitives Text
  | PEParsec Parsec.ParseError
  | PEExceptionTable Text
  deriving (Show)

instance TextShow ParseError where
  showb = fromString . show

parseSeries :: Text -> Either ParseError SeriesData
parseSeries hyphenated =
  case HashMap.lookup (replace "|" "" hyphenated) mapExceptions of
    Just raw ->
      case Raw.parseWord raw of
        Right chords ->
          let sdHyphenated = hyphenated
              sdSeries = Series chords
              sdScore = 0
              sdPath = PathException
          in Right SeriesData{..}
        Left err ->
          Left $ PEExceptionTable $
               hyphenated
            <> "; " <> Text.pack (show err)
    Nothing ->
      let str' = Text.encodeUtf8 $ toLower hyphenated
          st = State
            { stSteno = []
            , stNLetters = 0
            , stNChords = 1
            , stMFinger = Nothing
            }
      in  case optimizeStenoSeries st str' of
            Left err -> Left $ PEMissingPrimitives err
            Right result -> case result of
              Success State{..} ->
                let -- the score of a series of chords for a given word is the average
                    -- chord score, i.e. the average number of letters per chord
                    sdHyphenated = hyphenated
                    sdScore = score result
                    sdSeries = Series $ toChords stSteno
                    sdPath = PathOptimize
                in  Right SeriesData{..}
              Failure err -> Left $ PEParsec err

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
      KoSSlash -> n + 1

-- | Optimize steno series

-- | a series is a list of steno keys, interspersed with slashes ('/') to mark
--   a new chord
data KeysOrSlash
  = KoSKeys [Key]
  | KoSSlash

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
  | Failure Parsec.ParseError

data State = State
  { stSteno    :: [KeysOrSlash]
  , stNLetters :: CountLetters
  , stNChords  :: CountChords
  , stMFinger  :: Maybe Finger
  }

bsPipe :: Word8
bsPipe = 0x7C

score
  :: Result State
  -> Double
score (Success State{..}) =
  fromIntegral (unCountLetters stNLetters) / fromIntegral (unCountChords stNChords)
score _ = 0

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
optimizeStenoSeries :: State -> ByteString -> Either Text (Result State)
optimizeStenoSeries st "" = Right $ Success st
optimizeStenoSeries st str | BS.head str == bsPipe =
  let newState = State
        { stSteno = KoSSlash : stSteno st
        , stNLetters = stNLetters st
        , stNChords = stNChords st + 1
        , stMFinger = Nothing
        }
      r1 = optimizeStenoSeries newState $ BS.tail str
      r2 = optimizeStenoSeries st $ BS.tail str
  in  sequence [r1, r2] <&> \results ->
        maximumBy (comparing score) results
optimizeStenoSeries st str =
  let ms = Trie.matches primitives str
  in  if null ms
        then  Left $ Text.decodeUtf8 str
        else let lsEResult = ms <&> \(consumed, result, rem) ->
                   case parseKey result (stMFinger st) of
                     Left  err -> Right $ Failure err
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
                       in  optimizeStenoSeries newState rem
             in  sequence lsEResult <&> \results ->
                   maximumBy (comparing score) results
  where
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
              (Right result, _)      -> Right result
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

-- | the primitives as defined in "primitives.json" parsed to a TRIE
primitives :: Trie [RawSteno]
primitives =
  let str = stripComments $(embedFile "primitives.json5")
      m = case Aeson.eitherDecodeStrict str of
             Right ls -> ls
             Left err -> error $ "Could not decode primitives.json5: " <> err
      lsByteString = first Text.encodeUtf8 <$> Map.toList m
  in  Trie.fromList lsByteString
