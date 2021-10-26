{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Palantype.Tools.Steno where

import           Control.Applicative       (Alternative ((<|>)),
                                            Applicative (pure, (<*)))
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
import           Data.Foldable             (Foldable (foldl, length, maximum, foldl', null),
                                            maximumBy)
import           Data.Function             (($), const)
import           Data.Functor              (Functor (fmap), void, ($>), (<$>),
                                            (<&>))
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap
import           Data.Hashable             (Hashable)
import           Data.Int                  (Int)
import           Data.List                 (concat, dropWhile,
                                            intersperse, last, splitAt, (!!),
                                            (++), head, reverse)
import           Data.List.NonEmpty        (NonEmpty)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (Maybe (..), fromMaybe, maybe)
import           Data.Monoid               (Monoid (mconcat, mempty), (<>))
import           Data.Ord                  (Ord ((>=)), comparing)
import           Data.String               (String)
import           Data.Text                 (Text, intercalate, replace, splitOn,
                                            toLower, tail)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Data.Text.IO              (interact, putStrLn)
import           Data.Traversable          (for, Traversable (sequence))
import           Data.Trie                 (Trie)
import qualified Data.Trie                 as Trie
import           Data.Tuple                (fst, snd)
import           GHC.Err                   (error)
import           GHC.Float                 (Double)
import           GHC.Generics              (Generic)
import           GHC.Num                   (Num ((-), fromInteger), (+))
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
import           Text.Parsec               (ParseError, Parsec, anyChar, char,
                                            eof, evalParser, getInput, getState,
                                            letter, many1, noneOf, parse,
                                            parserFail, runParser, sepBy1,
                                            setInput, setState, string, try, oneOf)
import           Text.Printf               (printf)
import           Text.Show                 (Show (show))
import           TextShow                  (TextShow (showb, showbPrec, showt),
                                            fromText, singleton)
import           TextShow.Generic          (genericShowbPrec)
import Data.Word (Word8)

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

parseSeries :: Text -> Either Text SeriesData
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
          Left $ "Parse error in exception table: "
            <> hyphenated
            <> "; " <> Text.pack (show err)
    Nothing ->
      let str' = Text.encodeUtf8 $ toLower hyphenated
          st = State
            { stSteno = []
            , stNLetters = 0
            , stNChords = 1
            , stMFinger = Nothing
            }
          result = optimizeStenoSeries st str'
      in  case result of
            Left err ->
              Left $ "Encountered missing primitive when parsing: " <> err
            Right result -> case result of
              Success State{..} ->
                let -- the score of a series of chords for a given word is the average
                    -- chord score, i.e. the average number of letters per chord
                    sdHyphenated = hyphenated
                    sdScore = score result
                    sdSeries = Series $ toChords stSteno
                    sdPath = PathOptimize
                in  Right SeriesData{..}
              Failure -> Left "Parsing failed"

newtype CountLetters = CountLetters { unCountLetters :: Int }
  deriving (Num)

countLetters
  :: ByteString
  -> CountLetters
countLetters str =
  CountLetters $ BS.length str

newtype CountChords = CountChords { unCountChords :: Int }
  deriving (Num)

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
      KoSSlash    -> (mkChord keys : chords, [])

data Result a
  = Success a
  | Failure

data State = State
  { stSteno :: [KeysOrSlash]
  , stNLetters :: CountLetters
  , stNChords :: CountChords
  , stMFinger :: Maybe Finger
  }

bsPipe :: Word8
bsPipe = 0x7C

score
  :: Result State
  -> Double
score (Success State{..}) =
  fromIntegral (unCountLetters stNLetters) / fromIntegral (unCountChords stNChords)
score _ = 0

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
                     Nothing -> Right Failure
                     Just (mFinger, ks) ->
                       let newState = State
                             { stSteno = KoSKeys ks : stSteno st
                             , stNLetters = stNLetters st + countLetters consumed
                             , stNChords = stNChords st
                             , stMFinger = mFinger
                             }
                       in  optimizeStenoSeries newState rem
             in  sequence lsEResult <&> \results ->
                   maximumBy (comparing score) results
  where
    parseKey ls mFinger =
      let primitive str =
            either (const Nothing) Just $
              evalParser (Raw.keys <* eof) mFinger "" str

          acc parser (RawSteno str) =
            parser <|> primitive str

      in  foldl' acc Nothing ls

    -- maximumBy (comparing score) $
    --   [1 .. (length parts)] <&> \i ->
    --     let (front, back) = splitAt i parts
    --     in  do
    --           (scoreFront, cFront) <- parseChord $ intercalate "|" front
    --           (scoreBack , cBack ) <- optimizeStenoSeries back
    --           pure (scoreFront + scoreBack, cFront : cBack)

-- | A chord tries to fit as many letters as possible into a steno
--   expression that can be typed all at once.
--   The score of a chord is the number of letters it successfully
--   encoded, w/o the hyphenation symbol.
parseChord :: Text -> Either ParseError (Int, Chord Key)
parseChord str =
    let strUtf8 = Text.encodeUtf8 str
    in  runParser chord Nothing "series" strUtf8 <&> \ks ->
          (Text.length $ replace "|" "" str, Chord ks)

  where
    chord =
      -- TODO: replace 'concat <$> many1 keys' by backtracking
      concat <$> many1 keys <* eof

    keys = do

      mFinger <- getState
      str <- getInput

      -- I don't know why this is necessary, but apparently 'setInput'
      -- is not enough, i.e. setInput "" creates a weird error
      void $ noneOf "|"

      (consumed, result, rem) <-
        -- TODO: matches
        maybe (parserFail "no primitive match") pure $ Trie.match primitives str

      let primitive (RawSteno raw) =
            case evalParser (Raw.keys <* eof) mFinger "trie" raw of
              Left  err           -> fail $ show err
              Right (mFinger, ks) -> setState mFinger $> ks

          acc parser raw =
            parser <|> primitive raw

      ks <- foldl acc (fail "primitives no parse") result
      setInput rem
      pure ks

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

-- TODO: input can have
--         '-', '/'
--         digits
--         ? other special characters?
--       otherwise:
--         a-zäüöß
--         international: é è, à, ê, ...


-- chord :: Parsec Text ParserState Chord
-- chord = inner <* (eof <|> char '|')
--   where
--     inner = many1 key
--     key =

-- probably useless:

newtype Part = Part { unPart :: Text }

parseParts
  :: Text
  -> Either ParseError [Part]
parseParts = parse parts "command line input"
  where
    parts = sepBy1 part $ char '|'
    part = Part . Text.pack <$> letters
    letters = many1 letter
