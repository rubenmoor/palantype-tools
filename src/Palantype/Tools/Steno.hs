{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

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
import qualified Data.ByteString           as ByteString
import qualified Data.ByteString.Lazy      as LBS
import           Data.Char                 (Char)
import           Data.Either               (Either (..))
import           Data.Eq                   (Eq)
import           Data.FileEmbed            (embedFile)
import           Data.Foldable             (Foldable (foldl, length, maximum),
                                            maximumBy)
import           Data.Function             (($))
import           Data.Functor              (Functor (fmap), void, ($>), (<$>),
                                            (<&>))
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap
import           Data.Hashable             (Hashable)
import           Data.Int                  (Int)
import           Data.List                 (concat, dropWhile, head,
                                            intersperse, last, splitAt, (!!),
                                            (++))
import           Data.List.NonEmpty        (NonEmpty)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (Maybe (..), fromMaybe, maybe)
import           Data.Monoid               (Monoid (mconcat, mempty), (<>))
import           Data.Ord                  (Ord ((>=)), comparing)
import           Data.String               (String)
import           Data.Text                 (Text, intercalate, replace, splitOn,
                                            toLower)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Data.Text.IO              (interact, putStrLn)
import           Data.Traversable          (for)
import           Data.Trie                 (Trie)
import qualified Data.Trie                 as Trie
import           Data.Tuple                (fst, snd)
import           GHC.Err                   (error)
import           GHC.Float                 (Double)
import           GHC.Generics              (Generic)
import           GHC.Num                   (Num ((-)), (+))
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
                                            setInput, setState, string, try)
import           Text.Printf               (printf)
import           Text.Show                 (Show (show))
import           TextShow                  (TextShow (showb, showbPrec, showt),
                                            fromText, singleton)
import           TextShow.Generic          (genericShowbPrec)

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

parseSeries :: Text -> Either ParseError SeriesData
parseSeries hyphenated =
  case HashMap.lookup (replace "|" "" hyphenated) mapExceptions of
    Just raw ->
      Raw.parseWord raw <&> \chords ->
        let sdHyphenated = hyphenated
            sdSeries = Series chords
            sdScore = 0
            sdPath = PathException
        in SeriesData{..}
    Nothing ->
      let str' = splitOn "|" $ toLower hyphenated
      in  optimizeStenoSeries str' <&> \(scoreAcc, chords) ->
            let -- the score of a series of chords for a given word is the average
                -- chord score, i.e. the average number of letters per chord
                sdHyphenated = hyphenated
                sdScore = (fromIntegral scoreAcc :: Double)
                      / fromIntegral ( length chords)
                sdSeries = Series chords
                sdPath = PathOptimize
            in  SeriesData{..}

optimizeStenoSeries :: [Text] -> Either ParseError (Int, [Chord Key])
optimizeStenoSeries [] = Right (0, [])
  -- TODO: Quatsch
optimizeStenoSeries parts =
    maximumBy (comparing score) $
      [1 .. (length parts)] <&> \i ->
        let (front, back) = splitAt i parts
        in  do
              (scoreFront, cFront) <- parseChord $ intercalate "|" front
              (scoreBack , cBack ) <- optimizeStenoSeries back
              pure (scoreFront + scoreBack, cFront : cBack)
  where
    score (Left _)       = 0
    score (Right (s, _)) = s

parseChord :: Text -> Either ParseError (Int, Chord Key)
parseChord str =
    let strUtf8 = Text.encodeUtf8 str
    in  runParser chord Nothing "series" strUtf8 <&> \ks ->
          -- the score of a chord is the number of letters it successfully
          -- encoded
          (Text.length str, Chord ks)

  where
    chord =
      -- TODO: replace 'concat <$> many1 keys' by backtracking
      concat <$> sepBy1 (concat <$> many1 keys) pipe <* eof

    pipe = char '|' $> []

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
