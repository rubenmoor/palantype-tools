{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Applicative   (Alternative ((<|>)),
                                        Applicative (pure, (*>), (<*>)))
import           Control.Category      (Category ((.)))
import           Control.Monad         (MonadPlus (mzero), foldM)
import           Data.Bifunctor        (Bifunctor (first, second))
import           Data.Bool             (Bool (False))
import qualified Data.ByteString       as ByteString
import qualified Data.ByteString.Char8 as Char8
import           Data.Char             (Char)
import           Data.Either           (Either (..))
import           Data.Eq               (Eq)
import           Data.FileEmbed        (embedFile)
import           Data.Foldable         (Foldable (foldl, length, maximum),
                                        maximumBy)
import           Data.Function         (($))
import           Data.Functor          (Functor (fmap), void, ($>), (<$>),
                                        (<&>))
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HashMap
import           Data.Int              (Int)
import           Data.List             (concat, dropWhile, head, intersperse,
                                        last, splitAt, (!!), (++))
import           Data.List.NonEmpty    (NonEmpty)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (Maybe (..), fromMaybe, maybe)
import           Data.Monoid           (Monoid (mconcat), (<>))
import           Data.Ord              (Ord ((>=)), comparing)
import           Data.String           (String)
import           Data.Text             (Text, intercalate, splitOn, toLower, replace)
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import           Data.Text.IO          (interact, putStrLn)
import           Data.Traversable      (for)
import           Data.Trie             (Trie)
import qualified Data.Trie             as Trie
import           Data.Tuple            (fst, snd)
import           GHC.Err               (error)
import           GHC.Float             (Double)
import           GHC.Num               (Num ((-)), (+))
import           GHC.Real              (Fractional ((/)), fromIntegral, (^))
import           Safe                  (headMay, lastMay)
import           System.Environment    (getArgs)
import           System.IO             (IO)
import qualified Text.JSON5            as JSON5
import           Text.Parsec           (ParseError, Parsec, char, eof, getState,
                                        letter, many, many1, parse, runParser,
                                        sepBy1, setState, string, try)
import           Text.Show             (Show (show))
import           TextShow              (TextShow (showb, showt), singleton)

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ parseSeries $ Text.pack $ head args

parseSeries :: Text -> Text
parseSeries str =
  case HashMap.lookup (replace "|" "" str) mapExceptions of
    Just steno -> showt $ parseRawSteno steno
    Nothing ->
      case optSeries $ splitOn "|" $ toLower str of
        Right (scoreAcc, chords) ->
          let -- the score of a series of chords for a given word is the average
              -- chord score, i.e. the average number of letters per chord
              score = (fromIntegral scoreAcc :: Double)
                    / fromIntegral ( length chords)
          in  showt (Series chords) <> " " <> showt score
        Left  err   -> err

optSeries :: [Text] -> Either Text (Int, [Chord])
optSeries [] = Right (0, [])
optSeries parts =
  maximum $
    [1 .. (length parts)] <&> \i ->
      let (front, back) = splitAt i parts
      in  do
            (scoreFront, cFront) <- parseChord $ intercalate "|" front
            (scoreBack , cBack ) <- optSeries back
            pure (scoreFront + scoreBack, cFront : cBack)

parseChord :: Text -> Either Text (Int, Chord)
parseChord str =
    case runParser chord LeftPinky "series" str of
      Left  err -> Left  $ Text.pack $ show err
      -- the score of a chord is the number of letters it successfully encoded
      Right ks  -> Right (Text.length str, Chord ks)
  where
    chord = do
      initial <- keys
      tail <- concat <$> many (pipe <|> keys)
      eof
      pure $ initial ++ tail

    pipe = char '|' $> []

    keys = do

      lastFinger <- getState

      -- drop used keys from list
      -- a key is used when the finger of that key has been used
      let predicate pair = fromMaybe False $ do
            k <- headMay $ snd pair
            pure $ lastFinger >= finger k

          remPrimitives = dropWhile predicate primitives

          -- string consumes when fails
          acc parser (str, ks) =
            let primitive = do
                  _ <- try $ string $ Text.unpack str
                  setState $ maybe lastFinger finger (lastMay ks)
                  pure ks
            in  parser <|> primitive
      foldl acc mzero remPrimitives

-- full word exceptions
-- exceptions that span several chords go here
mapExceptions :: HashMap Text Text
mapExceptions =
  let str = Char8.unpack $(embedFile "exceptions.json5")
      ls = case JSON5.decodeStrict str of
             JSON5.Ok ls -> ls
             JSON5.Error err -> error $ "Could not decode exceptions.json5: " <> err
  in  HashMap.fromList ls

-- primitives

mapCharParsers :: Trie [Text]
mapCharParsers =
  let str = Char8.unpack $(embedFile "primitives.json5")
      ls = case JSON5.decodeStrict str of
             JSON5.Ok    ls  -> ls
             JSON5.Error err -> error $ "Could not decode primitives.json5: " <> err
      lsByteString = first Text.encodeUtf8 <$> ls
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
