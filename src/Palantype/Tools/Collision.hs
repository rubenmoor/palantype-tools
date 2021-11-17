module Palantype.Tools.Collision where

import           Control.Applicative            ( (<$>)
                                                , Applicative(pure)
                                                )
import           Data.Foldable                  ( Foldable(foldl'), maximumBy )
import           Data.Function                  ( ($) )
import           Data.HashMap.Internal.Strict   ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Int                       ( Int )
import           Data.Maybe                     ( fromMaybe )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text.Lazy                as Lazy
import           Data.Text.Lazy.IO              ( readFile )
import           GHC.Err                        ( error )
import           Palantype.Common.RawSteno      ( RawSteno )
import           System.IO                      ( IO )
import           Text.Read                      ( read )
import Data.Ord (comparing)

-- | word frequencies from UNI Leipzig based on
--   35 Mio. sentences
readFrequencies :: IO (HashMap Text Int)
readFrequencies = do
    ls <- Lazy.lines <$> readFile "deu_news_2020_freq.txt"
    let acc m l = case Lazy.head l of
            '#' -> m
            _   -> case Lazy.splitOn "\t" l of
                [w, strFrequency] -> HashMap.insert
                    (Lazy.toStrict w)
                    (read $ Lazy.unpack strFrequency)
                    m
                _ -> error $ "could not read: " <> Lazy.unpack l
    pure $ foldl' acc HashMap.empty ls

freq :: HashMap Text Int -> Text -> Int
freq freqs w = fromMaybe 0 (HashMap.lookup w freqs)

getLsStenoWord
    :: HashMap Text Int
    -> HashMap RawSteno [Text]
    -> [(RawSteno, Text)]

getLsStenoWord freqs mapStenoWords =
    let
        acc lsSW (r, words) = case words of

            -- no collision: ignore path, store raw steno with word
            -- TODO: unless there is already an entry
            [word] -> (r, word) : lsSW

            -- collision
            _ : _ ->
                let
                    word = maximumBy (comparing $ freq freqs) words
                    -- lsPathFreqWord = words <&> \word -> (freq freqs word, word)
                in  (r, word) : lsSW

            -- impossible case
            [] -> error "empty entry"
    in  foldl' acc [] (HashMap.toList mapStenoWords)
