module Palantype.Tools.Collision where

import           Control.Applicative            ( (<$>)
                                                , Applicative(pure)
                                                )
import           Control.Monad                  ( foldM )
import           Data.Foldable                  ( Foldable(foldl') )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<&>) )
import           Data.HashMap.Internal.Strict   ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Int                       ( Int )
import           Data.Maybe                     ( fromMaybe )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                as Lazy
import           Data.Text.Lazy.IO              ( readFile )
import           Data.Tuple                     ( snd )
import           GHC.Err                        ( error )
import           Palantype.Common.RawSteno      ( RawSteno )
import           Palantype.Tools.Steno          ( Path )
import           System.IO                      ( IO )
import           Text.Read                      ( read )
import           TextShow                       ( TextShow(showt) )

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

type MapStenoWord = HashMap RawSteno Text
type MapCollisions = HashMap RawSteno [(Path, Int, Text)]

getMapStenoWord
    :: HashMap Text Int
    -> HashMap RawSteno [(Path, Text)]
    -> (MapStenoWord, MapCollisions)
getMapStenoWord freqs mapStenoWords =
    let acc (mapSW, mapCollisions) (r, lsPathWord) = case lsPathWord of

            -- no collision: ignore path, store raw steno with word
            -- TODO: unless there is already an entry
            [(_, word)] -> (HashMap.insert r word mapSW, mapCollisions)

            -- collision
            p : _ ->
                -- appendLine fileStenoWordsCollisions
                --     $  Lazy.fromStrict
                --     $  showt r
                --     <> ": "
                --     <> showPathFreqWord lsPathWord
                let lsPathFreqWord =
                        lsPathWord <&> \(path, word) -> (path, freq freqs word, word)
                in  ( HashMap.insert r (snd p) mapSW
                    , HashMap.insert r lsPathFreqWord mapCollisions
                    )

            -- impossible case
            [] -> error "empty entry"
    in  foldl' acc (HashMap.empty, HashMap.empty) (HashMap.toList mapStenoWords)
