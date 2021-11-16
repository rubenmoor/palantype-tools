module Palantype.Tools.Collision where

import           Control.Applicative            ( (<$>)
                                                , Applicative(pure)
                                                )
import           Data.Foldable                  ( Foldable(foldl') )
import           Data.Function                  ( ($) )
import           Data.HashMap.Internal.Strict   ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Int                       ( Int )
import           Data.Maybe                     ( fromMaybe )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text.Lazy                as Lazy
import           Data.Text.Lazy.IO              ( readFile
                                                )
import           GHC.Err                        ( error )
import           System.IO                      ( IO
                                                )
import           Text.Read                      ( read )

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
freq freqs w =
  fromMaybe 0 (HashMap.lookup w freqs)

--       let
--         acc' m (r, pairs) = case pairs of
--
--           -- no collision: ignore path, store raw steno with word
--           -- TODO: unless there is already an entry
--           [(_, word)] -> pure $ HashMap.insert r word m
--
--           -- collision
--           p:_ -> do
--             mSolution <- solveCollision (snd <$> pairs) swsMapWordSteno
--             case mSolution of
--               Just stenoWord -> pure stenoWord
--               Nothing -> do
--                 appendLine fileStenoWordsCollisions $
--                   Lazy.fromStrict $ showt r <> ": " <> showt pairs
--                 pure (r, snd p)
--
--           -- impossible case
--           []     -> error "empty entry"
--
--       mapStenoWord <-
--         foldM acc' HashMap.empty (HashMap.toList swsMapStenoWords)
