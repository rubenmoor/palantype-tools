{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Palantype.Tools.Collision where

import           Control.Applicative            ( (<$>)
                                                , Applicative(pure)
                                                )
import           Control.Category               ( Category((.), id) )
import           Data.Bool                      ( (&&) )
import           Data.Eq                        ( Eq((==)) )
import           Data.Foldable                  ( Foldable(foldl', null) )
import           Data.Function                  ( ($) )
import           Data.HashMap.Internal.Strict   ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Int                       ( Int )
import           Data.List                      ( delete
                                                , length
                                                )
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.List.NonEmpty             ( NonEmpty((:|)) )
import Data.Monoid (mconcat)
import           Data.Maybe                     ( Maybe(Just, Nothing)
                                                , fromMaybe, maybe, maybeToList
                                                )
import           Data.Ord                       ( Ord((>), (>=))
                                                , comparing
                                                )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text
                                                , toLower
                                                , intercalate
                                                )
import qualified Data.Text.Lazy                as Lazy
import           Data.Text.Lazy.IO              ( readFile )
import           GHC.Err                        ( error )
import           Palantype.Common.RawSteno      ( RawSteno(RawSteno) )
import           System.IO                      ( IO )
import           Text.Read                      ( read )
import TextShow (TextShow (showt, showb), fromText)

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

data StateCollision = StateCollision
    { stcLsStenoWord   :: [(RawSteno, Text)]
    , stcMapWordStenos :: HashMap Text [RawSteno]
    , stcLosers        :: [Loser]
    }

data Loser = Loser
 { loserWord :: Text
 , loserFreq :: Int
 , loserSteno :: [RawSteno]
 , loserWinner :: Text
 , loserWinnerFreq :: Int
 }

instance TextShow Loser where
  showb Loser {..} =
    fromText $ loserWord <> " " <> showt loserFreq
            <> " < " <> loserWinner <> " " <> showt loserWinnerFreq
            <> ": " <> intercalate ", " (showt <$> loserSteno)

stateInitial :: HashMap Text [RawSteno] -> StateCollision
stateInitial m = StateCollision [] m []

resolveCollisions
    :: HashMap Text Int
    -> HashMap Text [RawSteno]
    -> HashMap RawSteno [Text]
    -> StateCollision
resolveCollisions freqs mapWordStenos mapStenoWords =
    let
        acc st (r, words) =
            let m     = stcMapWordStenos st
                nAlts = nAlternatives m
                lsStenoWord    = stcLsStenoWord st
                losers = stcLosers st
            in  case words of

                -- no collision: ignore path, store raw steno with word
                -- TODO: unless there is already an entry
                    [word] -> st { stcLsStenoWord = (r, word) : lsStenoWord }

                    -- pseudo collision due to capitalization
                    [w1, w2] | toLower w1 == toLower w2 ->
                        st { stcLsStenoWord = (r, toLower w1) : lsStenoWord }

                    -- collision where first word doesn't have alternatives
                    [w1, w2] | nAlts w1 == 1 && nAlts w2 >= 1 ->
                               let (m', mLoser) = removeAlt r w1 m w2
                               in  st { stcLsStenoWord            = (r, w1) : lsStenoWord
                                      , stcMapWordStenos = m'
                                      , stcLosers = maybe id (:) mLoser losers
                                      }

                    -- collision where second word doesn't have alternatives
                    [w1, w2] | nAlts w2 == 1 && nAlts w1 >= 1 ->
                               let (m', mLoser) = removeAlt r w2 m w1
                               in  st { stcLsStenoWord            = (r, w2) : lsStenoWord
                                      , stcMapWordStenos = m'
                                      , stcLosers = maybe id (:) mLoser losers
                                      }

                    -- collision where neither word has alternatives OR
                    -- both words have
                    -- in this case: let the frequency decide
                    [w1, w2] ->
                        let (winner, runnerUp) = sortTwo w1 w2
                            (m', mLoser) =  removeAlt r winner m runnerUp
                        in  st { stcLsStenoWord            = (r, winner) : lsStenoWord
                               , stcMapWordStenos = m'
                               , stcLosers = maybe id (:) mLoser losers
                               }

                    -- collision of more then two words
                    h : ts ->
                        let
                            winner :| runnerUps =
                                NonEmpty.sortBy (comparing $ freq freqs)
                                    $  h
                                    :| ts
                            acc' (map, ls) ru =
                              let (m', mLoser) = removeAlt r winner map ru
                              in  (m', maybe id (:) mLoser ls)
                            (m', losers) = foldl' acc' (m, []) runnerUps
                        in  st
                                { stcLsStenoWord            = (r, winner) : lsStenoWord
                                , stcMapWordStenos = m'
                                , stcLosers = losers
                                }

                    -- impossible case
                    [] -> error "empty entry"
    in  foldl' acc (stateInitial mapWordStenos) (HashMap.toList mapStenoWords)
  where
    nAlternatives :: HashMap Text [RawSteno] -> Text -> Int
    nAlternatives m word =
        length $ fromMaybe (error "nAlternatives: impossible") $ HashMap.lookup
            word
            m

    -- | remove an alternative, because it's been used for another word
    --   if there are no more alternatives, the word becomes a loser
    --   i.e. a word w/o steno representation in the result
    removeAlt
        :: RawSteno
        -> Text
        -> HashMap Text [RawSteno]
        -> Text
        -> (HashMap Text [RawSteno], Maybe Loser)
    removeAlt raw winner m word =
      case HashMap.lookup word m of
        Just ls ->
          let new = delete raw ls
              ml = if null new
                     then let raws =
                                 mconcat $ maybeToList $
                                   HashMap.lookup word mapWordStenos
                              f = freq freqs word
                              fW = freq freqs winner
                          in  (, Just $ Loser word f raws winner fW)
                     else (, Nothing)
          in  ml $ HashMap.insert word new m
        Nothing -> error "removeAlt: impossible"

    sortTwo :: Text -> Text -> (Text, Text)
    sortTwo w1 w2 =
        let f1 = freq freqs w1
            f2 = freq freqs w2
        in  if f1 > f2 then (w1, w2) else (w2, w1)
