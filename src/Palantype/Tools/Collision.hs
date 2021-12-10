{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Palantype.Tools.Collision where

import           Control.Applicative            ( (<$>)

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
import           Data.Maybe                     ( Maybe(Just, Nothing)
                                                , fromMaybe
                                                , maybe
                                                , maybeToList
                                                )
import           Data.Monoid                    ( mconcat )
import           Data.Ord                       ( Down(Down)
                                                , Ord((>))
                                                , comparing
                                                )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text
                                                , intercalate
                                                , toLower

                                                )
import           Data.Tuple                     ( snd )
import           GHC.Err                        ( error )
import           Palantype.Common.RawSteno      ( RawSteno )
import           Palantype.Tools.Steno          ( Path )
import           TextShow                       ( TextShow(showb, showt)
                                                , fromText
                                                )

resolveCollision :: Text -> Text -> Text
resolveCollision str _ = str

data StateCollision = StateCollision
    { stcLsStenoWord   :: [(RawSteno, Text)]
    , stcMapWordStenos :: HashMap Text [RawSteno]
    , stcLosers        :: [Loser]
    }

data Loser = Loser
    { loserCase       :: LoserCase
    , loserWord       :: Text
    , loserFreq       :: Int
    , loserSteno      :: [RawSteno]
    , loserWinner     :: Text
    , loserWinnerFreq :: Int
    }

data LoserCase
  = Case1A
  | Case1B
  | Case2
  | Case3

instance TextShow Loser where
    showb Loser {..} =
        fromText
            $  loserWord
            <> " "
            <> showt loserFreq
            <> " "
            <> showt loserCase
            <> " < "
            <> loserWinner
            <> " "
            <> showt loserWinnerFreq
            <> ": "
            <> intercalate ", " (showt <$> loserSteno)

instance TextShow LoserCase where
    showb = fromText . \case
        Case1A -> "Case1A"
        Case1B -> "Case1B"
        Case2  -> "Case2"
        Case3  -> "Case3"

stateInitial :: HashMap Text [RawSteno] -> StateCollision
stateInitial m = StateCollision [] m []

resolveCollisions
    :: HashMap Text Int
    -> HashMap Text [RawSteno]
    -> HashMap RawSteno [(Path, Text)]
    -> StateCollision
resolveCollisions freqs mapWordStenos mapStenoWords =
    let
        acc st (r, words) =
            let m           = stcMapWordStenos st
                nAlts       = nAlternatives m
                lsStenoWord = stcLsStenoWord st
                losers      = stcLosers st
            in  case words of

                    -- Case 0: no collision: ignore path, store raw steno with word
                    -- TODO: unless there is already an entry
                    [(_, word)] ->
                        st { stcLsStenoWord = (r, word) : lsStenoWord }

                    -- Case 0C: pseudo collision due to capitalization
                    [(_, w1), (_, w2)] | toLower w1 == toLower w2 ->
                        st { stcLsStenoWord = (r, toLower w1) : lsStenoWord }

                    -- Case 1a: collision where first word doesn't have alternatives
                    [(_, w1), (_, w2)] | nAlts w1 == 1 && nAlts w2 > 1 ->
                        let (m', mLoser) = removeAlt Case1A r w1 m w2
                        in  st { stcLsStenoWord   = (r, w1) : lsStenoWord
                               , stcMapWordStenos = m'
                               , stcLosers        = maybe id (:) mLoser losers
                               }

                    -- Case 1b: collision where second word doesn't have alternatives
                    [(_, w1), (_, w2)] | nAlts w2 == 1 && nAlts w1 > 1 ->
                        let (m', mLoser) = removeAlt Case1B r w2 m w1
                        in  st { stcLsStenoWord   = (r, w2) : lsStenoWord
                               , stcMapWordStenos = m'
                               , stcLosers        = maybe id (:) mLoser losers
                               }

                    -- Case 2: collision where neither word has alternatives OR
                    -- both words have
                    -- in this case: let the frequency decide
                    [(_, w1), (_, w2)] ->
                        let (winner, runnerUp) = sortTwo w1 w2
                            (m', mLoser) = removeAlt Case2 r winner m runnerUp
                        in  st { stcLsStenoWord   = (r, winner) : lsStenoWord
                               , stcMapWordStenos = m'
                               , stcLosers        = maybe id (:) mLoser losers
                               }

                    -- Case 3: collision of more then two words
                    h : ts ->
                        let
                            (_, winner) :| runnerUps =
                                -- NonEmpty.sortBy
                                --      (comparing $ Down . freq freqs . snd)
                                --   $ h
                                       h
                                    :| ts
                            acc' (map, ls) ru =
                                let (m'', mLoser) =
                                        removeAlt Case3 r winner map ru
                                in  (m'', maybe id (:) mLoser ls)
                            (m', losers') =
                                foldl' acc' (m, losers) $ snd <$> runnerUps
                        in
                            st { stcLsStenoWord   = (r, winner) : lsStenoWord
                               , stcMapWordStenos = m'
                               , stcLosers        = losers'
                               }

                    -- impossible case
                    [] -> error "empty entry"
    in  foldl' acc (stateInitial mapWordStenos)
                -- sort by descending path:
                -- G3, G2, G1, G0, Exception
                -- This way, G3 is given to high scoring words with priority
                                                $ HashMap.toList mapStenoWords
  where
    nAlternatives :: HashMap Text [RawSteno] -> Text -> Int
    nAlternatives m word =
        length $ fromMaybe (error "nAlternatives: impossible") $ HashMap.lookup
            word
            m

    freq _ _ = 0

    -- | remove an alternative, because it's been used for another word
    --   if there are no more alternatives, the word becomes a loser
    --   i.e. a word w/o steno representation in the result
    removeAlt
        :: LoserCase
        -> RawSteno
        -> Text
        -> HashMap Text [RawSteno]
        -> Text
        -> (HashMap Text [RawSteno], Maybe Loser)
    removeAlt lcase raw winner m word = case HashMap.lookup word m of
        Just ls ->
            let
                new = delete raw ls
                ml  = if null new
                    then
                        let
                            raws = mconcat $ maybeToList $ HashMap.lookup
                                word
                                mapWordStenos
                            f  = freq freqs word
                            fW = freq freqs winner
                        in
                            (, Just $ Loser lcase word f raws winner fW)
                    else (, Nothing)
            in
                ml $ HashMap.insert word new m
        Nothing -> error "removeAlt: impossible"

    sortTwo :: Text -> Text -> (Text, Text)
    sortTwo w1 w2 =
        let f1 = freq freqs w1
            f2 = freq freqs w2
        in  if f1 > f2 then (w1, w2) else (w2, w1)
