{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module BuildDict where

import Args (OptionsStenoDict (..))
import Common (
    appendLine,
    removeFiles,
    writeJSONFile,
 )
import Control.Applicative (Applicative (pure))
import Control.Arrow ((***))
import Control.Category (
    Category ((.)),
    (<<<),
 )
import Control.Concurrent (getNumCapabilities, modifyMVar, newMVar)
import Control.Concurrent.Async (replicateConcurrently)
import qualified Control.Concurrent.Lock as Lock
import Control.Monad (
    foldM,
    when,
    (<$!>),
 )
import qualified Data.Aeson as Aeson
import Data.Either (Either (..))
import Data.Foldable (
    Foldable (foldl', length, toList),
    for_,
    minimumBy,
    traverse_,
 )
import Data.Function (($))
import Data.Functor (
    ($>),
    (<$>), Functor (fmap)
 )
import Data.Int (Int)
import Data.List (sortOn, (++), take)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (
    Maybe (..),
    fromMaybe,
 )
import Data.Monoid (
    mconcat,
    (<>),
 )
import Data.Ord (Down (Down), comparing)
import Data.Strict.Tuple (Pair ((:!:)))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.Tuple (fst, snd)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Formatting (
    fprint,
    (%),
 )
import Formatting.Clock (timeSpecs)
import GHC.Err (error)
import GHC.Exts (seq)
import GHC.Float (Double)
import GHC.Num ((+))
import GHC.Real (
    Fractional ((/)),
    Real,
    realToFrac,
 )
import Palantype.Common (Greediness, Lang (DE, EN), PatternGroup)
import Palantype.Common.RawSteno (RawSteno)
import qualified Palantype.DE.Keys as DE
import qualified Palantype.DE.Pattern as DE
import qualified Palantype.EN.Keys as EN
import Palantype.Tools.Collision (DictState (..))
import qualified Palantype.Tools.Collision as Collision
import Palantype.Tools.Steno (
    ParseError (..),
    parseSeries,
 )
import Sort (getMapFrequencies)
import System.Clock (
    Clock (Monotonic),
    getTime,
 )
import System.Directory (doesFileExist)
import System.IO (
    FilePath,
    IO,
    hFlush,
    putStr,
    putStrLn,
    stdout,
 )
import Text.Show (Show (show))
import TextShow (TextShow (showt))
import WCL (wcl)

fileDictDuplicates :: FilePath
fileDictDuplicates = "buildDict-duplicates.txt"

fileLost :: FilePath
fileLost = "buildDict-lostwords.txt"

fileCollisions :: FilePath
fileCollisions = "buildDict-collisions.txt"

average :: forall a t. (Real a, Foldable t) => t a -> Double
average ls =
    let (t, n) = foldl' (\(!b, !c) a -> (a + b, (c :: Int) + 1)) (0, 0) ls
     in realToFrac t / realToFrac n

buildDict :: OptionsStenoDict -> IO ()
buildDict (OStDArg lang str) = do
    let parseSeries' = case lang of
            DE -> parseSeries @DE.Key @DE.Pattern
            EN -> parseSeries @EN.Key @DE.Pattern -- TODO: EN.Pattern
    case parseSeries' str of
        Left err -> Text.putStrLn $ showt err
        Right sds -> traverse_ (Text.putStrLn <<< showt) sds
buildDict (OStDFile fileInput fileOutputJson fileOutputDoc bAppend lang) = do
    start <- getTime Monotonic

    let lsFiles =
            [ fileDictNoParse
            , fileDictDuplicates
            , fileLost
            , fileCollisions
            , fileOutputDoc
            ]
    removeFiles lsFiles

    -- initial state with existing steno in output file
    mapStenoWord <-
        if bAppend
            then do
                nLO <- wcl fileOutputJson
                putStr $
                    "Reading data from output file: "
                        <> fileOutputJson
                        <> " ("
                        <> show nLO
                        <> " lines) ..."
                hFlush stdout
                mMap <- Aeson.decodeFileStrict' fileOutputJson
                putStrLn $ mMap `seq` " done."
                pure $ fromMaybe (error "Could not decode file.") mMap
            else pure Map.empty
    let accFlip m (steno, word) =
            Map.insertWith (Vector.++) word (Vector.singleton steno) m
        mapWordStenos =
            foldl' accFlip Map.empty $ Map.toList mapStenoWord

    runBuildDict mapWordStenos

    putStrLn ""
    -- putStrLn $ "Number of words with steno code: " <> show (length newScores)

    putStrLn "Number of lines in"

    for_ (fileInput : fileOutputJson : lsFiles) $ \file -> do
        exists <- doesFileExist file
        when exists $ do
            nl <- wcl file
            putStrLn $ show nl <> "\t" <> file

    -- TODO: scoring: move elsewhere
    -- nNoParse <- wcl fileDictNoParse
    -- let newZeroScores = replicate nNoParse (0 :: Double)

    --     scores        = newZeroScores <> (fromRational <$> newScores)
    --     meanScore     = average scores
    -- writeFile (fileScores now)
    --     $ Lazy.unlines (Lazy.fromStrict . showt <$> scores)

    -- putStrLn ""
    -- StrictIO.putStrLn $ "Average score: " <> showt meanScore

    putStrLn ""

    stop <- getTime Monotonic
    putStr "StenoWords runtime: "
    fprint (timeSpecs % "\n") start stop
  where
    fileDictNoParse = "buildDict-noparse.txt"

    runBuildDict mapWordStenosExisting = do
        putStr $ "Reading input file " <> fileInput <> " ..."
        hFlush stdout
        ls <- Text.lines <$> Text.readFile fileInput
        let l = length ls
        putStrLn $ l `seq` " done."

        putStrLn $ "Creating steno chords for " <> show l <> " entries."

        let parseSeries' = case lang of
                DE -> parseSeries @DE.Key @DE.Pattern
                EN -> parseSeries @EN.Key @DE.Pattern

        nj <- getNumCapabilities
        putStr $ "\nRunning " <> show nj <> " jobs.\n\n"
        putStr "Optimizing steno chords ..."
        hFlush stdout

        lock <- Lock.new
        mvarLs <- newMVar ls

        let parseWord hyph = do
                let word = Text.replace "|" "" hyph
                    eRaws = Vector.fromList <$!> parseSeries' hyph
                    mExisting = Map.lookup word mapWordStenosExisting

                case (eRaws, mExisting) of
                    (_, Just stenos) -> pure (hyph :!: stenos)
                    (Right stenos, _) -> pure (hyph :!: stenos)
                    (Left (PEExceptionTable orig), _) ->
                        Text.putStrLn ("Error in exception table for: " <> orig)
                            $> (hyph :!: Vector.empty)
                    (Left (PEParsec raw _), _) -> do
                        Lock.with lock $
                            appendLine fileDictNoParse (Text.unwords [word, hyph, showt raw])
                                $> (hyph :!: Vector.empty)

            loop rs = do
                mJob <-
                    modifyMVar mvarLs $
                        pure . \case
                            [] -> ([], Nothing)
                            (j : js) -> (js, Just j)
                case mJob of
                    Just hyph -> do
                        p <- parseWord hyph
                        loop $ p : rs
                    Nothing -> pure rs

        lsStenos <- mconcat <$> replicateConcurrently nj (loop [])

        -- vecStenos <- mconcat <$> forConcurrently jobs (traverse parseWord)
        putStrLn $ lsStenos `seq` " done."

        let --      collision resolution stays sequential
            accDict ::
                forall pg.
                PatternGroup pg =>
                DictState pg ->
                Pair Text (Vector (RawSteno, (pg, Greediness))) ->
                IO (DictState pg)
            accDict dst@DictState{..} (hyph :!: raws) =
                let word = Text.replace "|" "" hyph
                in  if word `Map.member`dstMapWordStenos
                    then appendLine fileDictDuplicates word $> dst
                    else do
                        let (dst', isLost) =
                              Collision.resolve word (toList raws) dst
                        when isLost $
                            appendLine fileCollisions $
                                word <> " " <> Text.intercalate " "
                                        (toList $ showt <$> raws)
                        pure dst'

        putStr "Resolving collisions ..."
        hFlush stdout
        DictState{..} <-
            foldM
                accDict
                (DictState Map.empty Map.empty)
                lsStenos
        putStrLn $ dstMapWordStenos `seq` " done."

        mapFrequencies <- getMapFrequencies "deu_news_2020_freq.txt"

        let
            criterion = Down <<< (\w -> Map.findWithDefault 0 w mapFrequencies)
            sorted =
                sortOn  (criterion <<< snd)
                  $   (Text.encodeUtf8 . showt *** Text.encodeUtf8)
                  <$> Map.toList dstMapStenoWord

        let -- mapStenoWordMin :: forall pg. PatternGroup pg => Map pg (Map Greediness [(Text, RawSteno)])
            mapStenoWordMin :: Map DE.Pattern (Map Greediness [(Text, RawSteno)])
            mapStenoWordMin = Map.foldrWithKey
                ( \w stenos m ->
                    let (_, (raw, (pat, g))) = minimumBy (comparing fst) stenos
                     in Map.insertWith (Map.unionWith (++)) pat (Map.singleton g [(w, raw)]) m
                )
                Map.empty
                dstMapWordStenos

            mapStenoWordTake100
              :: Map DE.Pattern (Map Greediness (Int, [(Text, RawSteno)]))
            mapStenoWordTake100 =
              fmap (fmap (\xs ->
                  ( length xs
                  , take 100 $ sortOn (criterion <<< Text.encodeUtf8 <<< fst) xs
                  )))
                  mapStenoWordMin

        -- checking for lost words
        putStr $ "Writing lost words to " <> fileLost <> " ..."
        hFlush stdout
        u <- for_ ls $ \w ->
            case Map.lookup (Text.replace "|" "" w) dstMapWordStenos of
                Just _ -> pure ()
                Nothing -> appendLine fileLost w
        putStrLn $ u `seq` " done."

        removeFiles
            [ fileOutputJson
            , fileOutputDoc
            ]

        Aeson.encodeFile fileOutputDoc mapStenoWordTake100
            -- foldMap (\(s, w) -> BSB.byteString $ s <> " " <> w <> "\n") sortedMin
        writeJSONFile fileOutputJson sorted

        nLO <- wcl fileOutputJson
        Text.putStrLn $
            "Written "
                <> Text.pack fileOutputJson
                <> " ( "
                <> showt nLO
                <> " lines)"

-- TODO scoring stats
-- pure $ Map.toList stMapWordStenos <&> \(_, sds) ->
--     maximum $ scorePrimary . sdScore <$> sds
