{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Control.Concurrent (getNumCapabilities, modifyMVar, newMVar, threadDelay, forkIO, readMVar, modifyMVar_)
import Control.Concurrent.Async (replicateConcurrently)
import qualified Control.Concurrent.Lock as Lock
import Control.Monad (
    foldM,
    when, unless
 )
import qualified Data.Aeson as Aeson
import Data.Either (Either (..))
import Data.Foldable (
    Foldable (foldl', length, null),
    for_,
    minimumBy,
    traverse_,
 )
import Data.Function (($))
import Data.Functor (
    ($>),
    (<$>), Functor (fmap, (<$)), void
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
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.Tuple (fst, snd)
import Formatting (
    fprint,
    (%),
 )
import Formatting.Clock (timeSpecs)
import GHC.Err (error)
import GHC.Exts (seq)
import GHC.Float (Double)
import GHC.Num ((+), Num ((*), (-)))
import GHC.Real (
    Fractional ((/)),
    Real,
    realToFrac,
 )

import Palantype.Common (Greediness, Lang (DE, EN), Palantype (PatternGroup), RawSteno, MapStenoWordTake100, triePrimitives)
import qualified Palantype.DE.Keys as DE
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
    stdout
 )
import Text.Show (Show (show))
import TextShow (TextShow (showt))
import WCL (wcl)
import System.Console.ANSI (setCursorColumn)
import qualified Data.Text.IO as StrictIO
import Control.DeepSeq (deepseq)
import System.Signal (installHandler, sigINT)
import Control.Exception (throw, AsyncException (UserInterrupt))
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as LBS
import System.Exit (exitSuccess)

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
buildDict (OStDArg lang str) =
  case lang of
    DE -> parseSeries' @DE.Key
    EN -> parseSeries' @EN.Key
  where
    parseSeries'
      :: forall key
      . Palantype key
      => IO ()
    parseSeries' =
      case parseSeries @key triePrimitives str of
            Left err -> Text.putStrLn $ showt err
            Right sds -> traverse_ (Text.putStrLn <<< showt) sds
buildDict
  ( OStDFile fileInput
             fileOutputPlover
             fileOutputPloverMin
             fileOutputJson
             fileOutputDoc
             bAppend
             lang
  ) =
  case lang of
    DE -> buildDict' @DE.Key
    EN -> buildDict' @EN.Key
  where
    buildDict' :: forall key. Palantype key => IO ()
    buildDict' = do
        start <- getTime Monotonic

        let lsFiles =
                [ fileDictNoParse
                , fileDictDuplicates
                , fileLost
                , fileCollisions
                , fileOutputDoc
                , fileOutputPlover
                , fileOutputPloverMin
                ]
        removeFiles lsFiles

        -- initial state with existing steno in output file
        -- (mapStenoWord :: Map (Pair RawSteno (Pair (PatternGroup key) Greediness)) Text)
        (mapWordStenos :: Map Text [(Int, (RawSteno, (PatternGroup key, Greediness)))])
          <-
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

        removeFiles [fileOutputJson]

        runBuildDict $ fmap snd . sortOn fst <$> mapWordStenos

        putStrLn ""
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

        runBuildDict
          -- :: Map Text (Vector (Pair RawSteno (Pair (PatternGroup key) Greediness)))
          :: Map Text [(RawSteno, (PatternGroup key, Greediness))]
          -> IO ()
        runBuildDict mapWordStenosExisting = do
            putStr $ "Reading input file " <> fileInput <> " ..."
            hFlush stdout
            ls <- Text.lines <$> Text.readFile fileInput
            let l = length ls
            putStrLn $ l `seq` " done."

            putStrLn $ "Creating steno chords for " <> show l <> " entries."

            nj <- getNumCapabilities
            putStr $ "\nRunning " <> show nj <> " jobs.\n\n"
            putStr "Optimizing steno chords ..."
            hFlush stdout

            lock <- Lock.new
            mvarLs <- newMVar (ls, 1 :: Int)
            mvarLastI <- newMVar (0 :: Int)

            let
                -- !trie = triePrimitives
                parseWord hyph =
                  let word = Text.replace "|" "" hyph
                  in  case Map.lookup word mapWordStenosExisting of
                        Just lsSteno ->
                          pure ( hyph
                               , lsSteno)
                        Nothing -> case parseSeries @key triePrimitives hyph of
                          Right stenos -> pure (hyph, stenos)
                          Left pe      -> (hyph, []) <$ case pe of
                            PEExceptionTable orig -> Text.putStrLn $
                              "Error in exception table for: " <> orig
                            PEParsec raw _ -> Lock.with lock $
                              appendLine fileDictNoParse $
                                Text.unwords [word, hyph, showt raw]
                            PEImpossible str -> do
                              Text.putStrLn $ "Seemingly impossible: " <> str
                              appendLine fileDictNoParse $ Text.unwords [word, hyph]

                loop rs = do
                    mJob <-
                        modifyMVar mvarLs $ \(ls', i) -> do
                            pure $ case ls' of
                                [] -> (([], i), Nothing)
                                (j : js) -> ((js, i + 1), Just j)
                    case mJob of
                        Just hyph -> do
                            p <- parseWord hyph
                            p `deepseq` loop (p : rs)
                        Nothing -> pure rs

            let minuteReport = do
                  threadDelay (1000 * 1000 * 60)
                  (ls', newI) <- readMVar mvarLs
                  unless (null ls') $ do
                    delta <- modifyMVar mvarLastI $ \i -> do
                                  setCursorColumn 28
                                  StrictIO.putStr $ showt newI
                                  pure (newI, newI - i)
                    StrictIO.putStr $ ", " <> showt delta <> " w/m   "
                    hFlush stdout
                    minuteReport

            installHandler sigINT $ \_ -> do
                putStrLn ""
                putStrLn "Terminating ..."
                modifyMVar_ mvarLs $ \_ -> pure ([], 0)
                installHandler sigINT $ \_ -> throw UserInterrupt

            _ <- forkIO minuteReport

            lsStenos <- mconcat <$> replicateConcurrently nj (loop [])

            -- vecStenos <- mconcat <$> forConcurrently jobs (traverse parseWord)
            setCursorColumn 28
            putStrLn $ lsStenos `seq` " done.                 "

            void exitSuccess

            let --      collision resolution stays sequential
                accDict ::
                    DictState key ->
                    (Text, [(RawSteno, (PatternGroup key, Greediness))]) ->
                    IO (DictState key)
                accDict dst@DictState{..} (hyph, raws) =
                    let word = Text.replace "|" "" hyph
                    in  if word `Map.member` dstMapWordStenos
                        then appendLine fileDictDuplicates word $> dst
                        else do
                            let (dst', isLost) =
                                  Collision.resolve word raws dst
                            when isLost $
                                appendLine fileCollisions $
                                    word <> " " <> Text.intercalate " " (showt <$> raws)
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

            let
                mapStenoWordDoc :: Map (PatternGroup key) (Map Greediness [(Text, RawSteno)])
                mapStenoWordDoc = Map.foldrWithKey
                    ( \w stenos m ->
                        let (_, (raw, (pat, g))) = minimumBy (comparing fst) stenos
                         in Map.insertWith (Map.unionWith (++)) pat (Map.singleton g [(w, raw)]) m
                    )
                    Map.empty
                    dstMapWordStenos

                mapStenoWordTake100 :: MapStenoWordTake100 key
                mapStenoWordTake100 =
                  fmap (fmap (\xs ->
                      ( length xs
                      , take 100 $ sortOn (criterion <<< Text.encodeUtf8 <<< fst) xs
                      )))
                      mapStenoWordDoc

                mapStenoWordMin :: Map Text RawSteno
                mapStenoWordMin = Map.foldrWithKey
                  ( \w stenos m ->
                      let (_, (raw, _)) = minimumBy (comparing fst) stenos
                      in  Map.insert w raw m
                  ) Map.empty dstMapWordStenos

            -- checking for lost words
            putStr $ "Writing lost words to " <> fileLost <> " ..."
            hFlush stdout
            u <- for_ ls $ \w ->
                unless (Text.replace "|" "" w `Map.member` dstMapWordStenos) $
                    appendLine fileLost w
            putStrLn $ u `seq` " done."

            putStr $ "Writing file " <> fileOutputDoc <> " ..."
            hFlush stdout
            uDoc <- LBS.writeFile fileOutputDoc $ Aeson.encodePretty mapStenoWordTake100
            putStrLn $ uDoc `seq` " done."
                -- foldMap (\(s, w) -> BSB.byteString $ s <> " " <> w <> "\n") sortedMin
            writeJSONFile fileOutputPlover sorted
            LBS.writeFile fileOutputPloverMin $ Aeson.encodePretty mapStenoWordMin

            LBS.writeFile fileOutputJson $ Aeson.encodePretty dstMapWordStenos

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
