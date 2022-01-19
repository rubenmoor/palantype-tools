{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module BuildDict where

import Args (OptionsBuildDict (..))
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
import Control.Monad (
    foldM,
    when, unless
 )
import qualified Data.Aeson as Aeson
import Data.Foldable (
    Foldable (foldl', length),
    for_,
    minimumBy,
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
    fromMaybe,
 )
import Data.Monoid (
    (<>), Monoid (mconcat)
 )
import Data.Ord (Down (Down), comparing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Tuple (fst, snd)
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
    realToFrac
 )

import Palantype.Common (Greediness, Lang (DE, EN), Palantype (PatternGroup), RawSteno, MapStenoWordTake100)
import qualified Palantype.DE.Keys as DE
import qualified Palantype.EN.Keys as EN
import Palantype.Tools.Collision (DictState (..))
import qualified Palantype.Tools.Collision as Collision
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
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Traversable (Traversable (traverse))

fileLost :: FilePath
fileLost = "buildDict-lostwords.txt"

fileCollisions :: FilePath
fileCollisions = "buildDict-collisions.txt"

fileDuplicates :: FilePath
fileDuplicates = "buildDict-duplicates.txt"

average :: forall a t. (Real a, Foldable t) => t a -> Double
average ls =
    let (t, n) = foldl' (\(!b, !c) a -> (a + b, (c :: Int) + 1)) (0, 0) ls
     in realToFrac t / realToFrac n

buildDict :: OptionsBuildDict -> IO ()
buildDict
  ( OBDFile lsFileInput
            fileOutputPlover
            fileOutputPloverMin
            fileOutputDoc
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
                [ fileLost
                , fileCollisions
                , fileDuplicates
                , fileOutputDoc
                , fileOutputPlover
                , fileOutputPloverMin
                ]
        removeFiles lsFiles

        putStrLn "Reading data from input files: "
        for_ lsFileInput putStrLn
        hFlush stdout
        (ls :: [(Text, [(RawSteno, (PatternGroup key, Greediness))])]) <-
            mconcat <$> traverse
              (     fmap (fromMaybe $ error "Could not decode file.")
                <<< Aeson.decodeFileStrict'
              ) lsFileInput
        putStrLn $ ls `seq` ", " <> show (length ls) <> "entries, done."

        mapFrequencies <- getMapFrequencies "deu_news_2020_freq.txt"
        let
            criterion = Down <<< (\w -> Map.findWithDefault 0 w mapFrequencies)
            lsSorted = sortOn (criterion <<< Text.encodeUtf8 <<< fst) ls

        -- TODO: scoring: move elsewhere
        -- nNoParse <- wcl fileDictNoParse
        -- let newZeroScores = replicate nNoParse (0 :: Double)

        --     scores        = newZeroScores <> (fromRational <$> newScores)
        --     meanScore     = average scores
        -- writeFile (fileScores now)
        --     $ Lazy.unlines (Lazy.fromStrict . showt <$> scores)

        -- putStrLn ""
        -- StrictIO.putStrLn $ "Average score: " <> showt meanScore

        let
            accDict ::
                DictState key ->
                (Text, [(RawSteno, (PatternGroup key, Greediness))]) ->
                IO (DictState key)
            accDict dst@DictState{..} (hyph, raws) =
                let word = Text.replace "|" "" hyph
                in  if word `Map.member` dstMapWordStenos
                    then appendLine fileDuplicates word $> dst
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
                lsSorted
        putStrLn $ dstMapWordStenos `seq` " done."

        let
            sorted =
                sortOn  (criterion <<< snd)
                  $   (Text.encodeUtf8 . showt *** Text.encodeUtf8)
                  <$> Map.toList dstMapStenoWord

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
        u <- for_ ls $ \(w, _) ->
            unless (Text.replace "|" "" w `Map.member` dstMapWordStenos) $
                appendLine fileLost w
        putStrLn $ u `seq` " done."

        putStr $ "Writing file " <> fileOutputDoc <> " ..."
        hFlush stdout
        uDoc <- LBS.writeFile fileOutputDoc $ Aeson.encodePretty mapStenoWordTake100
        putStrLn $ uDoc `seq` " done."

        writeJSONFile fileOutputPlover sorted
        LBS.writeFile fileOutputPloverMin $ Aeson.encodePretty mapStenoWordMin

        putStrLn ""
        putStrLn "Number of lines in"

        for_ lsFiles $ \file -> do
            exists <- doesFileExist file
            when exists $ do
                nl <- wcl file
                putStrLn $ show nl <> "\t" <> file

        stop <- getTime Monotonic
        putStr "StenoWords runtime: "
        fprint (timeSpecs % "\n") start stop


-- TODO scoring stats
-- pure $ Map.toList stMapWordStenos <&> \(_, sds) ->
--     maximum $ scorePrimary . sdScore <$> sds
