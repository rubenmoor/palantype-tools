{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module MakeSteno where

import Args (OptionsMakeSteno (OMkStFile, OMkStArg))
import Common (
    appendLine,
    removeFiles, writeJSONFile
 )
import Control.Applicative (Applicative (pure))
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.MVar
    ( readMVar, modifyMVar_, modifyMVar, newMVar )
import Control.Concurrent.Async (replicateConcurrently_)
import qualified Control.Concurrent.Lock as Lock
import Control.Monad (
    when, Monad ((>>))
 )
import Data.Either (Either (..), isRight)
import Data.Foldable (
    Foldable (length),
    for_, traverse_
 )
import Data.Function (($))
import Data.Functor (
    (<$>), Functor (fmap)
 )
import Data.List (sortOn, minimumBy, take)
import Data.Monoid (
    (<>),
 )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Formatting (
    fprint,
    (%),
 )
import Formatting.Clock (timeSpecs)
import GHC.Exts (seq)

import Palantype.Common (Greediness, Lang (DE, EN), Palantype (PatternGroup), triePrimitives, MapStenoWordTake100, RawSteno)
import qualified Palantype.DE.Keys as DE
import qualified Palantype.EN.Keys as EN
import Palantype.Tools.Steno (
    ParseError (..),
    parseSeries, acronym, isCapitalized
 )
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
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Eq (Eq((==)))
import Control.Category ((<<<), Category ((.)))
import qualified Data.Map.Strict as Map
import Palantype.Tools.Collision (DictState(DictState, dstMapWordStenos))
import qualified Palantype.Tools.Collision as Collision
import qualified Data.Text.Encoding as Text
import Data.Map.Strict (Map)
import Data.Tuple (snd, fst)
import Sort (getMapFrequencies)
import Data.Ord (Down(Down), comparing)
import Control.Arrow (Arrow((***)))
import Data.Maybe (Maybe(Nothing, Just))
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import qualified Data.Set as Set
import Data.Set ((\\))
import Data.Bool (Bool(True, False), not, (&&))
import Text.Parsec (runParser)
-- import Control.Scheduler (traverseConcurrently, Comp (ParN, ParOn))

fileNoParse :: FilePath
fileNoParse = "makeSteno-noparse.txt"

fileLost :: FilePath
fileLost = "makeSteno-lostwords.txt"

fileCollisions :: FilePath
fileCollisions = "makeSteno-collisions.txt"

fileDuplicates :: FilePath
fileDuplicates = "makeSteno-duplicates.txt"

makeSteno :: OptionsMakeSteno -> IO ()
makeSteno ( OMkStArg lang str ) =
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
makeSteno
  ( OMkStFile fileInput
              fileOutputPlover
              fileOutputPloverMin
              fileOutputDoc
              lang
  ) =
  case lang of
    DE -> makeSteno' @DE.Key
    EN -> makeSteno' @EN.Key
  where
    makeSteno' :: forall key. Palantype key => IO ()
    makeSteno' = do
        start <- getTime Monotonic

        let lsFiles =
              [ fileNoParse
              , fileOutputPlover
              , fileOutputPloverMin
              , fileOutputDoc
              , fileCollisions
              , fileDuplicates
              , fileLost
              ]
        removeFiles lsFiles

        putStr $ "Reading input file " <> fileInput <> " ..."
        hFlush stdout
        ls <- Text.lines <$> Text.readFile fileInput

        let
            l = length ls
            setLs = Set.fromList ls

        putStrLn $ l `seq` " done."

        putStrLn $ "Creating steno chords for " <> show l <> " entries."

        nj <- getNumCapabilities
        putStr $ "\nRunning " <> show nj <> " jobs.\n\n"
        putStr "Optimizing steno chords ..."
        hFlush stdout

        lock <- Lock.new

        dictState <- newMVar $ DictState Map.empty Map.empty
        mvarLs <- newMVar ls

        let
            isAcronym = isRight <<< runParser acronym () ""

            parseWord :: Text -> IO ()
            parseWord hyph = do
              mapWordStenos <- dstMapWordStenos <$> readMVar dictState

              let
                  isDupl = word `Map.member` mapWordStenos
                  isCapl = isCapitalized hyph && not (isAcronym hyph)
                  isCaplDupl = isCapl && Text.toLower hyph `Set.member` setLs
                  word = Text.replace "|" "" hyph

              case (isDupl, isCaplDupl) of
                  (True , _    ) -> appendLine fileDuplicates word
                  (False, True ) -> appendLine fileDuplicates $ word <> " capitalized"
                  (False, False) -> case parseSeries @key triePrimitives hyph of
                      Right stenos -> modifyMVar_ dictState \dst -> do
                          let (dst', isLost) = Collision.resolve word (force stenos) dst
                          _ <- evaluate dst'
                          when isLost $
                              appendLine fileCollisions $
                                  word <> " "
                                       <> Text.intercalate " " (showt <$> stenos)
                          pure dst'
                      Left pe      -> case pe of
                        PEExceptionTable orig -> Text.putStrLn $
                          "Error in exception table for: " <> orig
                        PEParsec raw _ ->
                          Lock.with lock $
                            appendLine fileNoParse $
                              Text.unwords [word, hyph, showt raw]
                        PEImpossible str -> do
                          Text.putStrLn $ "Seemingly impossible: " <> str
                          Lock.with lock $
                            appendLine fileNoParse
                              $ Text.unwords [word, hyph]

            loop = do
                mJob <- modifyMVar mvarLs \ls' ->
                    pure $ case ls' of
                        [] -> ([], Nothing)
                        (j : js) -> (js, Just j)
                case mJob of
                    Just hyph -> parseWord hyph >> loop
                    Nothing -> pure ()

        if nj == 1
          then traverse_ parseWord ls
          else replicateConcurrently_ nj loop

        setCursorColumn 28
        putStrLn "done.                 "

        DictState {..} <- readMVar dictState

        mapFrequencies <- getMapFrequencies "deu_news_2020_freq.txt"

        let
            criterion = Down <<< (\w -> Map.findWithDefault 0 w mapFrequencies)
            sorted =
                sortOn  (criterion <<< snd)
                  $   (Text.encodeUtf8 . showt *** Text.encodeUtf8)
                  <$> Map.toList dstMapStenoWord

            mapStenoWordDoc :: Map (PatternGroup key) (Map Greediness [(Text, RawSteno)])
            mapStenoWordDoc = Map.foldrWithKey
                ( \w stenos m ->
                    let (_, (raw, (g, pat))) = minimumBy (comparing fst) stenos
                     in Map.insertWith (Map.unionWith (<>)) pat (Map.singleton g [(w, raw)]) m
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
        traverse_ (appendLine fileLost) $
            Set.map (Text.replace "|" "") setLs \\ Map.keysSet dstMapWordStenos
        putStrLn " done."

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

        putStrLn ""

        stop <- getTime Monotonic
        putStr "StenoWords runtime: "
        fprint (timeSpecs % "\n") start stop
