{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MakeSteno where

import Args (OptionsMakeSteno (OMkStFile, OMkStArg))
import Common (
    appendLine,
    removeFiles,
 )
import Control.Applicative (Applicative (pure))
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import qualified Control.Concurrent.Lock as Lock
import Control.Monad (
    when, (<=<)
 )
import Data.Either (Either (..))
import Data.Foldable (
    Foldable (length, foldl'),
    for_, traverse_
 )
import Data.Function (($))
import Data.Functor (
    (<$>), Functor ((<$), fmap)
 )
import Data.List (transpose, replicate, last)
import Data.Monoid (
    mconcat,
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
import GHC.Num (Num ((*)))

import Palantype.Common (Greediness, Lang (DE, EN), Palantype (PatternGroup, patSimpleMulti), RawSteno (RawSteno), triePrimitives)
import qualified Palantype.DE.Keys as DE
import qualified Palantype.EN.Keys as EN
import Palantype.Tools.Steno (
    ParseError (..),
    parseSeries,
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
import Data.List.Split (chunksOf)
import Data.Eq (Eq((==)))
import Control.Category ((<<<), Category ((.)))
import Control.DeepSeq (deepseq, NFData, force)
import Palantype.DE (Pattern(PatSimple))
import Data.Traversable (Traversable(traverse))
import Control.Exception (evaluate)
-- import Control.Scheduler (traverseConcurrently, Comp (ParN, ParOn))

fileNoParse :: FilePath
fileNoParse = "makeSteno-noparse.txt"

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
              fileOutputJson
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
              , fileOutputJson
              ]
        removeFiles lsFiles

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

        let
            parseWord :: Text -> IO (Text, [(RawSteno, (PatternGroup key, Greediness))])
            parseWord hyph = case parseSeries triePrimitives hyph of
                    Right stenos -> pure (hyph, stenos)
                    Left pe      -> (hyph, []) <$ case pe of
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
              where
                word = Text.replace "|" "" hyph

            parseWordDummy :: Text -> IO (Text, [(RawSteno, (PatternGroup key, Greediness))])
            parseWordDummy hyph = do
              let lsFoo = replicate 600 hyph
              pure (Text.takeEnd 100 $ foldl' (\strRes str -> Text.head str `Text.cons` strRes) "" lsFoo, [])

            traverseDeep :: NFData b => (a -> IO b) -> [a] -> IO [b]
            traverseDeep f = traverse (evaluate . force <=< f)

        beginParallel <- getTime Monotonic
        lsStenos <- if nj == 1
          then traverseDeep parseWordDummy ls
          else mconcat <$> mapConcurrently (traverseDeep parseWord)
                                           (transpose $ chunksOf (10 * nj) ls)
        endParallel <- getTime Monotonic
        putStr "Parallel runtime: "
        fprint (timeSpecs % "\n") beginParallel endParallel
            -- stopLls <- lls `deepseq` getTime Monotonic
            -- putStr "Runtime till lls: "
            -- fprint (timeSpecs % "\n") start stopLls
            -- let lsFlat = mconcat lls

            -- stopLsFlat <- lsFlat `deepseq` getTime Monotonic
            -- putStr "Runtime till lsFlat: "
            -- fprint (timeSpecs % "\n") start stopLsFlat
            -- pure lsFlat

        setCursorColumn 28
        putStrLn $ lsStenos `deepseq` "done.                 "

        putStr "Saving result ..."
        hFlush stdout
        oj <- LBS.writeFile fileOutputJson $ Aeson.encodePretty lsStenos
        putStrLn $ oj `seq` " done."

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
