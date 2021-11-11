{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

module Main where

import           Args                       (OptionsStenoWords (..),
                                             OptionsStenoWordsRun (..),
                                             OptionsSyllables (OSylArg, OSylFile),
                                             Task (..), argOpts)
import           Control.Applicative        (Applicative (pure))
import           Control.Category           (Category ((.)))
import           Control.Monad              (Monad ((>>=)), foldM, foldM_,
                                             forever, unless, when)
import           Data.Bool                  (Bool (..))
import           Data.Either                (Either (..))
import           Data.Eq                    (Eq ((==)))
import           Data.Foldable              (Foldable (foldl', length, maximum),
                                             for_)
import           Data.Function              (($))
import           Data.Functor               (Functor (fmap), (<$>), (<&>))
import           Data.HashMap.Strict        (HashMap, member)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Int                   (Int)
import           Data.List                  (head, replicate, (++), take, sortOn, zip)
import qualified Data.Map as Map
import           Data.Maybe                 (Maybe (Nothing, Just))
import           Data.Monoid                ((<>), Monoid (mempty, mconcat))
import           Data.Text                  (Text, replace, splitOn, toLower)
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as StrictIO
import qualified Data.Text.Lazy             as Lazy
import           Data.Text.Lazy.IO          (appendFile, getLine, hGetContents,
                                             readFile, writeFile, hPutStrLn)
import           Data.Text.Lazy.Read        (double)
import           Data.Time.Clock            (UTCTime, getCurrentTime)
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import           Formatting                 (fprint, (%))
import           Formatting.Clock           (timeSpecs)
import           GHC.Err                    (error)
import           GHC.Float                  (Double)
import           GHC.Num                    ((+))
import           GHC.Real                   (Fractional ((/), fromRational), Real, realToFrac, fromIntegral)
import           Options.Applicative        (execParser)
import           Palantype.Common.RawSteno  (RawSteno (RawSteno))
import qualified Palantype.Common.RawSteno  as RawSteno
import qualified Palantype.DE.Keys          as DE
import           Palantype.Tools.Statistics (plotScoresShow)
import           Palantype.Tools.Steno      (ParseError (..), SeriesData (..),
                                             parseSeries, PartsData (..), Score (scorePrimary), Path)
import           Palantype.Tools.Syllables  (Exception (..), Result (..),
                                             SyllableData (SyllableData),
                                             parseSyllables, sdWord)
import           System.Clock               (Clock (Monotonic), getTime)
import           System.Directory           (doesFileExist, listDirectory,
                                             removeFile, renamePath)
import           System.FilePath            (takeDirectory, (</>))
import           System.IO                  (FilePath, IO, IOMode (ReadMode, ReadWriteMode),
                                             hSetNewlineMode, openFile, print,
                                             putStr, putStrLn,
                                             universalNewlineMode, Handle, SeekMode (AbsoluteSeek), hSeek)
import           Text.Show                  (Show (show))
import           TextShow                   (TextShow (showt))
import           WCL                        (wcl)
import Data.Ord (Ord((>)))
import Data.Tuple (snd, fst)
import GHC.Conc (getNumCapabilities)
import Prelude (Semigroup, RealFrac (ceiling), Applicative ((*>)))
import Control.Concurrent.Async (forConcurrently)
import Data.List.Split (chunksOf)
import Data.Traversable (forM)
import qualified Data.Aeson as Aeson
import Palantype.Tools.Collision (readFrequencies, lookUpFrequency, getSession)

main :: IO ()
main =
  execParser argOpts >>= \case
    TaskRawSteno   str   -> rawSteno   str
    TaskSyllables  opts  -> syllables  opts
    TaskStenoWords opts  -> stenoWords opts
    TaskGermanDict  _ -> pure ()

fileSyllables :: FilePath
fileSyllables = "syllables.txt"

rawSteno
  :: Text
  -> IO ()
rawSteno str =
  StrictIO.putStrLn $ showt $ RawSteno.parseSteno @DE.Key (RawSteno str)

syllables
  :: OptionsSyllables
  -> IO ()
syllables (OSylArg str) = do
  cache <- readFrequencies
  session <- getSession
  for_ (parseSyllables str) $ \case
    Success sd -> do
      f <- lookUpFrequency cache session (sdWord sd)
      StrictIO.putStrLn $ showt sd <> " " <> showt f
    other -> StrictIO.putStrLn $ showt other

syllables (OSylFile reset) = do
    start <- getTime Monotonic
    let lsFiles =
          [ fileSyllables
          , fileSyllablesNoParse
          , fileSyllablesAbbreviations
          , fileSyllablesMultiple
          , fileSyllablesSpecialChar
          , fileSyllablesSingleLetter
          , fileSyllablesEllipsis
          , fileSyllablesAcronyms
          ]
    when reset $ do
      for_ lsFiles $ \file -> do
        exists <- doesFileExist file
        when exists $ do
          putStrLn $ "Deleting " <> file
          removeFile file

    existsFileNoParse <- doesFileExist fileSyllablesNoParse

    runSyllables $
      if existsFileNoParse
        then fileSyllablesNoParse
        else "entries.txt"

    putStrLn ""
    putStrLn "Number of lines in"

    for_ ("entries.txt":lsFiles) $ \file -> do
      nl <- wcl file
      putStrLn $ show nl <> "\t" <> file

    putStrLn ""
    stop <- getTime Monotonic
    putStr "Syllables runtime: "
    fprint (timeSpecs % "\n") start stop

  where
    fileSyllablesNoParse = "syllables-noparse.txt"
    fileSyllablesAbbreviations = "syllables-abbreviations.txt"
    fileSyllablesMultiple = "syllables-multiple.txt"
    fileSyllablesSpecialChar = "syllables-specialchar.txt"
    fileSyllablesSingleLetter = "syllables-singleletter.txt"
    fileSyllablesEllipsis = "syllables-ellipsis.txt"
    fileSyllablesAcronyms = "syllables-acronyms.txt"

    runSyllables file = do

      let tmpFileNoParse = "syllables-noparse.tmp.txt"

      cache <- readFrequencies
      session <- getSession
      handle <- openFile file ReadMode
      hSetNewlineMode handle universalNewlineMode
      entries <- Lazy.lines <$> hGetContents handle

      let
        acc last entry = do
          -- eliminate duplicate entries after parsing
          let lsHyphenated = parseSyllables $ Lazy.toStrict entry
              hyphenated = head lsHyphenated
          unless (hyphenated == last) $
            for_ lsHyphenated $ \case
              Failure err -> do
                print err
                appendLine tmpFileNoParse entry
              Success sd -> do
                f <- lookUpFrequency cache session (sdWord sd)
                appendLine fileSyllables $ Lazy.fromStrict $
                  showt sd <> " " <> showt f
              Exception exc -> case exc of
                ExceptionAbbreviation  -> appendLine fileSyllablesAbbreviations entry
                ExceptionMultiple      -> appendLine fileSyllablesMultiple entry
                ExceptionSpecialChar c -> appendLine fileSyllablesSpecialChar $ Lazy.singleton c <> " " <> entry
                ExceptionSingleLetter  -> appendLine fileSyllablesSingleLetter entry
                ExceptionEllipsis      -> appendLine fileSyllablesEllipsis entry
                ExceptionAcronym       -> appendLine fileSyllablesAcronyms entry
          pure hyphenated
      foldM_ acc (Success $ SyllableData "" []) entries

      appendLine tmpFileNoParse "END"
      renamePath tmpFileNoParse fileSyllablesNoParse

fileStenoWords :: FilePath
fileStenoWords = "stenowords.txt"

fileStenoParts :: FilePath
fileStenoParts = "stenoparts.txt"

fileStenoWordsScores :: UTCTime -> FilePath
fileStenoWordsScores time =
     "stenowordsstats/stenowords-stats_"
  <> formatTime defaultTimeLocale "%y%m%d-%T" time <> ".txt"

fileStenoWordsDuplicates :: FilePath
fileStenoWordsDuplicates = "stenowords-duplicates.txt"

fileStenoWordsMissingPrimitives :: FilePath
fileStenoWordsMissingPrimitives = "stenowords-missingprimitives.txt"

fileStenoWordsParsecErrors :: FilePath
fileStenoWordsParsecErrors = "stenowords-parsecerrors.txt"

fileStenoWordsCollisions :: FilePath
fileStenoWordsCollisions = "stenowords-collisions.txt"

readDouble
  :: Lazy.Text -> Double
readDouble str =
  case double str of
    Right (d, _) -> d
    Left  err    -> error $ "Could not read double: " <> err

readScores
  :: FilePath
  -> IO [Double]
readScores file = do
  fmap readDouble . Lazy.lines <$> readFile file

average
  :: forall a t.
  ( Real a
  , Foldable t
  )
  => t a
  -> Double
average ls =
  let (t, n) = foldl' (\(!b,!c) a -> (a + b, (c :: Int) + 1)) (0, 0) ls
  in  realToFrac t / realToFrac n

data StenoWordsState
  = StenoWordsState
    { swsMapWordSteno  :: HashMap Text SeriesData
    , swsMapChordParts :: HashMap PartsData [Text]
    , swsMapStenoWords  :: HashMap RawSteno [(Path, Text)]
    , swsMissingPrimitives :: [Lazy.Text]
    , swsParsecError :: [Lazy.Text]
    , swsNoParse :: [Lazy.Text]
    , swsDuplicates :: [Lazy.Text]
    }

instance Semigroup StenoWordsState where
  (StenoWordsState mWordSteno1 mChordParts1 mStenoWord1 prims1 parsecErr1 noparse1 dupls1)
    <> (StenoWordsState mWordSteno2 mChordParts2 mStenoWord2 prims2 parsecErr2 noparse2 dupls2) =
      StenoWordsState (HashMap.union mWordSteno1 mWordSteno2)
                      (HashMap.unionWith (++) mChordParts1 mChordParts2)
                      (HashMap.unionWith (++) mStenoWord1 mStenoWord2)
                      (prims1 <> prims2)
                      (parsecErr1 <> parsecErr2)
                      (noparse1 <> noparse2)
                      (dupls1 <> dupls2)

instance Monoid StenoWordsState where
  mempty = StenoWordsState HashMap.empty HashMap.empty HashMap.empty [] [] [] []

stenoWords :: OptionsStenoWords -> IO ()
stenoWords (OStwRun greediness (OStwArg str)) =
    case parseSeries greediness str of
      Left err -> StrictIO.putStrLn $ showt err
      Right (sd, alts) -> do
        StrictIO.putStr $ showSeriesData sd <> " – "
          <> Text.intercalate "; " (showAlt <$> alts)
  where
    showSeriesData SeriesData{..} =
           sdHyphenated
        <> " " <> showt sdScore
        <> " " <> showt sdPath
        <> " " <> Text.intercalate "/" (showt . snd <$> sdParts)
    showAlt SeriesData {..} =
      showt sdPath <> " " <> Text.intercalate "/" (showt . snd <$> sdParts)

stenoWords (OStwRun greediness OStwStdin) =
  forever $ do
    l <- Lazy.toStrict <$> getLine
    StrictIO.putStrLn $ showt $ parseSeries greediness l

stenoWords (OStwRun greediness
           (OStwFile reset showChart mFileOutput))
  = do

    start <- getTime Monotonic
    now <- getCurrentTime

    let lsFiles =
          [ fileStenoWords
          , fileStenoWordsNoParse
          , fileStenoWordsDuplicates
          , fileStenoWordsMissingPrimitives
          , fileStenoWordsParsecErrors
          , fileStenoParts
          , fileStenoWordsCollisions
          ]
    when reset $ do
      for_ lsFiles $ \file -> do
        exists <- doesFileExist file
        when exists $ do
          putStrLn $ "Deleting " <> file
          removeFile file

    existsStenoWordsNoParse <- doesFileExist fileStenoWordsNoParse
    newScores <- runStenoWords $ if existsStenoWordsNoParse
      then (fileStenoWordsNoParse, False)
      else (fileSyllables, True)

    putStrLn ""
    putStrLn $ "Number of words with steno code: " <> show (length newScores)

    putStrLn "Number of lines in"

    for_ lsFiles $ \file -> do
      exists <- doesFileExist file
      when exists $ do
        nl <- wcl file
        putStrLn $ show nl <> "\t" <> file

    nNoParse <- wcl fileStenoWordsNoParse
    let newZeroScores = replicate nNoParse 0

    let scores = newZeroScores <> (fromRational <$> newScores)
        meanScore = average scores
    writeFile (fileStenoWordsScores now) $ Lazy.unlines $ Lazy.fromStrict . showt <$> scores

    putStrLn ""
    StrictIO.putStrLn $ "Average score: " <> showt meanScore

    putStrLn ""

    stop <- getTime Monotonic
    putStr "StenoWords runtime: "
    fprint (timeSpecs % "\n") start stop

    when showChart $ plotScoresShow scores
  where
    fileStenoWordsNoParse = "stenowords-noparse.txt"

    runStenoWords (file, bFirstPass) = do

      lsSyllable <- Lazy.lines <$> readFile file
      let l = length lsSyllable
      putStrLn $ "Creating steno chords for " <> show l <> " entries."

      let
        acc
          :: Handle
          -> StenoWordsState
          -> Lazy.Text
          -> IO StenoWordsState
        acc h sws@StenoWordsState{..} str =
          let
            (word, hyphenated) =
              case splitOn " " $ Lazy.toStrict str of
                [w, hy] -> (w, hy)
                _      -> error $ Text.unpack $ Lazy.toStrict $ "illegal entry: " <> str
          in
            case parseSeries greediness hyphenated of
              Left err -> case err of
                PEMissingPrimitives orig ->
                  pure $ if bFirstPass
                    then sws { swsMissingPrimitives = Lazy.fromStrict orig : swsMissingPrimitives
                             , swsNoParse = str : swsNoParse
                             }
                    else sws { swsNoParse = str : swsNoParse }
                PEExceptionTable orig -> do
                  print $ "Error in exception table for: " <> orig
                  pure sws
                PEParsec raw pe ->
                  pure $ if bFirstPass
                    then
                      let
                        line = showt word <> " "
                            <> showt hyphenated <> " "
                            <> showt raw <> " "
                            <> replace "\n" "" (Text.pack $ show pe)
                      in
                        sws { swsParsecError = Lazy.fromStrict line : swsParsecError
                            , swsNoParse = str : swsNoParse
                            }
                    else sws { swsNoParse = str : swsNoParse }
              Right (sd, alts)  ->
                if word `member` swsMapWordSteno
                  then pure $
                    if bFirstPass
                      then sws { swsDuplicates = str : swsDuplicates }
                      else sws
                  else do
                    let
                      acc' m (o, c) =
                        HashMap.insertWith (++) (PartsData o c $ sdPath sd) [word] m
                      partsData = foldl' acc' HashMap.empty $
                        mconcat $ sdParts sd : (sdParts <$> alts)

                      raw = RawSteno $ Text.intercalate "/" $ showt . snd <$> sdParts sd

                      ciAppend [p1@(_, w1)] [p2@(_, w2)] =
                        if toLower w1 == toLower w2 then [p1] else [p1, p2]
                      ciAppend ws1 ws2 = ws1 ++ ws2

                      showAlt SeriesData {..} =
                        showt sdPath <> " " <> Text.intercalate "/" (showt . snd <$> sdParts)

                    -- write to file
                    hPutStrLn h $ Lazy.fromStrict $
                      word <> " " <> showt sd
                      <> " – " <> Text.intercalate "; " (showAlt <$> alts)

                    pure $ sws
                      { swsMapWordSteno = HashMap.insert word sd swsMapWordSteno
                      , swsMapChordParts = HashMap.unionWith (++) partsData swsMapChordParts
                      , swsMapStenoWords = HashMap.insertWith ciAppend raw [(sdPath sd, word)] swsMapStenoWords
                      }

      nj <- getNumCapabilities
      putStrLn ""
      putStrLn $ "Running " <> show nj <> " jobs."
      putStrLn ""

      let d = ceiling ((fromIntegral l :: Double) / fromIntegral nj)
          jobs = zip [1 :: Int ..] $ chunksOf d lsSyllable
      lsResult <- forConcurrently  jobs $ \(i, ls) -> do
        let filePart = "stenoWordsPart" <> show i <> ".txt"
        exists <- doesFileExist filePart
        when exists $ removeFile filePart
        h <- openFile filePart ReadWriteMode
        sws <- foldM (acc h) mempty ls
        pure (h, sws)

      let lsSws = snd <$> lsResult
          handles = fst <$> lsResult
          StenoWordsState{..} = mconcat lsSws

      writeFile fileStenoWordsMissingPrimitives $ Lazy.unlines swsMissingPrimitives
      writeFile fileStenoWordsParsecErrors $ Lazy.unlines swsParsecError
      writeFile fileStenoWordsNoParse $ Lazy.unlines swsNoParse
      writeFile fileStenoWordsDuplicates $ Lazy.unlines swsDuplicates

      putStrLn $ "Writing file " <> fileStenoWords
      let hReadFile h = hSeek h AbsoluteSeek 0 *> hGetContents h
      content <- foldM (\c h -> (c <>) <$> hReadFile h) "" handles
      writeFile fileStenoWords content

      case mFileOutput of
        Just fileOutput -> do
          putStrLn $ "Writing " <> fileOutput

          for_ (HashMap.toList swsMapWordSteno) $ \(word, sd) -> do
            let str = Lazy.fromStrict $ word <> " " <> showt sd
            appendLine fileOutput str
        Nothing -> pure ()

      putStrLn $ "Writing file " <> fileStenoParts
      for_ (sortOn (length . snd) $ HashMap.toList swsMapChordParts) $ \(pd, words) -> do
        let
          n = length words
          str = Lazy.fromStrict $
                showt pd <> "; "
             <> showt n <> (if n > 1 then " words: " else " word: ")
             <> Text.unwords (take 3 words)
             <> if n > 3 then " ..." else ""
        appendLine fileStenoParts str

      -- TODO: replace with collision detection from Palantype.Tools.Collision
      let
        acc' m (r, pairs) = case pairs of

          -- no collision: ignore path, store raw steno with word
          -- TODO: unless there is already an entry
          [(_, word)] -> pure $ HashMap.insert r word m

          -- collision
          p:_ -> do
            appendLine fileStenoWordsCollisions $
              Lazy.fromStrict $ showt r <> ": " <> showt pairs
            pure $ HashMap.insert r (snd p) m

          -- impossible case
          []     -> error "empty entry"

      mapStenoWord <-
        foldM acc' HashMap.empty (HashMap.toList swsMapStenoWords)

      -- TODO: aeson object instead of list, with line breaks
      putStrLn "Writing small json dictionary"
      Aeson.encodeFile "smalldict.json" mapStenoWord

      pure $ HashMap.toList swsMapWordSteno <&> \(_, SeriesData {..}) ->
        scorePrimary sdScore

stenoWords OStwShowChart = do
  now <- getCurrentTime
  let dir = takeDirectory $ fileStenoWordsScores now
  files <- listDirectory dir
  let latest = maximum files
  scores <- readScores $ dir </> latest
  plotScoresShow scores

filePartsDict :: FilePath
filePartsDict = "partsdict.txt"

appendLine
  :: FilePath
  -> Lazy.Text
  -> IO ()
appendLine file str =
  appendFile file $ str <> "\n"
