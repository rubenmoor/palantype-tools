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
import           Data.Either                (Either (..), either)
import           Data.Eq                    (Eq ((==)))
import           Data.Foldable              (Foldable (foldl', length, maximum),
                                             for_)
import           Data.Function              (const, ($))
import           Data.Functor               (Functor (fmap), ($>), (<$>), (<&>))
import           Data.HashMap.Strict        (HashMap, member)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Int                   (Int)
import           Data.List                  (head, replicate, sort, (!!), (++))
import           Data.Maybe                 (Maybe (Just, Nothing), maybe)
import           Data.Monoid                ((<>))
import           Data.Ord                   (Ord (compare), comparing)
import           Data.Text                  (Text, replace, splitOn)
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as StrictIO
import qualified Data.Text.Lazy             as Lazy
import           Data.Text.Lazy.IO          (appendFile, getLine, hGetContents,
                                             readFile, writeFile)
import           Data.Text.Lazy.Read        (double)
import           Data.Time.Clock            (UTCTime, getCurrentTime)
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import           Formatting                 (fprint, (%))
import           Formatting.Clock           (timeSpecs)
import           GHC.Err                    (error)
import           GHC.Float                  (Double)
import           GHC.Num                    ((+))
import           GHC.Real                   (Fractional ((/)), Real, realToFrac)
import           Options.Applicative        (execParser)
import           Palantype.Common           (Chord)
import           Palantype.Common.RawSteno  (RawSteno (RawSteno))
import qualified Palantype.Common.RawSteno  as RawSteno
import qualified Palantype.DE.Keys          as DE
import           Palantype.Tools.Statistics (plotScoresShow)
import           Palantype.Tools.Steno      (ParseError (..), Path, SeriesData (..),
                                             parseSeries)
import           Palantype.Tools.Syllables  (Exception (..), Result (..),
                                             SyllableData (SyllableData),
                                             parseSyllables)
import           System.Clock               (Clock (Monotonic), getTime)
import           System.Directory           (doesFileExist, listDirectory,
                                             removeFile, renamePath)
import           System.FilePath            (takeDirectory, (</>))
import           System.IO                  (FilePath, IO, IOMode (ReadMode),
                                             hSetNewlineMode, openFile, print,
                                             putStr, putStrLn,
                                             universalNewlineMode)
import           Text.Read                  (readMaybe)
import           Text.Show                  (Show (show))
import           TextShow                   (TextShow (showb, showt))
import           WCL                        (wcl)

main :: IO ()
main =
  execParser argOpts >>= \case
    TaskRawSteno   str   -> rawSteno   str
    TaskSyllables  opts  -> syllables  opts
    TaskStenoWords opts  -> stenoWords opts
    TaskPartsDict  reset -> partsDict  reset

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
syllables (OSylArg str) =
  StrictIO.putStrLn $ showt $ parseSyllables str

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

    runSyllables file = do

      let tmpFileNoParse = "syllables-noparse.tmp.txt"

      handle <- openFile file ReadMode
      hSetNewlineMode handle universalNewlineMode
      entries <- Lazy.lines <$> hGetContents handle

      let acc last entry = do
                -- eliminate duplicate entries after parsing
                let lsHyphenated = parseSyllables $ Lazy.toStrict entry
                    hyphenated = head lsHyphenated
                unless (hyphenated == last) $
                  for_ lsHyphenated $ \case
                    Failure err -> do
                      print err
                      appendLine tmpFileNoParse entry
                    Success sd -> appendLine fileSyllables $ Lazy.fromStrict $ showt sd
                    Exception exc -> case exc of
                      ExceptionAbbreviation  -> appendLine fileSyllablesAbbreviations entry
                      ExceptionMultiple      -> appendLine fileSyllablesMultiple entry
                      ExceptionSpecialChar c -> appendLine fileSyllablesSpecialChar $ Lazy.singleton c <> " " <> entry
                      ExceptionSingleLetter  -> appendLine fileSyllablesSingleLetter entry
                      ExceptionEllipsis      -> appendLine fileSyllablesEllipsis entry
                pure hyphenated
      foldM_ acc (Success $ SyllableData "" []) entries

      appendLine tmpFileNoParse "END"
      renamePath tmpFileNoParse fileSyllablesNoParse

fileStenoWords :: FilePath
fileStenoWords = "stenowords.txt"

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
    , swsMapChordParts :: HashMap Text [PartsData]
    }

stenoWords :: OptionsStenoWords -> IO ()
stenoWords (OStwRun greediness (OStwArg str)) =
  case parseSeries greediness str of
    Left err -> StrictIO.putStrLn $ showt err
    Right SeriesData{..} ->
      StrictIO.putStrLn $
           sdHyphenated
        <> " " <> showt sdScore
        <> " " <> showt sdPath
        <> " " <> showt sdParts

stenoWords (OStwRun greediness OStwStdin) =
  forever $ do
    l <- Lazy.toStrict <$> getLine
    StrictIO.putStrLn $ showt $ parseSeries greediness l

stenoWords (OStwRun greediness (OStwFile reset showChart mFileOutput)) = do

    start <- getTime Monotonic
    now <- getCurrentTime

    let lsFiles =
          [ fileStenoWords
          , fileStenoWordsNoParse
          , fileStenoWordsDuplicates
          , fileStenoWordsMissingPrimitives
          , fileStenoWordsParsecErrors
          ]
    when reset $ do
      for_ lsFiles $ \file -> do
        exists <- doesFileExist file
        when exists $ do
          putStrLn $ "Deleting " <> file
          removeFile file

    existsStenoWordsNoParse <- doesFileExist fileStenoWordsNoParse
    runStenoWords $ if existsStenoWordsNoParse
      then (fileStenoWordsNoParse, False)
      else (fileSyllables, True)

    putStrLn ""
    putStrLn "Number of lines in"

    for_ lsFiles $ \file -> do
      nl <- wcl file
      putStrLn $ show nl <> "\t" <> file

    nNoParse <- wcl fileStenoWordsNoParse
    let newZeroScores = replicate nNoParse 0

    ls <- Lazy.lines <$> readFile fileStenoWords
    let newScores = ls <&> \line -> readDouble $ Lazy.words line !! 2

    let scores = newZeroScores <> newScores
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
      putStrLn $ "Creating steno chords for " <> show (length lsSyllable) <> " entries."

      let tmpFileNoParse = "stenowords-noparse.tmp.txt"

          -- acc :: HashMap Text SeriesData -> Lazy.Text -> IO (HashMap Text SeriesData)
          acc
            :: StenoWordsState
            -> Lazy.Text
            -> IO StenoWordsState
          acc sws@StenoWordsState{..} str =
            let (word, hyphenated) =
                  case splitOn " " $ Lazy.toStrict str of
                    [w, h] -> (w, h)
                    _      -> error $ Text.unpack $ Lazy.toStrict $ "illegal entry: " <> str
            in  case parseSeries greediness hyphenated of
                  Left err  -> do
                    case err of
                      PEMissingPrimitives orig ->
                        when bFirstPass $
                          appendLine fileStenoWordsMissingPrimitives $ Lazy.fromStrict orig
                      PEExceptionTable orig ->
                        print $ "Error in exception table for: " <> orig
                      PEParsec raw pe ->
                        when bFirstPass $
                          appendLine fileStenoWordsParsecErrors $
                            Lazy.fromStrict $
                                 showt word <> " "
                              <> showt hyphenated <> " "
                              <> showt raw <> " "
                              <> replace "\n" "" (Text.pack $ show pe)
                    appendLine tmpFileNoParse str $> sws
                  Right sd  ->
                    if word `member` swsMapWordSteno
                      then do when bFirstPass $
                                appendLine fileStenoWordsDuplicates str
                              pure sws
                      else pure $ sws
                             { swsMapWordSteno = HashMap.insert word sd swsMapWordSteno
                             }

      let swsInitial = StenoWordsState HashMap.empty HashMap.empty
      StenoWordsState{..} <- foldM acc swsInitial lsSyllable
      for_ (HashMap.toList swsMapWordSteno) $ \(word, sd) -> do
        let str = Lazy.fromStrict $ word <> " " <> showt sd
        appendLine fileStenoWords str
        maybe (pure ()) (`appendLine` str) mFileOutput

      renamePath tmpFileNoParse fileStenoWordsNoParse

stenoWords OStwShowChart = do
  now <- getCurrentTime
  let dir = takeDirectory $ fileStenoWordsScores now
  files <- listDirectory dir
  let latest = maximum files
  scores <- readScores $ dir </> latest
  plotScoresShow scores

data PartsData
  = PartsData
    { pdOrig       :: Text
    , pdHyphenated :: Text
    , pdPath       :: Path
    }
  deriving stock Eq

instance TextShow PartsData where
  showb PartsData{..} =
              showb pdPath
    <> " " <> showb pdOrig
    <> " " <> showb pdHyphenated

instance Ord PartsData where
  compare = comparing pdPath

filePartsDict :: FilePath
filePartsDict = "partsdict.txt"

partsDict
  :: Bool
  -> IO ()
partsDict _ = do
    lsStenoWords <- Lazy.lines <$> readFile fileStenoWords
    putStrLn $ "Processing word parts for " <> show (length lsStenoWords) <> " words."

    start <- getTime Monotonic

    let
      acc :: HashMap Text [PartsData] -> Lazy.Text -> HashMap Text [PartsData]
      acc m str =
        let
          mPartsData = do
            (pdOrig, pdHyphenated, strPath, strSeries) <-
              case splitOn " " $ Lazy.toStrict str of
              [orig, hyph, _, path, series] -> Just (orig, hyph, path, series)
              _                             -> Nothing
            pdPath <- readMaybe $ Text.unpack strPath
            chords <- either (const Nothing) Just $
              RawSteno.parseWord $ RawSteno strSeries
            pure (chords, PartsData{..})
        in
          case mPartsData of
            Nothing -> error $ Text.unpack $ Lazy.toStrict $ "illegal entry: " <> str
            Just (chords, pd) ->
              let acc' m' chord =
                    HashMap.insertWith (++) (showt @(Chord DE.Key) chord) [pd] m'
              in  foldl' acc' m chords

    let m = foldl' acc HashMap.empty lsStenoWords
    for_ (HashMap.toList m) $ \(part, lsPd) -> do
       let str = Lazy.fromStrict $ part <> " " <> showt (sort lsPd)
       appendLine filePartsDict str
       -- maybe (pure ()) (`appendLine` str) mFileOutput

    -- renamePath tmpFileNoParse fileStenoWordsNoParse
    putStrLn ""

    stop <- getTime Monotonic
    putStr "StenoWords runtime: "
    fprint (timeSpecs % "\n") start stop

appendLine
  :: FilePath
  -> Lazy.Text
  -> IO ()
appendLine file str =
  appendFile file $ str <> "\n"
