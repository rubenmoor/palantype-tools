{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Args                       (OptionsStenoWords (..),
                                             OptionsSyllables (OSylArg, OSylFile),
                                             Task (..), argOpts, OptionsStenoWordsRun (..))
import           Control.Applicative        (Alternative ((<|>)),
                                             Applicative (pure, (*>), (<*>)))
import           Control.Category           (Category ((.)))
import           Control.Monad              (Monad ((>>=)), MonadPlus (mzero),
                                             foldM, foldM_, forever, join,
                                             unless, when)
import           Data.Bifunctor             (Bifunctor (first, second))
import           Data.Bool                  (Bool (..), (&&))
import           Data.Char                  (Char)
import           Data.Either                (Either (..))
import           Data.Eq                    (Eq ((==)))
import           Data.Foldable              (Foldable (foldl, foldl', length, maximum),
                                             for_, maximumBy, traverse_)
import           Data.Function              (flip, ($))
import           Data.Functor               (Functor (fmap, (<$)), void, ($>),
                                             (<$>), (<&>))
import           Data.HashMap.Strict        (HashMap, member)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Int                   (Int)
import           Data.List                  (concat, dropWhile, head,
                                             intersperse, last, replicate,
                                             splitAt, (!!), (++))
import           Data.List.NonEmpty         (NonEmpty)
import           Data.Maybe                 (Maybe (..), fromMaybe, maybe)
import           Data.Monoid                (Monoid (mconcat, mempty), (<>))
import           Data.Ord                   (Ord ((>=)), comparing)
import           Data.String                (String)
import           Data.Text                  (Text, intercalate, replace,
                                             splitOn, toLower)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.IO               as StrictIO
import qualified Data.Text.Lazy             as Lazy
import           Data.Text.Lazy.IO          (appendFile, getLine, hGetContents,
                                             readFile, writeFile)
import           Data.Text.Lazy.Read        (double)
import           Data.Time.Clock            (UTCTime, getCurrentTime)
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import           Data.Traversable           (Traversable (traverse), for)
import           Data.Tuple                 (fst, snd)
import           Formatting                 (fprint, (%))
import           Formatting.Clock           (timeSpecs)
import           GHC.Err                    (error)
import           GHC.Float                  (Double)
import           GHC.Num                    (Num ((-)), (+))
import           GHC.Real                   (Fractional ((/)), Real,
                                             fromIntegral, realToFrac, (^))
import           Options.Applicative        (Parser, argument, command,
                                             execParser, idm, info, progDesc,
                                             str, subparser)
import           Palantype.Tools.Statistics (plotScoresShow)
import           Palantype.Tools.Steno      (ParseError (..),
                                             SeriesData (SeriesData, sdScore),
                                             parseSeries)
import           Palantype.Tools.Syllables  (Exception (..), Result (..),
                                             SyllableData (SyllableData),
                                             parseSyllables)
import           System.Clock               (Clock (Monotonic), getTime)
import           System.Directory           (doesFileExist, listDirectory,
                                             removeFile, renamePath)
import           System.Environment         (getArgs)
import           System.FilePath            (takeDirectory, (</>))
import           System.IO                  (FilePath, IO, IOMode (ReadMode),
                                             hSetNewlineMode, openFile, print,
                                             putStr, putStrLn,
                                             universalNewlineMode)
import           Text.Read                  (read)
import           Text.Show                  (Show (show))
import           TextShow                   (TextShow (showt))
import           WCL                        (wcl)

main :: IO ()
main =
  execParser argOpts >>= \case
    TaskSyllables opts  -> syllables opts
    TaskStenoWords opts -> stenoWords opts
    TaskPartsDict reset -> partsDict reset

fileSyllables :: FilePath
fileSyllables = "syllables.txt"

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
        when exists $ removeFile file

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
  let Right (d, _) = double str
  in  d

readScores
  :: FilePath
  -> IO [Double]
readScores file = do
  fmap readDouble . Lazy.lines <$> readFile file

average
  :: (Real a, Foldable t)
  => t a
  -> Double
average ls =
  let (t, n) = foldl' (\(!b,!c) a -> (a + b, c + 1)) (0, 0) ls
  in  realToFrac t / realToFrac n

stenoWords :: OptionsStenoWords -> IO ()
stenoWords (OStwRun greediness (OStwArg str)) =
    StrictIO.putStrLn $ showt $ parseSeries greediness str

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
    when reset $
      for_ lsFiles $ \file -> do
        exists <- doesFileExist file
        when exists $ removeFile file

    existsStenoWordsNoParse <- doesFileExist fileStenoWordsNoParse
    runStenoWords now mFileOutput $ if existsStenoWordsNoParse
      then (fileStenoWordsNoParse, False)
      else (fileSyllables, True)

    putStrLn ""
    putStrLn "Number of lines in"

    for_ lsFiles $ \file -> do
      nl <- wcl file
      putStrLn $ show nl <> "\t" <> file

    putStrLn ""

    stop <- getTime Monotonic
    putStr "StenoWords runtime: "
    fprint (timeSpecs % "\n") start stop

    nNoParse <- wcl fileStenoWordsNoParse
    let newZeroScores = replicate nNoParse 0

    ls <- Lazy.lines <$> readFile fileStenoWords
    let newScores = ls <&> \line -> readDouble $ Lazy.words line !! 2

    let scores = newZeroScores <> newScores
        meanScore = average scores
    writeFile (fileStenoWordsScores now) $ Lazy.unlines $ Lazy.fromStrict . showt <$> scores

    putStrLn ""
    StrictIO.putStrLn $ "Average score: " <> showt meanScore

    when showChart $ plotScoresShow scores
  where
    fileStenoWordsNoParse = "stenowords-noparse.txt"

    runStenoWords time mFileOutput (file, bFirstPass) = do
      syllables <- Lazy.lines <$> readFile file
      putStrLn $ "Creating steno chords for " <> show (length syllables) <> " entries."

      let tmpFileNoParse = "stenowords-noparse.tmp.txt"

          acc :: HashMap Text SeriesData -> Lazy.Text -> IO (HashMap Text SeriesData)
          acc m str =
            let [word, hyphenated] = splitOn " " $ Lazy.toStrict str
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
                    appendLine tmpFileNoParse str $> m
                  Right sd  ->
                    if word `member` m
                      then do when bFirstPass $
                                appendLine fileStenoWordsDuplicates str
                              pure m
                      else pure $ HashMap.insert word sd m

      m <- foldM acc HashMap.empty syllables
      for_ (HashMap.toList m) $ \(word, sd) -> do
        let str = Lazy.fromStrict $ word <> " " <> showt sd
        appendLine fileStenoWords str
        maybe (pure ()) (`appendLine` str) mFileOutput

      appendLine tmpFileNoParse "END"
      renamePath tmpFileNoParse fileStenoWordsNoParse

stenoWords OStwShowChart = do
  now <- getCurrentTime
  let dir = takeDirectory $ fileStenoWordsScores now
  files <- listDirectory dir
  let latest = maximum files
  scores <- readScores $ dir </> latest
  plotScoresShow scores

partsDict :: Bool -> IO ()
partsDict reset = do
  putStrLn "Starting over ..."

appendLine :: FilePath -> Lazy.Text -> IO ()
appendLine file str = appendFile file $ str <> "\n"
