{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Args                      (OptionsStenoWords (..),
                                            OptionsSyllables (OSylArg, OSylFile),
                                            Task (..), argOpts)
import           Control.Applicative       (Alternative ((<|>)),
                                            Applicative (pure, (*>), (<*>)))
import           Control.Category          (Category ((.)))
import           Control.Monad             (Monad ((>>=)), MonadPlus (mzero),
                                            foldM, forever, join, when)
import           Data.Bifunctor            (Bifunctor (first, second))
import           Data.Bool                 (Bool (False))
import           Data.Char                 (Char)
import           Data.Either               (Either (..))
import           Data.Eq                   (Eq)
import           Data.Foldable             (Foldable (foldl, length, maximum),
                                            for_, maximumBy, traverse_)
import           Data.Function             (($))
import           Data.Functor              (Functor (fmap), void, ($>), (<$>),
                                            (<&>))
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap
import           Data.Int                  (Int)
import           Data.List                 (concat, dropWhile, head,
                                            intersperse, last, splitAt, (!!),
                                            (++))
import           Data.List.NonEmpty        (NonEmpty)
import           Data.Maybe                (Maybe (..), fromMaybe, maybe)
import           Data.Monoid               (Monoid (mconcat, mempty), (<>))
import           Data.Ord                  (Ord ((>=)), comparing)
import           Data.String               (String)
import           Data.Text                 (Text, intercalate, replace, splitOn,
                                            toLower)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import qualified Data.Text.IO              as StrictIO
import qualified Data.Text.Lazy            as Lazy
import           Data.Text.Lazy.IO         (appendFile, getLine, hGetContents,
                                            readFile)
import           Data.Traversable          (for)
import           Data.Tuple                (fst, snd)
import           Formatting                (fprint, (%))
import           Formatting.Clock          (timeSpecs)
import           GHC.Err                   (error)
import           GHC.Float                 (Double)
import           GHC.Num                   (Num ((-)), (+))
import           GHC.Real                  (Fractional ((/)), fromIntegral, (^))
import           Options.Applicative       (Parser, argument, command,
                                            execParser, idm, info, progDesc,
                                            str, subparser)
import           Palantype.Tools.Steno     (SeriesData (SeriesData),
                                            parseSeries)
import           Palantype.Tools.Syllables (Exception (..), Result (..),
                                            parseSyllables)
import           System.Clock              (Clock (Monotonic), getTime)
import           System.Directory          (doesFileExist, removeFile,
                                            renamePath)
import           System.Environment        (getArgs)
import           System.IO                 (FilePath, IO, IOMode (ReadMode),
                                            hSetNewlineMode, openFile, print,
                                            putStr, putStrLn,
                                            universalNewlineMode)
import           Text.Show                 (Show (show))
import           TextShow                  (TextShow (showt))
import           WCL                       (wcl)

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

      for_ entries $ \entry ->
        for_ (parseSyllables $ Lazy.toStrict entry) $ \case
          Failure err     -> do
            print err
            appendLine tmpFileNoParse entry
          Success sd -> appendLine fileSyllables $ Lazy.fromStrict $ showt sd
          Exception exc -> case exc of
            ExceptionAbbreviation  -> appendLine fileSyllablesAbbreviations entry
            ExceptionMultiple      -> appendLine fileSyllablesMultiple entry
            ExceptionSpecialChar c -> appendLine fileSyllablesSpecialChar $ Lazy.singleton c <> " " <> entry
            ExceptionSingleLetter  -> appendLine fileSyllablesSingleLetter entry
            ExceptionEllipsis      -> appendLine fileSyllablesEllipsis entry
      renamePath tmpFileNoParse fileSyllablesNoParse

fileStenoWords :: FilePath
fileStenoWords = "stenowords.txt"

stenoWords :: OptionsStenoWords -> IO ()
stenoWords (OStwArg str) =
  case parseSeries str of
    Left err -> print err
    Right sd -> StrictIO.putStrLn $ showt sd

stenoWords OStwStdin =
  forever $ do
    l <- Lazy.toStrict <$> getLine
    case parseSeries l of
      Left err -> print err
      Right sd -> StrictIO.putStrLn $ showt sd

stenoWords (OStwFile reset) = do

    start <- getTime Monotonic

    let lsFiles =
          [ fileStenoWords
          , fileStenoWordsNoParse
          ]
    when reset $
      for_ lsFiles $ \file -> do
        exists <- doesFileExist file
        when exists $ removeFile file

    existsStenoWordsNoParse <- doesFileExist fileStenoWordsNoParse
    runStenoWords $ if existsStenoWordsNoParse
      then fileStenoWordsNoParse
      else fileSyllables

    putStrLn ""
    putStrLn "Number of lines in"

    for_ lsFiles $ \file -> do
      nl <- wcl file
      putStrLn $ show nl <> "\t" <> file

    putStrLn ""

    stop <- getTime Monotonic
    putStr "StenoWords runtime: "
    fprint (timeSpecs % "\n") start stop
  where
    fileStenoWordsNoParse = "stenowords-noparse.txt"

    runStenoWords file = do
      syllables <- Lazy.lines <$> readFile file
      putStrLn $ "Creating steno chords for " <> show (length syllables) <> " entries."

      let tmpFileNoParse = "stenowords-noparse.tmp.txt"

          acc :: HashMap Text SeriesData -> Lazy.Text -> IO (HashMap Text SeriesData)
          acc m str =
            let [word, hyphenated] = splitOn " " $ Lazy.toStrict str
            in  case parseSeries hyphenated of
                  Left err -> appendLine tmpFileNoParse str $> m
                  Right sd -> pure $ HashMap.insert word sd m

      m <- foldM acc HashMap.empty syllables
      for_ (HashMap.toList m) $ \(word, sd) ->
        appendLine fileStenoWords $ Lazy.fromStrict $ word <> " " <> showt sd

      renamePath tmpFileNoParse fileStenoWordsNoParse

partsDict :: Bool -> IO ()
partsDict reset = do
  putStrLn "Starting over ..."

appendLine :: FilePath -> Lazy.Text -> IO ()
appendLine file str = appendFile file $ str <> "\n"
