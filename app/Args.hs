module Args
    ( Task(..)
    , OptionsPrepare(..)
    , OptionsHyphenate(..)
    , OptionsStenoDict(..)
    , OptionsShowChart(..)
    , OptionsSort(..)
    , argOpts
    ) where

import           Control.Applicative
import           Data.Monoid                    ( Monoid(mempty) )
import           Data.Semigroup                 ( (<>) )
import           Data.String                    ( String )
import           Data.Text                      ( Text )
import           Options.Applicative            ( InfoMod
                                                , Parser
                                                , ParserInfo
                                                , auto
                                                , command
                                                , help
                                                , helper
                                                , info
                                                , long
                                                , metavar
                                                , option
                                                , progDesc
                                                , short
                                                , showDefault
                                                , strOption
                                                , subparser
                                                , value
                                                )
import           Palantype.Common               ( Lang(DE) )
import           System.IO                      ( FilePath )

data Task

  -- | validate some raw steno
  = TaskRawSteno Text

  -- | turn some word list into the required format
  | TaskPrepare OptionsPrepare

  -- | hyphenate a word list
  | TaskHyphenate OptionsHyphenate

  -- | build the steno dict
  | TaskStenoDict OptionsStenoDict

  -- | show charts
  | TaskShowChart OptionsShowChart

  -- | sort a word list by frequency
  | TaskSort OptionsSort

data OptionsPrepare
  = OPrepFile FilePath FilePath
  | OPrepArg Text

data OptionsHyphenate
  = OHyphFile FilePath [FilePath] FilePath Lang
  | OHyphArg Lang Text

-- TODO
data OptionsShowChart = OSCHistScores

data OptionsStenoDict
  -- | input file: hyphenated words
  --   output file: if the output file exists,
  --   its entries are taken into account
  = OStDFile FilePath FilePath Lang
  | OStDArg Lang Text

-- | input file: list of words
--   frequency data
--   output file: sorted list of words
data OptionsSort = OptionsSort FilePath FilePath FilePath

argOpts :: ParserInfo Task
argOpts = info (helper <*> task) mempty

task :: Parser Task
task = subparser
    (  command
          "rawSteno"
          (info (TaskRawSteno <$> arg rawStenoHelp <* helper) rawStenoInfo)
    <> command "prepare"
               (info (TaskPrepare <$> optsPrepare <* helper) prepareInfo)
    <> command
           "hyphenate"
           (info (TaskHyphenate <$> optsHyphenate <* helper) hyphenateInfo)
    <> command
           "stenoDict"
           (info (TaskStenoDict <$> optsStenoDict <* helper) stenoWordsInfo)
    <> command "sort" (info (TaskSort <$> optsSort <* helper) sortInfo)
    )
  where
    rawStenoHelp
        = "Parse raw steno like \"A/HIFn/LA/HIFn/GDAOD\" into a steno chord to \
      \validate it."

    rawStenoInfo = progDesc "Validate raw steno."

arg :: String -> Parser Text
arg hlp = strOption (long "arg" <> short 'a' <> help hlp)


lang :: Parser Lang
lang = option
    auto
    (  long "language"
    <> value DE
    <> help "One of DE, EN"
    <> metavar "LANG"
    <> showDefault
    )

optsPrepare :: Parser OptionsPrepare
optsPrepare =
    (OPrepFile <$> fileInput <*> fileOutput) <|> (OPrepArg <$> arg hlpArg)
  where
    hlpArg
        = "Extract syllable patterns from one single line of the format: \
          \Direktive >>> Di|rek|ti|ve, die; -, -n (Weisung; Verhaltensregel)"

    fileInput = option
        auto
        (  long "file-input"
        <> value "entries.txt"
        <> help
               "Input file with lines of the format \"Direktive >>> Di|rek|ti|ve, die; -, -n (Weisung; Verhaltensregel)\""
        <> metavar "FILE"
        )

    fileOutput = option
        auto
        (  long "file-output"
        <> value "hyphenated-prepared.txt"
        <> help "Output file with lines of the format \"Di|rek|ti|ve\"."
        <> metavar "FILE"
        )

prepareInfo :: InfoMod a
prepareInfo =
    progDesc
        "Read the provided file and extract the syllable patterns. \
           \The result is written to the output file. The remainder is written \
           \to \"prepare-noparse.txt.\""

optsHyphenate :: Parser OptionsHyphenate
optsHyphenate =
    (OHyphFile <$> fileInput <*> some fileHyphenated <*> fileOutput <*> lang)
        <|> (OHyphArg <$> lang <*> arg argHlp)
  where

    fileInput = option
        auto
        (  long "file-input"
        <> value "words.txt"
        <> help "Input file with lines containing one word each."
        <> metavar "FILE"
        <> showDefault
        )

    fileHyphenated = option
        auto
        (  long "files-hyphenated"
        <> short 'h'
        <> value "hyphenated-prepared.txt"
        <> help
               "A list of files that contain hyphenated words. Any word that appears in one of these files won't be hyphenated again."
        <> metavar "FILELIST"
        <> showDefault
        )

    fileOutput = option
        auto
        (  long "file-output"
        <> value "hyphenated.txt"
        <> help
               "A file containing a hyphenated word in each line, e.g. \"Di|rek|ti|ve\". If the file exists, lines will be appended."
        <> metavar "FILE"
        <> showDefault
        )

    argHlp = "Hyphenate the word given in the argument."

hyphenateInfo :: InfoMod a
hyphenateInfo =
    progDesc
        "Read words from the input file, check if this word has been hyphenated before. If not, hyphenate given the language and write to the output file."

optsStenoDict :: Parser OptionsStenoDict
optsStenoDict =
    (OStDFile <$> fileInput <*> fileOutput <*> lang)
        <|> (OStDArg <$> lang <*> arg argHlp)
  where
    argHlp
        = "Parse one word into a series of steno chords. The format is: \
          \\"Di|rek|ti|ve\"."

    fileInput = option
        auto
        (  long "file-input"
        <> value "hyphenated.txt"
        <> help
               "Input file with lines containing one word each. The words must be hyphenated, e.g. \"Di|rek|ti|ve\"."
        <> metavar "FILE"
        <> showDefault
        )

    fileOutput = option
        auto
        (  long "file-output"
        <> value "palantype.json"
        <> help "The dictionary file for use with plover."
        <> metavar "FILE"
        <> showDefault
        )

    -- showChart = flag'
    --     OStwShowChart
    --     (long "show-chart-only" <> short 'c' <> help
    --         "Don't compute chords. Only show the chart of the latest scores."
    --     )

stenoWordsInfo :: InfoMod a
stenoWordsInfo =
    progDesc
        "Read the file \"syllables.txt\" and parse every word into a series \
           \of steno chords. The result is written to \"steno-words.txt\". The \
           \remainder is written to \"steno-words-noparse.txt\"."

-- frequencyInfo :: InfoMod a
-- frequencyInfo =
--     progDesc
--         "Write 'sten/o/chords word' to a file, most frequent first. 'frequency2k' \
--         \is meant for use with palantype.com/learn-palantype, e.g. \
--         \copy frequency2k.txt ../learn-palantype/backend/. 'frequencyAll' can \
--         \be used to identify missing single syllable words and add the to \
--         \the stenoWords algorithm."

optsSort :: Parser OptionsSort
optsSort = OptionsSort <$> fileInput <*> fileFrequencyData <*> fileOutput
  where
    fileInput = option
        auto
        (  long "file-input"
        <> value "words.txt"
        <> help "Input file with lines containing one word each."
        <> metavar "FILE"
        <> showDefault
        )

    fileFrequencyData = option
        auto
        (  long "file-frequency-data"
        <> value "deu_news_2020_freq.txt"
        <> help "Frequency data file."
        <> metavar "FILE"
        <> showDefault
        )

    fileOutput = option
        auto
        (  long "file-output"
        <> value "words-sorted.txt"
        <> help
               "Output file with lines containing one word each, sorted by frequency in descending order."
        <> metavar "FILE"
        <> showDefault
        )


sortInfo :: InfoMod a
sortInfo =
    progDesc
        "Given a file with frequency information in the format WORD NUMBER, where NUMBER is the frequency, sort any file that contains words by frequency, descending."
