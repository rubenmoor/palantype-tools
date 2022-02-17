module Args
    ( Task(..)
    , OptionsPrepare(..)
    , OptionsHyphenate(..)
    , OptionsHyphenateMode(..)
    , OptionsMakeSteno (..)
    , OptionsShowChart(..)
    , OptionsSort(..)
    , OptionsMakeNumbers (..)
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

  -- | create a couple of steno codes for every word
  -- and build the steno dict from the steno codes
  | TaskMakeSteno OptionsMakeSteno

  -- | sort a word list by frequency
  | TaskSort OptionsSort

  -- | show charts
  | TaskShowChart OptionsShowChart

  -- | create a dictionary file for numbers
  | TaskMakeNumbers OptionsMakeNumbers

data OptionsPrepare
  = OPrepFile FilePath FilePath
  | OPrepArg Text

data OptionsHyphenate = OptionsHyphenate [FilePath] Lang OptionsHyphenateMode

--   = OHyphFile FilePath [FilePath] FilePath Lang
--   | OHyphArg Lang Text

data OptionsHyphenateMode
  = OHMFile FilePath FilePath
  | OHMArg  Text


-- TODO
data OptionsShowChart = OSCHistScores

data OptionsMakeSteno
  -- | input file: hyphenated words
  = OMkStFile FilePath FilePath FilePath FilePath Lang
  | OMkStArg Lang Text

-- | input file: list of words
--   frequency data
--   output file: sorted list of words
data OptionsSort = OptionsSort FilePath [FilePath]

data OptionsMakeNumbers =
  -- | output file
  OptionsMakeNumbers FilePath Lang

argOpts :: ParserInfo Task
argOpts = info (helper <*> task) mempty

task :: Parser Task
task = subparser
    (  command
          "rawSteno"
          (info (TaskRawSteno  <$> arg rawStenoHelp <* helper) rawStenoInfo)
    <> command "prepare"
          (info (TaskPrepare   <$> optsPrepare <* helper) prepareInfo)
    <> command
          "hyphenate"
          (info (TaskHyphenate <$> optsHyphenate <* helper) hyphenateInfo)
    <> command
          "makeSteno"
          (info (TaskMakeSteno <$> optsMakeSteno <* helper) makeStenoInfo)
    <> command
          "sort"
          (info (TaskSort      <$> optsSort <* helper) sortInfo)
    <> command
          "showChart"
          (info (TaskShowChart <$> optsShowChart <* helper) showChartInfo)
    <> command
          "makeNumbers"
          (info (TaskMakeNumbers <$> optsMakeNumbers <* helper) makeNumbersInfo)
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
    <> short 'l'
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

    fileInput = strOption
        (  long "file-input"
        <> short 'i'
        <> value "entries.txt"
        <> help
               "Input file with lines of the format \"Direktive >>> Di|rek|ti|ve, die; -, -n (Weisung; Verhaltensregel)\""
        <> metavar "FILE"
        <> showDefault
        )

    fileOutput = strOption
        (  long "file-output"
        <> short 'o'
        <> value "hyphenated-prepared.txt"
        <> help "Output file with lines of the format \"Di|rek|ti|ve\"."
        <> metavar "FILE"
        <> showDefault
        )

prepareInfo :: InfoMod a
prepareInfo =
    progDesc
        "Read the provided file and extract the syllable patterns. \
           \The result is written to the output file. The remainder is written \
           \to \"prepare-noparse.txt.\""

optsHyphenate :: Parser OptionsHyphenate
optsHyphenate =
    OptionsHyphenate <$> some fileHyphenated <*> lang <*> optsHyphMode
  where

    optsHyphMode :: Parser OptionsHyphenateMode
    optsHyphMode =
            (OHMFile <$> fileInput <*> fileOutput)
        <|> (OHMArg <$> arg argHlp)

    fileInput = strOption
        (  long "file-input"
        <> short 'i'
        <> value "german.utf8.dic"
        <> help "Input file with lines containing one word each."
        <> metavar "FILE"
        <> showDefault
        )

    fileHyphenated = strOption
        (  long "file-hyphenated"
        <> short 'h'
        -- <> value "hyphenated-prepared.txt" -- causes run time memory leak, probably in combination with `some`
        <> help
               "A file that contain hyphenated words. Any word that \
               \appears in the file won't be hyphenated again and the \
               \hyphenation information will be taken from this file. \
               \Use this flag various time to add more than one file, \
               \e.g.: \"-h hyphenated-prepared.txt -h hyphenated-checked.txt\"."
        <> metavar "FILE"
        <> showDefault
        )

    fileOutput = strOption
        (  long "file-output"
        <> short 'o'
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
        "Read words from the input file, check if this word has been hyphenated \
        \before. If not, hyphenate it, given the language. \
        \Write the hyphenated word to the output file."

optsMakeSteno :: Parser OptionsMakeSteno
optsMakeSteno =
    ( OMkStFile <$> fileInput
                <*> fileOutputPlover
                <*> fileOutputPloverMin
                <*> fileOutputDoc
                <*> lang
    )
    <|> (OMkStArg <$> lang <*> arg argHlp)
  where
    argHlp
        = "Parse one word into a series of steno chords. The format is: \
          \\"Di|rek|ti|ve\"."

    fileInput = strOption
        (  long "file-input"
        <> short 'i'
        <> value "hyphenated.txt"
        <> help "Input file with lines containing one word each. \
                \The words must be hyphenated, e.g. \"Di|rek|ti|ve\"."
        <> metavar "FILE"
        <> showDefault
        )

    fileOutputPlover = strOption
        (  long "file-output-plover"
        <> short 'o'
        <> value "palantype-DE.json"
        <> help "The dictionary file for use with plover."
        <> metavar "FILE"
        <> showDefault
        )

    fileOutputPloverMin = strOption
        (  long "file-output-plover-minimal"
        <> short 'm'
        <> value "palantype-DE-min.json"
        <> help "A minimal version of the dictionary file for use with plover."
        <> metavar "FILE"
        <> showDefault
        )

    fileOutputDoc = strOption
        (  long "file-output-doc"
        <> short 'd'
        <> value "palantype-DE-doc.json"
        <> help "A map of pattern groups pointing to maps of greediness pointing \
                \to a pair of the number of unique dictionary entries, given \
                \the combination of pattern group and greediness, and the \
                \entries themselves, reduced to a maximum of 100. \
                \\"Unique\" means that every word is only represented by one \
                \steno code."
        <> metavar "FILE"
        <> showDefault
        )

makeStenoInfo :: InfoMod a
makeStenoInfo =
    progDesc "Read the input file and parse every word into a series \
           \of steno chords. Resolve the collisions and write the \
           \dictionary files."

optsSort :: Parser OptionsSort
optsSort = OptionsSort <$> fileFrequencyData <*> some file
  where
    file = strOption
        (  long "file"
        <> short 'i'
        <> help "The provided files have to be either a text file with lines \
                \that contain a word first, then either nothing or other \
                \content separated by space. \
                \Or a JSON file (.json) with a dictionary of type STENO: \
                \WORD. Each file is sorted and replaced by the sorted version."
        <> metavar "FILE"
        )

    fileFrequencyData = strOption
        (  long "file-frequency-data"
        <> short 'f'
        <> value "deu_news_2020_freq.txt"
        <> help "Frequency data file."
        <> metavar "FILE"
        <> showDefault
        )

sortInfo :: InfoMod a
sortInfo =
    progDesc
        "Given a file with frequency information in the format WORD NUMBER, \
        \where NUMBER is the frequency, sort any file that contains words by \
        \frequency, descending. The | symbol in words is ignored. Files with \
        \.json ending are expected to be dictionaries of type: STENO: WORD, \
        \and are sorted by word frequency."

optsShowChart :: Parser OptionsShowChart
optsShowChart = pure OSCHistScores

showChartInfo :: InfoMod a
showChartInfo = progDesc "Show a chart with statistics."

optsMakeNumbers :: Parser OptionsMakeNumbers
optsMakeNumbers = OptionsMakeNumbers <$> file <*> lang
  where
    file = strOption
        (  long "file"
        <> short 'o'
        <> help "Output file for the numbers dictionary in json format; for \
                \use with Plover."
        <> value "palantype-DE-numbers.json"
        <> metavar "FILE"
        <> showDefault
        )

makeNumbersInfo :: InfoMod a
makeNumbersInfo =
    progDesc "Create Palantype-style steno codes for numbers for the given \
             \language."
