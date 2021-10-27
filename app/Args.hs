module Args where

import           Data.Bool           (Bool)
import           Data.Maybe          (Maybe)
import           Data.Monoid         (Monoid (mempty))
import           Data.Semigroup      ((<>))
import           Data.String         (String)
import           Data.Text           (Text)
import           Options.Applicative (Alternative ((<|>)),
                                      Applicative ((<*), (<*>)), Parser,
                                      ParserInfo, argument, command, flag',
                                      help, helper, info, long, progDesc, short,
                                      str, strOption, subparser, switch, (<$>), metavar, optional, option, auto)
import           System.IO           (FilePath)
import Data.Function (($))
import Data.Int (Int)

data Task
  = TaskSyllables OptionsSyllables
  | TaskStenoWords OptionsStenoWords
  | TaskPartsDict Bool

data OptionsSyllables
  = OSylFile Bool
  | OSylArg Text

data OptionsStenoWords
  = OStwRun Int OptionsStenoWordsRun
  | OStwShowChart

data OptionsStenoWordsRun
  = OStwFile Bool Bool (Maybe FilePath)
  | OStwArg Text
  | OStwStdin

argOpts :: ParserInfo Task
argOpts = info (helper <*> task) mempty

task :: Parser Task
task = subparser
  (  command "syllables"  (info (TaskSyllables  <$> optsSyllables  <* helper) syllablesInfo)
  <> command "stenoWords" (info (TaskStenoWords <$> optsStenoWords <* helper) stenoWordsInfo)
  <> command "stenoDict"  (info (TaskPartsDict  <$> switchReset    <* helper) stenoDictInfo)
  )

switchReset :: Parser Bool
switchReset = switch
  (  long "reset"
  <> help "delete the no-parse-file, start from scratch and overwrite the \
          \result file."
  )

arg :: String -> Parser Text
arg hlp =
  strOption
    (  long "arg"
    <> short 'a'
    <> help hlp
    )

wordInfo =
  progDesc "Parse a single word in the format \"Di|rek|to|ren\" into a series \
           \of steno chords."

optsSyllables :: Parser OptionsSyllables
optsSyllables =
      (OSylFile <$> switchReset)
  <|> (OSylArg  <$> arg hlp)
  where
    hlp = "Extract syllable patterns from one single line of the format: \
          \Direktive >>> Di|rek|ti|ve, die; -, -n (Weisung; Verhaltensregel)"

syllablesInfo =
  progDesc "Read the file \"entries.txt\" and extract the syllable patterns. \
           \The result is written to \"syllables.txt\". The remainder is written \
           \to \"syllables-noparse.txt.\" When the file \"syllables-noparse.txt\" \
           \exists already, ignore \"entries.txt\" and work on the remainder, only."

switchShowChart = switch
  (  long "show-chart"
  <> short 's'
  <> help "Show the histogram of scores after the computation."
  )

mOutputFile = optional $ strOption
  (  long "output-file"
  <> short 'f'
  <> help "Addiontally write results into the provided file."
  <> metavar "FILE"
  )

greediness = option auto
  (   long "greediness"
  <> short 'g'
  <> help "Greediness 0: use the minimal set of primitive patterns. greediness \
          \3: use all available patterns."
  )

optsStenoWords :: Parser OptionsStenoWords
optsStenoWords =
      (OStwRun <$> greediness <*> optsStenoWordsRun)
  <|> showChart
  where
    showChart = flag' OStwShowChart
      (  long "show-chart-only"
      <> short 'c'
      <> help "Don't compute chords. Only show the chart of the latest scores."
      )

optsStenoWordsRun :: Parser OptionsStenoWordsRun
optsStenoWordsRun =
      (OStwFile <$> switchReset <*> switchShowChart <*> mOutputFile)
  <|> (OStwArg  <$> arg hlp)
  <|> stdin
  where
    hlp = "Parse one word into a series of steno chords. The format is: \
          \\"Di|rek|ti|ve\"."
    stdin = flag' OStwStdin
      (  long "stdin"
      <> help "Read input from stdin. The format for each line is: \
              \\"Sil|ben|tren|nung\"."
      )

stenoWordsInfo =
  progDesc "Read the file \"syllables.txt\" and parse every word into a series \
           \of steno chords. The result is written to \"steno-words.txt\". The \
           \remainder is written to \"steno-words-noparse.txt\"."

stenoDictInfo =
  progDesc "Read the file \"steno-words.txt\" and reverse the mapping: \
           \Store for every steno chord a list of real-life word parts \
           \along with the words in which they occur."
