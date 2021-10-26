module Args where

import           Data.Bool           (Bool)
import           Data.Monoid         (Monoid (mempty))
import           Data.Semigroup      ((<>))
import           Data.Text           (Text)
import           Options.Applicative (Applicative ((<*>), (<*)), Parser, ParserInfo,
                                      argument, command, help, helper, info,
                                      long, progDesc, str, subparser, switch,
                                      (<$>), Alternative ((<|>)), strOption, short, flag')
import Data.String (String)

data Task
  = TaskSyllables OptionsSyllables
  | TaskStenoWords OptionsStenoWords
  | TaskPartsDict Bool

data OptionsSyllables
  = OSylFile Bool
  | OSylArg Text

data OptionsStenoWords
  = OStwFile Bool Bool
  | OStwArg Text
  | OStwStdin
  | OStwShowChart

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

optsStenoWords :: Parser OptionsStenoWords
optsStenoWords =
      (OStwFile <$> switchReset <*> switchShowChart)
  <|> (OStwArg  <$> arg hlp)
  <|> stdin
  <|> showChart
  where
    hlp = "Parse one word into a series of steno chords. The format is: \
          \\"Di|rek|ti|ve\"."
    stdin = flag' OStwStdin
      (  long "stdin"
      <> help "Read input from stdin. The format for each line is: \
              \\"Sil|ben|tren|nung\"."
      )
    showChart = flag' OStwShowChart
      (  long "show-chart-only"
      <> short 'c'
      <> help "Don't compute chords. Only show the chart of the latest scores."
      )

stenoWordsInfo =
  progDesc "Read the file \"syllables.txt\" and parse every word into a series \
           \of steno chords. The result is written to \"steno-words.txt\". The \
           \remainder is written to \"steno-words-noparse.txt\"."

stenoDictInfo =
  progDesc "Read the file \"steno-words.txt\" and reverse the mapping: \
           \Store for every steno chord a list of real-life word parts \
           \along with the words in which they occur."
