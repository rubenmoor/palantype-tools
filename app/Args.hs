module Args where

import           Data.Bool           (Bool)
import           Data.Monoid         (Monoid (mempty))
import           Data.Semigroup      ((<>))
import           Data.Text           (Text)
import           Options.Applicative (Applicative ((<*>), (<*)), Parser, ParserInfo,
                                      argument, command, help, helper, info,
                                      long, progDesc, str, subparser, switch,
                                      (<$>), Alternative ((<|>)), strOption, short)

data Task
  = TaskWord Text
  | TaskSyllables OptionsSyllables
  | TaskStenoWords Bool
  | TaskPartsDict Bool

data OptionsSyllables
  = OSylFile Bool
  | OSylArg Text

argOpts :: ParserInfo Task
argOpts = info (helper <*> task) mempty

task :: Parser Task
task = subparser
  (  command "word" (info (TaskWord <$> argument str mempty <* helper) wordInfo)
  <> command "syllables" (info (TaskSyllables <$> optsSyllables <* helper) syllablesInfo)
  <> command "stenoWords" (info (TaskStenoWords <$> switchReset <* helper) stenoWordsInfo)
  <> command "stenoDict" (info (TaskPartsDict <$> switchReset <* helper) stenoDictInfo)
  )

switchReset :: Parser Bool
switchReset = switch
  (  long "reset"
  <> help "delete the no-parse-file, start from scratch and overwrite the \
          \result file."
  )

wordInfo =
  progDesc "Parse a single word in the format \"Di|rek|to|ren\" into a series \
           \of steno chords."

optsSyllables :: Parser OptionsSyllables
optsSyllables =
      (OSylFile <$> switchReset)
  <|> (OSylArg  <$> syllableArg)
  where
    syllableArg = strOption
      (  long "arg"
      <> short 'a'
      <> help "Extract syllable patterns from one single line of the format:\n\n \
              \Direktive >>> Di|rek|ti|ve, die; -, -n (Weisung; Verhaltensregel)"
      )

syllablesInfo =
  progDesc "Read the file \"entries.txt\" and extract the syllable patterns. \
           \The result is written to \"syllables.txt\". The remainder is written \
           \to \"syllables-noparse.txt.\" When the file \"syllables-noparse.txt\" \
           \exists already, ignore \"entries.txt\" and work on the remainder, only."

stenoWordsInfo =
  progDesc "Read the file \"syllables.txt\" and parse every word into a series \
           \of steno chords. The result is written to \"steno-words.txt\". The \
           \remainder is written to \"steno-words-noparse.txt\"."

stenoDictInfo =
  progDesc "Read the file \"steno-words.txt\" and reverse the mapping: \
           \Store for every steno chord a list of real-life word parts \
           \along with the words in which they occur."
