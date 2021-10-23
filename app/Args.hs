module Args where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Bool (Bool)
import Data.Monoid (Monoid(mempty))

data Task
  = TaskWord Text
  | TaskSyllables Bool
  | TaskStenoWords Bool
  | TaskPartsDict Bool

argOpts :: ParserInfo Task
argOpts = info task mempty

task :: Parser Task
task = subparser
  (  command "word" (info (TaskWord <$> argument str mempty) wordInfo)
  <> command "syllables" (info (TaskSyllables <$> resetSwitch) syllablesInfo)
  <> command "stenoWords" (info (TaskStenoWords <$> resetSwitch) stenoWordsInfo)
  <> command "stenoDict" (info (TaskPartsDict <$> resetSwitch) stenoDictInfo)
  )

resetSwitch :: Parser Bool
resetSwitch = switch
  (  long "reset"
  <> help "delete the no-parse-file, start from scratch and overwrite the \
          \result file."
  )

wordInfo =
  progDesc "Parse a single word in the format \"Di|rek|to|ren\" into a series \
           \of steno chords."

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
