module Common where

import Control.Monad (when)
import Data.Foldable (Foldable (toList), for_)
import Data.Function (($))
import Data.Functor (Functor, (<$>))
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (
    UTCTime,
    defaultTimeLocale,
    formatTime,
 )
import GHC.Exts (seq)
import GHC.IO (FilePath)
import Palantype.Common.RawSteno (RawSteno)
import System.Directory (
    doesFileExist,
    removeFile,
 )
import System.IO (
    IO,
    hFlush,
    putStr,
    putStrLn,
    stdout,
 )
import TextShow (TextShow (showt))

removeFiles :: [FilePath] -> IO ()
removeFiles files = for_ files $ \file -> do
    exists <- doesFileExist file
    when exists $ do
        putStrLn $ "Deleting " <> file
        removeFile file

appendLine :: FilePath -> Text -> IO ()
appendLine file str = Text.appendFile file $ str <> "\n"

fileScores :: UTCTime -> FilePath
fileScores time =
    "stenowordsstats/stenowords-stats_"
        <> formatTime defaultTimeLocale "%y%m%d-%T" time
        <> ".txt"

writeJSONFile :: forall l. (Foldable l, Functor l) => FilePath -> l (RawSteno, Text) -> IO ()
writeJSONFile file ls = do
    putStr $ "Writing file " <> file <> " ..."
    hFlush stdout
    u <-
        Text.writeFile file $
            "{\n"
                <> Text.intercalate ",\n" (toList $ formatJSONLine <$> ls)
                <> "\n}\n"
    putStrLn $ u `seq` " done."
  where
    formatJSONLine :: (RawSteno, Text) -> Text
    formatJSONLine (raw, word) =
        "\"" <> showt raw <> "\": \"" <> word <> "\""
