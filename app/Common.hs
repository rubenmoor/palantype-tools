module Common where

import System.Directory ( doesFileExist, removeFile )
import GHC.IO (FilePath)
import System.IO (IO, putStrLn)
import Data.Function (($))
import Data.Foldable (for_)
import Control.Monad (when)
import Data.Semigroup (Semigroup((<>)))
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.IO (appendFile)
import Data.Time (UTCTime, formatTime, defaultTimeLocale)

removeFiles :: [FilePath] -> IO ()
removeFiles files = for_ files $ \file -> do
    exists <- doesFileExist file
    when exists $ do
        putStrLn $ "Deleting " <> file
        removeFile file

appendLine :: FilePath -> Lazy.Text -> IO ()
appendLine file str = appendFile file $ str <> "\n"

fileScores :: UTCTime -> FilePath
fileScores time =
    "stenowordsstats/stenowords-stats_"
        <> formatTime defaultTimeLocale "%y%m%d-%T" time
        <> ".txt"
