module Common where

import           Control.Applicative            ( Applicative(pure) )
import           Control.Monad                  ( when )
import           Data.Foldable                  ( Foldable(foldl')
                                                , for_
                                                )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>) )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Int                       ( Int )
import           Data.Maybe                     ( fromMaybe )
import           Data.Semigroup                 ( Semigroup((<>)) )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime
                                                , defaultTimeLocale
                                                , formatTime
                                                )
import           GHC.Err                        ( error )
import           GHC.Exts                       ( seq )
import           GHC.IO                         ( FilePath )
import           Palantype.Common.RawSteno      ( RawSteno )
import           System.Directory               ( doesFileExist
                                                , removeFile
                                                )
import           System.IO                      ( IO
                                                , putStr
                                                , putStrLn, hFlush, stdout
                                                )
import           Text.Read                      ( read )
import           TextShow                       ( TextShow(showt) )
import qualified Data.Text.IO as Text
import qualified Data.Text as Text

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

readFrequencies :: FilePath -> IO (HashMap Text Int)
readFrequencies fileFrequencies = do
    putStr $ "Reading frequency data from " <> fileFrequencies <> " ..."
    hFlush stdout
    ls <- Text.lines <$> Text.readFile fileFrequencies
    let acc m l = case Text.head l of
            '#' -> m
            _   -> case Text.splitOn "\t" l of
                [w, strFrequency] -> HashMap.insert
                    w
                    (read $ Text.unpack strFrequency)
                    m
                _ -> error $ "could not read: " <> Text.unpack l
        map = foldl' acc HashMap.empty ls
    putStrLn $ map `seq` " done."
    pure map

freq :: HashMap Text Int -> Text -> Int
freq freqs w = fromMaybe 0 $ HashMap.lookup w freqs

writeJSONFile :: FilePath -> [(RawSteno, Text)] -> IO ()
writeJSONFile file ls = do
    putStr $ "Writing file " <> file <> " ..."
    hFlush stdout
    u <- Text.writeFile file
        $  "{\n"
        <> Text.intercalate ",\n" (formatJSONLine <$> ls)
        <> "\n}\n"
    putStrLn $ u `seq` " done."
  where
    formatJSONLine :: (RawSteno, Text) -> Text
    formatJSONLine (raw, word) =
        "\"" <> showt raw <> "\": \"" <> word <> "\""
