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
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as StrictIO
import qualified Data.Text.Lazy                as Lazy
import           Data.Text.Lazy.IO              ( appendFile
                                                , readFile
                                                , writeFile
                                                )
import           Data.Time                      ( UTCTime
                                                , defaultTimeLocale
                                                , formatTime
                                                )
import           GHC.Err                        ( error )
import           GHC.IO                         ( FilePath )
import           Palantype.Common.RawSteno      ( RawSteno )
import           System.Directory               ( doesFileExist
                                                , removeFile
                                                )
import           System.IO                      ( IO
                                                , putStrLn
                                                )
import           Text.Read                      ( read )
import           TextShow                       ( TextShow(showt) )

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

readFrequencies :: FilePath -> IO (HashMap Text Int)
readFrequencies fileFrequencies = do
    putStrLn $ "Reading frequency data from " <> fileFrequencies
    ls <- Lazy.lines <$> readFile fileFrequencies
    let acc m l = case Lazy.head l of
            '#' -> m
            _   -> case Lazy.splitOn "\t" l of
                [w, strFrequency] -> HashMap.insert
                    (Lazy.toStrict w)
                    (read $ Lazy.unpack strFrequency)
                    m
                _ -> error $ "could not read: " <> Lazy.unpack l
    pure $ foldl' acc HashMap.empty ls

freq :: HashMap Text Int -> Text -> Int
freq freqs w = fromMaybe 0 $ HashMap.lookup w freqs

writeJSONFile :: FilePath -> [(RawSteno, Text)] -> IO ()
writeJSONFile file ls = do
    StrictIO.putStrLn $ "Writing file " <> Text.pack file
    writeFile file
        $  "{\n"
        <> Lazy.intercalate ",\n" (formatJSONLine <$> ls)
        <> "\n}\n"
  where
    formatJSONLine :: (RawSteno, Text) -> Lazy.Text
    formatJSONLine (raw, word) =
        Lazy.fromStrict $ "\"" <> showt raw <> "\": \"" <> word <> "\""
