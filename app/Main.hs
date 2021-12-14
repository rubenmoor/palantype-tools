{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications   #-}

module Main where

import           Control.Category               ( (<<<)
                                                , Category((.))
                                                )
import           Control.Monad                  ( Monad((>>=))

                                                )
import           Data.Bool                      ( (&&)
                                                , not
                                                )
import           Data.Either                    ( Either(..) )
import           Data.Eq                        ( Eq((/=), (==)) )
import           Data.Foldable                  ( Foldable(maximum)
                                                , for_, traverse_

                                                )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>)
                                                , Functor(fmap)
                                                )
import           Data.List                      ( filter
                                                , sortOn, head
                                                )
import           Data.Monoid                    ( (<>)
                                                , Monoid(mconcat)
                                                )
import           Data.Text                      ( Text
                                                )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as StrictIO
import qualified Data.Text.IO              as Text
import           Data.Text.Read            ( double )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                )
import           Data.Traversable               ( Traversable(traverse) )
import           GHC.Err                        ( error )
import           GHC.Float                      ( Double )
import           Options.Applicative            ( execParser )
import           Palantype.Common               ( Lang(DE, EN) )
import           Palantype.Common.RawSteno      ( RawSteno(RawSteno) )
import qualified Palantype.Common.RawSteno     as RawSteno
import qualified Palantype.DE.Keys             as DE
import           Palantype.Tools.Statistics     ( plotScoresShow )
import           System.Directory               ( listDirectory )
import           System.FilePath                ( (</>)
                                                , FilePath
                                                , takeDirectory, takeExtension
                                                )
import           System.IO                      ( IO
                                                , putStrLn, putStr, hFlush, stdout
                                                )
import qualified Text.Hyphenation              as KL
import           Text.Show                      ( Show(show) )
import           TextShow                       ( TextShow(showt) )

import           Args                           ( OptionsHyphenate(..)
                                                , OptionsShowChart
                                                    ( OSCHistScores
                                                    )
                                                , OptionsSort(..)
                                                , Task(..)
                                                , argOpts
                                                )
import           BuildDict                      ( buildDict )
import           Common                         ( appendLine
                                                , fileScores, removeFiles
                                                , freq, readFrequencies, writeJSONFile
                                                )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Maybe                     ( Maybe (..), fromMaybe )
import           Data.Ord                       ( Down(Down) )
import           RunPrepare                     ( prepare )
import           WCL                            ( wcl )
import qualified Data.Aeson as Aeson
import Data.Tuple (snd)
import GHC.Exts (seq)

main :: IO ()
main = execParser argOpts >>= \case
    TaskRawSteno  str  -> rawSteno str
    TaskPrepare   opts -> prepare opts
    TaskHyphenate opts -> hyphenate opts
    TaskStenoDict opts -> buildDict opts
    TaskSort      opts -> sortByFrequency opts
    TaskShowChart opts -> showChart opts

rawSteno :: Text -> IO ()
rawSteno str =
    StrictIO.putStrLn $ showt $ RawSteno.parseSteno @DE.Key (RawSteno str)

hyphenate :: OptionsHyphenate -> IO ()
hyphenate (OHyphArg lang txt) = do

    let lang' = case lang of
            DE -> KL.German_1996
            EN -> KL.English_US

    StrictIO.putStrLn $ Text.intercalate "|" $ Text.pack <$> KL.hyphenate
        (KL.languageHyphenator lang')
        (Text.unpack txt)

hyphenate (OHyphFile fileInput filesHyphenated fileOutput lang) = do

    now <- getCurrentTime

    let lang' = case lang of
            DE -> KL.German_1996
            EN -> KL.English_US

    removeFiles [fileOutput]

    putStrLn "Reading existing hyphenation files: "
    for_ filesHyphenated $ \file -> do
        nLines <- wcl file
        putStrLn $ file <> " (" <> show nLines <> " lines)"

    let hasContent line = not (Text.null line) && Text.head line /= '#'

    lsExisting <- filter hasContent . mconcat <$> traverse (fmap Text.lines <<< Text.readFile) filesHyphenated

    let mapExisting = HashMap.fromList $ (\w -> (Text.replace "|" "" w, w)) <$> lsExisting

    nLines <- wcl fileInput
    putStrLn
        $  "Reading words from file "
        <> fileInput
        <> " ("
        <> show nLines
        <> " lines)"
    ls <- Text.lines <$> Text.readFile fileInput

    putStrLn $ "Writing to " <> fileOutput
    appendLine fileOutput $ "# " <> Text.pack
        (formatTime defaultTimeLocale "%y%m%d-%T" now)

    for_ ls $ \l -> do
        let hyph = case HashMap.lookup l mapExisting of
              Just h -> h
              Nothing -> Text.intercalate "|" $ Text.pack <$> KL.hyphenate (KL.languageHyphenator lang') (Text.unpack l)
        appendLine fileOutput hyph

    nLO <- wcl fileOutput
    putStrLn $ fileOutput <> " (" <> show nLO <> " lines) written."

sortByFrequency :: OptionsSort -> IO ()
sortByFrequency (OptionsSort fileFrequencies files) = do

    freqs <- readFrequencies fileFrequencies

    traverse_ (sortFile freqs) files
  where
    sortFile freqs file = do
      nLI <- wcl file
      putStr
          $  "Reading words from file "
          <> file
          <> " ("
          <> show nLI
          <> " lines) ..."
      hFlush stdout

      if takeExtension file == ".json"
          then do
              ls <- HashMap.toList . fromMaybe (error "Could not decode json file") <$> Aeson.decodeFileStrict' file
              putStrLn $ ls `seq` " done."

              putStr "Sorting ..."
              hFlush stdout
              let sorted = sortOn (Down <<< freq freqs <<< snd) ls
              putStrLn " done."

              writeJSONFile file sorted
          else do
              ls <- Text.lines <$> StrictIO.readFile file
              putStrLn $ ls `seq` " done."

              putStr "Sorting ..."
              hFlush stdout
              let sorted = sortOn
                      (Down <<< freq freqs <<< Text.replace "|" "" <<< head <<< Text.splitOn " ")
                      ls
              putStrLn $ sorted `seq` " done."
              putStr $ "Writing file " <> file <> " ..."
              hFlush stdout
              u <- Text.writeFile file $ Text.unlines sorted
              putStrLn $ u `seq` " done."

readScores :: FilePath -> IO [Double]
readScores file = do
    fmap readDouble . Text.lines <$> Text.readFile file
  where
    readDouble str = case double str of
        Right (d, _) -> d
        Left  err    -> error $ "Could not read double: " <> err

showChart :: OptionsShowChart -> IO ()
showChart OSCHistScores = do
    now <- getCurrentTime
    let dir = takeDirectory $ fileScores now
    files <- listDirectory dir
    let latest = maximum files
    putStrLn $ "Reading scores from " <> dir </> latest
    scores <- readScores $ dir </> latest
    plotScoresShow scores
