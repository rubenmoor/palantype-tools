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
                                                , for_

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
                                                , replace


                                                )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as StrictIO
import qualified Data.Text.Lazy                as Lazy
import           Data.Text.Lazy.IO              ( readFile
                                                , writeFile
                                                )
import           Data.Text.Lazy.Read            ( double )
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
                                                , putStrLn
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

    let hasContent line = not (Lazy.null line) && Lazy.head line /= '#'

    lsExisting <- filter hasContent . mconcat <$> traverse (fmap Lazy.lines <<< readFile) filesHyphenated

    let mapExisting = HashMap.fromList $ (\w -> (replace "|" "" w, w)) . Lazy.toStrict <$> lsExisting

    nLines <- wcl fileInput
    putStrLn
        $  "Reading words from file "
        <> fileInput
        <> " ("
        <> show nLines
        <> " lines)"
    ls <- Lazy.lines <$> readFile fileInput

    putStrLn $ "Writing to " <> fileOutput
    appendLine fileOutput $ "# " <> Lazy.pack
        (formatTime defaultTimeLocale "%y%m%d-%T" now)

    for_ ls $ \l -> do
        let hyph = case HashMap.lookup (Lazy.toStrict l) mapExisting of
              Just h -> Lazy.fromStrict h
              Nothing -> Lazy.intercalate "|" $ Lazy.pack <$> KL.hyphenate (KL.languageHyphenator lang') (Lazy.unpack l)
        appendLine fileOutput hyph

    nLO <- wcl fileOutput
    putStrLn $ fileOutput <> " (" <> show nLO <> " lines) written."

sortByFrequency :: OptionsSort -> IO ()
sortByFrequency (OptionsSort fileInput fileFrequencies fileOutput) = do

    nLI <- wcl fileInput
    putStrLn
        $  "Reading words from file "
        <> fileInput
        <> " ("
        <> show nLI
        <> " lines)"

    freqs <- readFrequencies fileFrequencies

    if takeExtension fileInput == ".json"
        then do
            ls <- HashMap.toList . fromMaybe (error "Could not decode json file") <$> Aeson.decodeFileStrict' fileInput

            StrictIO.putStr "Sorting ..."
            let sorted = sortOn (Down <<< freq freqs <<< snd) ls

            writeJSONFile fileOutput sorted
        else do
            ls <- Lazy.lines <$> readFile fileInput

            StrictIO.putStr "Sorting ..."
            let sorted = sortOn
                    (Down <<< freq freqs <<< replace "|" "" <<< head <<< Text.splitOn " " <<< Lazy.toStrict)
                    ls
            StrictIO.putStrLn " done."
            StrictIO.putStrLn $ "Writing file " <> Text.pack fileOutput
            writeFile fileOutput $ Lazy.unlines sorted

readScores :: FilePath -> IO [Double]
readScores file = do
    fmap readDouble . Lazy.lines <$> readFile file
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
