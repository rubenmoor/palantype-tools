{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications   #-}

module Main where

import           Control.Category               ( (<<<)
                                                , Category((.))
                                                )
import           Control.Monad                  ( Monad((>>=))
                                                , when
                                                )
import           Data.Bool                      ( (&&)
                                                , not
                                                )
import           Data.Either                    ( Either(..) )
import           Data.Eq                        ( Eq((/=), (==)) )
import           Data.Foldable                  ( Foldable(maximum)
                                                , for_
                                                , traverse_
                                                )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>)
                                                , Functor(fmap)
                                                )
import           Data.List                      ( filter
                                                , head
                                                , sortOn
                                                )
import           Data.Monoid                    ( (<>)
                                                , Monoid(mconcat)
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Text.Read                 ( double )
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
                                                , takeDirectory
                                                , takeExtension
                                                )
import           System.IO                      ( IO
                                                , hFlush
                                                , putStr
                                                , putStrLn
                                                , stdout
                                                )
import qualified Text.Hyphenation              as KL
import           Text.Show                      ( Show(show) )
import           TextShow                       ( TextShow(showt) )

import           Args                           ( OptionsHyphenate(..)
                                                , OptionsHyphenateMode(..)
                                                , OptionsShowChart
                                                    ( OSCHistScores
                                                    )
                                                , OptionsSort(..)
                                                , Task(..)
                                                , argOpts
                                                )
import           BuildDict                      ( buildDict )
import           Common                         ( appendLine
                                                , fileScores
                                                , freq
                                                , readFrequencies
                                                , removeFiles
                                                , writeJSONFile
                                                )
import qualified Data.Aeson                    as Aeson
import qualified Data.HashMap.Strict           as HashMap
import           Data.Maybe                     ( Maybe(..)
                                                , fromMaybe
                                                )
import           Data.Ord                       ( Down(Down) )
import           Data.Tuple                     ( snd )
import           GHC.Exts                       ( seq )
import           RunPrepare                     ( prepare )
import           WCL                            ( wcl )
import Data.Foldable (all)
import Data.Char (isUpper)

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
    Text.putStrLn $ showt $ RawSteno.parseSteno @DE.Key (RawSteno str)

hyphenate :: OptionsHyphenate -> IO ()
hyphenate (OptionsHyphenate filesHyphenated lang mode) = do
    now <- getCurrentTime

    putStrLn "Reading existing hyphenation files: "
    for_ filesHyphenated $ \file -> do
        nLines <- wcl file
        putStrLn $ file <> " (" <> show nLines <> " lines)"

    let hasContent line = not (Text.null line) && Text.head line /= '#'

    lsExisting <-
        filter hasContent
        .   mconcat
        <$> traverse (fmap Text.lines <<< Text.readFile) filesHyphenated

    let mapExisting =
            HashMap.fromList $ (\w -> (Text.replace "|" "" w, w)) <$> lsExisting

        lang' = case lang of
            DE -> KL.German_1996
            EN -> KL.English_US

        hyphenate' str = case HashMap.lookup str mapExisting of
            Just h  -> h
            Nothing -> if all isUpper $ Text.unpack str

                -- exception: acronym
                then Text.intersperse '|' str
                else Text.intercalate "|" $ Text.pack <$> KL.hyphenate
                    (KL.languageHyphenator lang')
                    (Text.unpack str)

    case mode of
        OHMFile fileInput fileOutput -> do
            removeFiles [fileOutput]

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

            for_ ls $ appendLine fileOutput <<< hyphenate'

            nLO <- wcl fileOutput
            putStrLn $ fileOutput <> " (" <> show nLO <> " lines) written."

        OHMArg str -> do
            when (str `HashMap.member` mapExisting)
                $ putStrLn "Existing hypenation pattern."
            Text.putStrLn $ hyphenate' str

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
                ls <-
                    HashMap.toList
                    .   fromMaybe (error "Could not decode json file")
                    <$> Aeson.decodeFileStrict' file
                putStrLn $ ls `seq` " done."

                putStr "Sorting ..."
                hFlush stdout
                let sorted = sortOn (Down <<< freq freqs <<< snd) ls
                putStrLn " done."

                writeJSONFile file sorted
            else do
                ls <- Text.lines <$> Text.readFile file
                putStrLn $ ls `seq` " done."

                putStr "Sorting ..."
                hFlush stdout
                let
                    sorted = sortOn
                        (   Down
                        <<< freq freqs
                        <<< Text.replace "|" ""
                        <<< head
                        <<< Text.splitOn " "
                        )
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
