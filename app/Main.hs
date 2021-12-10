{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications   #-}

module Main where

import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( (<<<)
                                                , Category((.))
                                                )
import           Control.Monad                  ( Monad((>>=))

                                                )
import           Data.Bool                      ( (&&)
                                                , not
                                                )
import           Data.Either                    ( Either(..) )
import           Data.Eq                        ( Eq((/=)) )
import           Data.Foldable                  ( Foldable(foldl', maximum)
                                                , for_
                                                , traverse_
                                                )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>)
                                                , Functor(fmap)
                                                )
import           Data.List                      ( filter
                                                , sortOn
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
                                                , takeDirectory
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
                                                , fileScores
                                                )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Int                       ( Int )
import           Data.Maybe                     ( fromMaybe, Maybe (..) )
import           Data.Ord                       ( Down(Down) )
import           RunPrepare                     ( prepare )
import           Text.Read
import           WCL                            ( wcl )

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

    putStrLn "Reading existing hyphenation files: "
    traverse_ putStrLn filesHyphenated

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
    appendLine fileOutput $ "# " <> Lazy.pack
        (formatTime defaultTimeLocale "%y%m%d-%T" now)

    for_ ls $ \l -> do
        let hyph = case HashMap.lookup (Lazy.toStrict l) mapExisting of
              Just h -> Lazy.fromStrict h
              Nothing -> Lazy.intercalate "|" $ Lazy.pack <$> KL.hyphenate (KL.languageHyphenator lang') (Lazy.unpack l)
        appendLine fileOutput hyph

sortByFrequency :: OptionsSort -> IO ()
sortByFrequency (OptionsSort fileInput fileFrequencies fileOutput) = do
    ls    <- Lazy.lines <$> readFile fileInput
    freqs <- readFrequencies
    let sorted = sortOn
            (Down <<< freq freqs <<< replace "|" "" <<< Lazy.toStrict)
            ls
    writeFile fileOutput $ Lazy.unlines sorted
  where
    -- | word frequencies from UNI Leipzig based on
    --   35 Mio. sentences
    readFrequencies :: IO (HashMap Text Int)
    readFrequencies = do
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
