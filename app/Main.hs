{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications   #-}

module Main where

import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( (<<<)
                                                , Category((.))
                                                )
import           Control.Monad                  ( Monad((>>=))
                                                , unless
                                                )
import           Data.Bool                      ( (&&)
                                                , not
                                                )
import           Data.Either                    ( Either(..) )
import           Data.Eq                        ( Eq((/=)) )
import           Data.Foldable                  ( Foldable(maximum)
                                                , for_
                                                , traverse_
                                                )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>)
                                                , Functor(fmap)
                                                )
import qualified Data.HashSet                  as HashSet
import           Data.List                      ( filter )
import           Data.Monoid                    ( (<>)
                                                , Monoid(mconcat)
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as StrictIO
import qualified Data.Text.Lazy                as Lazy
import           Data.Text.Lazy.IO              ( readFile )
import           Data.Text.Lazy.Read            ( double )
import           Data.Time                      ( UTCTime )
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
                                                , OptionsSort
                                                , Task(..)
                                                , argOpts
                                                )
import           BuildDict                      ( buildDict )
import           Common                         ( appendLine, fileScores )
import           RunPrepare                     ( prepare )
import           WCL                            ( wcl )

main :: IO ()
main = execParser argOpts >>= \case
    TaskRawSteno  str  -> rawSteno str
    TaskPrepare   opts -> prepare opts
    TaskHyphenate opts -> hyphenate opts
    TaskStenoDict opts -> buildDict opts
    TaskSort      opts -> sortByFrequency opts

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
    setExisting <-
        HashSet.fromList
        .   filter hasContent
        .   mconcat
        <$> traverse (fmap Lazy.lines <<< readFile) filesHyphenated

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

    for_ ls $ \l ->
        unless (HashSet.member l setExisting)
            $   appendLine fileOutput
            $   Lazy.intercalate "|"
            $   Lazy.pack
            <$> KL.hyphenate (KL.languageHyphenator lang') (Lazy.unpack l)

sortByFrequency :: OptionsSort -> IO ()
sortByFrequency _ = pure ()

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
