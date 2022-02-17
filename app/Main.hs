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
import           Data.Eq                        ( Eq((/=)) )
import           Data.Foldable                  ( Foldable(maximum)
                                                , all
                                                , for_
                                                )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>)
                                                , Functor(fmap)
                                                )
import           Data.List                      ( filter
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
import           Options.Applicative            ( execParser, Applicative (pure) )
import           Palantype.Common               ( Lang(DE, EN) , RawSteno (..)
                                                , parseSteno
                                                , dictNumbers
                                                )
import qualified Palantype.DE.Keys             as DE
import           System.Directory               ( listDirectory )
import           System.FilePath                ( (</>)
                                                , FilePath
                                                , takeDirectory
                                                )
import           System.IO                      ( IO
                                                , putStrLn, hFlush, stdout, putStr
                                                )
import qualified Text.Hyphenation              as KL
import           Text.Show                      ( Show(show) )
import           TextShow                       ( TextShow(showt) )

import           Args                           ( OptionsHyphenate(..)
                                                , OptionsHyphenateMode(..)
                                                , OptionsShowChart
                                                    ( OSCHistScores
                                                    )
                                                , OptionsMakeNumbers (..)
                                                , Task(..)
                                                , argOpts
                                                )
import           Common                         ( appendLine
                                                , fileScores
                                                , removeFiles
                                                )
import           Data.Char                      ( isUpper )
import           Data.Maybe                     ( Maybe(..)
                                                )
import           RunPrepare                     ( prepare )
import Sort (sortByFrequency)
import Palantype.Tools.Hyphenate (hyphPseudoSyllable)
import qualified Data.Map.Strict as Map
import WCL (wcl)
import MakeSteno (makeSteno)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Palantype.Common.Indices as KI
import qualified Palantype.EN.Keys as EN
import Data.Bifunctor (Bifunctor(first))

main :: IO ()
main = execParser argOpts >>= \case
    TaskRawSteno    str  -> rawSteno str
    TaskPrepare     opts -> prepare opts
    TaskHyphenate   opts -> hyphenate opts
    TaskMakeSteno   opts -> makeSteno opts
    TaskSort        opts -> sortByFrequency opts
    TaskShowChart   opts -> showChart opts
    TaskMakeNumbers opts -> makeNumbers opts

rawSteno :: Text -> IO ()
rawSteno str =
    Text.putStrLn $ showt $ parseSteno @DE.Key (RawSteno str)

hyphenate :: OptionsHyphenate -> IO ()
hyphenate (OptionsHyphenate filesHyphenated lang mode) = do
    now <- getCurrentTime

    putStrLn "Reading existing hyphenation files ..."
    hFlush stdout
    for_ filesHyphenated $ \file -> do
        nLines <- wcl file
        putStrLn $ file <> " (" <> show nLines <> " lines)"

    let hasContent line = not (Text.null line) && Text.head line /= '#'

    lsExisting <-
        filter hasContent
        .   mconcat
        <$> traverse (fmap Text.lines <<< Text.readFile) filesHyphenated

    let mapExisting =
            Map.fromList $ (\w -> (Text.replace "|" "" w, w)) <$> lsExisting

        lang' = case lang of
            DE -> KL.German_1996
            EN -> KL.English_US

        hyphenate' str = case Map.lookup str mapExisting of
            Just h  -> h
            Nothing -> if all isUpper $ Text.unpack str

                -- exception: acronym
                then Text.intersperse '|' str
                else
                    let
                        klhyph = KL.hyphenate (KL.languageHyphenator lang')
                            $ Text.unpack str
                    in
                        Text.intercalate "|" $ hyphPseudoSyllable klhyph

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
            when (str `Map.member` mapExisting)
                $ putStrLn "Existing hypenation pattern."
            Text.putStrLn $ hyphenate' str

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
    -- plotScoresShow scores
    pure ()

makeNumbers :: OptionsMakeNumbers -> IO ()
makeNumbers (OptionsMakeNumbers fileOutput lang) = do
    putStr $ "Writing numbers dict to " <> fileOutput <> " ..."
    hFlush stdout
    LBS.writeFile fileOutput $ Aeson.encodePretty $ Map.fromList $ case lang of
        DE -> first (KI.toRaw @DE.Key) <$> dictNumbers
        EN -> first (KI.toRaw @EN.Key) <$> dictNumbers
    putStrLn " done."
    nLines <- wcl fileOutput
    putStrLn $ "Written " <> fileOutput <> " (" <> show nLines <> " lines)"
