{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import           Control.Category               ( (<<<)
                                                , Category((.))
                                                )
import           Control.Monad                  ( Monad((>>=))
                                                , when
                                                , foldM
                                                )
import           Data.Bool                      ( (&&)
                                                , not
                                                )
import           Data.Either                    ( Either(..) )
import           Data.Eq                        ( Eq((/=)) )
import           Data.Ord                       ((<))
import           Data.Foldable                  ( Foldable(maximum)
                                                , all
                                                , for_
                                                , length
                                                , null
                                                )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>)
                                                , (<&>)
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
import           Data.Traversable               ( Traversable(traverse) )
import           GHC.Err                        ( error )
import           GHC.Float                      ( Double )
import           Options.Applicative            ( execParser, Applicative (pure) )
import           Palantype.Common               ( SystemLang(..)
                                                , parseSteno
                                                , dictNumbers
                                                , dictCommands
                                                , dictFKeys
                                                , dictSpecial
                                                , dictPlover
                                                )
import qualified Palantype.DE.Keys             as DE
import Palantype.DE.FingerSpelling (dictFingerSpelling)
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
                                                , OptionsExtraDict (..)
                                                , OptionsFindDuplicates (OptionsFindDuplicates)
                                                , Task(..)
                                                , argOpts
                                                )
import           Common                         ( appendLine
                                                , fileScores
                                                , removeFiles
                                                )
import           Data.Char                      ( isUpper )
import           Data.Maybe                     ( Maybe(..)
                                                , fromJust
                                                )
import           RunPrepare                     ( prepare )
import Sort (sortByFrequency)
import Palantype.Tools.Hyphenate (hyphPseudoSyllable)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import WCL (wcl)
import MakeSteno (makeSteno)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson as Aeson
import qualified Palantype.Common.Indices as KI
import qualified Palantype.EN.Keys as EN
import Data.Bifunctor (Bifunctor(first))
import qualified Palantype.Common.RawSteno as Raw

main :: IO ()
main = execParser argOpts >>= \case
    TaskRawSteno    str  -> rawSteno str
    TaskPrepare     opts -> prepare opts
    TaskHyphenate   opts -> hyphenate opts
    TaskMakeSteno   opts -> makeSteno opts
    TaskSort        opts -> sortByFrequency opts
    TaskShowChart   opts -> showChart opts
    TaskMakeNumbers opts -> makeNumbers opts
    TaskExtraDict   opts -> extraDict opts
    TaskFindDuplicates opts -> findDuplicates opts

rawSteno :: Text -> IO ()
rawSteno str =
    Text.putStrLn $ showt $ parseSteno @DE.Key $ Raw.fromText str

hyphenate :: OptionsHyphenate -> IO ()
hyphenate (OptionsHyphenate filesHyphenated lang mode) = do

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
            SystemDE -> KL.German_1996
            SystemEN -> KL.English_US

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
        SystemDE -> first (KI.toRaw @DE.Key) <$> dictNumbers
        SystemEN -> first (KI.toRaw @EN.Key) <$> dictNumbers
    putStrLn " done."
    nLines <- wcl fileOutput
    putStrLn $ "Written " <> fileOutput <> " (" <> show nLines <> " lines)"

extraDict :: OptionsExtraDict -> IO ()
extraDict (OptionsExtraDict fileOutput lang) = do
    putStr $ "Writing extra dict for fingerspelling, special keys, command \
             \keys, and plover commands to " <> fileOutput <> " ..."
    hFlush stdout
    let dict =  dictFingerSpelling
             <> dictCommands
             <> dictFKeys
             <> dictSpecial
             <> dictPlover
    LBS.writeFile fileOutput $ Aeson.encodePretty $ Map.fromList $ case lang of
        SystemDE -> first (KI.toRaw @DE.Key) <$> dict
        SystemEN -> first (KI.toRaw @EN.Key) <$> dict
    putStrLn " done."
    nLines <- wcl fileOutput
    putStrLn $ "Written " <> fileOutput <> " (" <> show nLines <> " lines)"


findDuplicates :: OptionsFindDuplicates -> IO ()
findDuplicates (OptionsFindDuplicates files) = do
    when (length files < 2) $ error "At least two files required"
    (_, dupls) <- foldM acc (Map.empty, []) files
    if null dupls
      then putStrLn "No duplicates"
      else do putStrLn "Duplicates found: "
              for_ dupls \(k, (v1, v2)) ->
                Text.putStrLn $ k <> ":\t" <> v1 <> "\t" <> v2
  where
    acc :: (Map Text Text, [(Text, (Text, Text))]) -> FilePath -> IO (Map Text Text, [(Text, (Text, Text))])
    acc (map, dupls) file = do
      m <- Aeson.decodeFileStrict file >>= \case
        Nothing -> error $ "Could not decode JSON of " <> file
        Just m  -> pure m
      let duplKeys = Map.keys $ map `Map.intersection` m
          duplLs = duplKeys <&> \k -> (k, (fromJust $ Map.lookup k map, fromJust $ Map.lookup k m))
      pure (map `Map.union` m, dupls <> duplLs)
