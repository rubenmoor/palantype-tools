{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

module BuildDict where

import           Args                           ( OptionsStenoDict (..)
                                                )
import           Common                         ( appendLine
                                                , removeFiles, fileScores
                                                )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( Category((.)), (<<<)
                                                )
import           Control.Monad                  ( foldM



                                                , when
                                                )
import           Data.Either                    ( Either(..) )
import           Data.Foldable                  ( Foldable
                                                    ( foldl'
                                                    , length
                                                    , maximum
                                                    )
                                                , for_, traverse_

                                                )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>)
                                                , (<&>)

                                                )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Int                       ( Int )
import           Data.List                      ( replicate




                                                )
import           Data.Monoid                    ( (<>)

                                                )
import           Data.Text                      ( Text


                                                )
import qualified Data.Text.IO                  as StrictIO
import qualified Data.Text.Lazy                as Lazy
import           Data.Text.Lazy.IO              ( readFile
                                                , writeFile
                                                )
import           Data.Time.Clock                ( getCurrentTime
                                                )
import           Formatting                     ( (%)
                                                , fprint
                                                )
import           Formatting.Clock               ( timeSpecs )
import           GHC.Float                      ( Double )
import           GHC.Num                        ( (+) )
import           GHC.Real                       ( Fractional((/), fromRational)
                                                , Real

                                                , realToFrac
                                                )
import           Palantype.Common               ( Lang(DE, EN)

                                                )
import           Palantype.Common.RawSteno      ( RawSteno(RawSteno) )
import qualified Palantype.DE.Keys             as DE
import           Palantype.Tools.Steno          ( ParseError(..)

                                                , Score(scorePrimary)
                                                , SeriesData(..)
                                                , parseSeries
                                                )
import           System.Clock                   ( Clock(Monotonic)
                                                , getTime
                                                )
import           System.Directory               ( doesFileExist

                                                , renameFile
                                                )
import           System.IO                      ( FilePath

                                                , IO




                                                , print
                                                , putStr
                                                , putStrLn
                                                )
import           Text.Show                      ( Show(show) )
import           TextShow                       ( TextShow(showt) )
import           WCL                            ( wcl )
import qualified Palantype.EN.Keys as EN

fileDictDuplicates :: FilePath
fileDictDuplicates = "buildDict-duplicates.txt"

fileCollisions :: FilePath
fileCollisions = "buildDict-collisions-v1.txt"

fileCollisionsV2 :: FilePath
fileCollisionsV2 = "buildDict-collisions-v2.txt"

average :: forall a t . (Real a, Foldable t) => t a -> Double
average ls =
    let (t, n) = foldl' (\(!b, !c) a -> (a + b, (c :: Int) + 1)) (0, 0) ls
    in  realToFrac t / realToFrac n

data State = State
    { stMapWordStenos :: HashMap Text [SeriesData]
    , stMapStenoWord :: HashMap RawSteno Text
    }

buildDict :: OptionsStenoDict -> IO ()
buildDict (OStDArg lang str) = do
    let
        parseSeries' = case lang of
          DE -> parseSeries @DE.Key
          EN -> parseSeries @EN.Key

    case parseSeries' str of
        Left  err -> StrictIO.putStrLn $ showt err
        Right sds -> traverse_ (StrictIO.putStrLn <<< showt) sds

buildDict (OStDFile fileInput fileOutput lang) = do

    start <- getTime Monotonic
    now   <- getCurrentTime

    let lsFiles =
            [ fileDictNoParse
            , fileDictDuplicates
            , fileCollisions
            , fileCollisionsV2
            ]
    removeFiles lsFiles

    newScores <- runBuildDict

    putStrLn ""
    putStrLn $ "Number of words with steno code: " <> show (length newScores)

    putStrLn "Number of lines in"

    for_ (fileInput : fileOutput : lsFiles) $ \file -> do
        exists <- doesFileExist file
        when exists $ do
            nl <- wcl file
            putStrLn $ show nl <> "\t" <> file

    nNoParse <- wcl fileDictNoParse
    let newZeroScores = replicate nNoParse (0 :: Double)

        scores        = newZeroScores <> (fromRational <$> newScores)
        meanScore     = average scores
    writeFile (fileScores now)
        $ Lazy.unlines (Lazy.fromStrict . showt <$> scores)

    putStrLn ""
    StrictIO.putStrLn $ "Average score: " <> showt meanScore

    putStrLn ""

    stop <- getTime Monotonic
    putStr "StenoWords runtime: "
    fprint (timeSpecs % "\n") start stop

  where
    fileDictNoParse = "buildDict-noparse.txt"
    fileOutputTmp   = "buildDict-tmp.txt"

    runBuildDict         = do

        ls <- Lazy.lines <$> readFile fileInput
        let
            l = length ls

            formatJSONLine raw word =
                Lazy.fromStrict $ "\"" <> showt raw <> "\": \"" <> word <> "\""

        putStrLn $ "Creating steno chords for " <> show l <> " entries."

        let
            parseSeries' = case lang of
              DE -> parseSeries @DE.Key
              EN -> parseSeries @EN.Key

            acc
                :: State
                -> Lazy.Text
                -> IO State
            acc st@State {..} str = do
                let str' = Lazy.toStrict str
                case parseSeries' str' of
                    Left err -> do
                      case err of
                        PEExceptionTable orig -> print $ "Error in exception table for: " <> orig
                        PEParsec _ _ -> appendLine fileDictNoParse str
                      pure st
                    Right sds -> do
                        -- TODO: collision detection
                        -- write to output file
                        for_ sds $ appendLine fileOutput <<< formatJSONLine str' <<< showt <<< sdRawSteno

                        pure $ st
                            { stMapWordStenos = HashMap.insert str' sds stMapWordStenos
                            }

        appendLine fileOutputTmp "{\n"
        State {..} <- foldM acc (State HashMap.empty HashMap.empty) ls
        appendLine fileOutputTmp "\n}\n"
        renameFile fileOutputTmp fileOutput
        removeFiles [fileOutputTmp]

        -- TODO
        -- check for double entries in the final result

        pure $ HashMap.toList stMapWordStenos <&> \(_, sds) ->
            maximum $ scorePrimary . sdScore <$> sds
