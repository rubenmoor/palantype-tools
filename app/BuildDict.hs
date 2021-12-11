{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

module BuildDict where

import           Args                           ( OptionsStenoDict(..) )
import           Common                         ( appendLine
                                                , removeFiles
                                                , writeJSONFile
                                                )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( (<<<), Category ((.)) )
import           Control.Concurrent             ( getNumCapabilities )
import           Control.Concurrent.Async       ( forConcurrently )
import           Control.Monad                  ( foldM
                                                , when
                                                )
import qualified Data.Aeson                    as Aeson
import           Data.Bool                      ( Bool(..) )
import           Data.Either                    ( Either(..) )
import           Data.Foldable                  ( Foldable(foldl', length)
                                                , for_
                                                , traverse_
                                                )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( ($>)
                                                , (<$>)
                                                , Functor(fmap)
                                                )
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.HashSet                  as HashSet
import           Data.Int                       ( Int )
import           Data.List                      ( (++) )
import           Data.List.Split                ( chunksOf )
import           Data.Maybe                     ( Maybe(..)
                                                , catMaybes
                                                , fromMaybe
                                                )
import           Data.Monoid                    ( (<>)
                                                , Monoid(mconcat)
                                                )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as StrictIO
import qualified Data.Text.Lazy                as Lazy
import           Data.Text.Lazy.IO              ( readFile )
import           Data.Traversable               ( for )
import           Formatting                     ( (%)
                                                , fprint
                                                )
import           Formatting.Clock               ( timeSpecs )
import           GHC.Err                        ( error )
import           GHC.Float                      ( Double )
import           GHC.Num                        ( (+) )
import           GHC.Real                       ( Fractional((/))
                                                , Real
                                                , RealFrac(ceiling)
                                                , fromIntegral
                                                , realToFrac
                                                )
import           Palantype.Common               ( Lang(DE, EN) )
import           Palantype.Common.RawSteno      ( RawSteno )
import qualified Palantype.DE.Keys             as DE
import qualified Palantype.EN.Keys             as EN
import qualified Palantype.Tools.Collision     as Collision
import           Palantype.Tools.Collision      ( DictState(..) )
import           Palantype.Tools.Steno          ( ParseError(..)
                                                , parseSeries
                                                )
import           System.Clock                   ( Clock(Monotonic)
                                                , getTime
                                                )
import           System.Directory               ( doesFileExist )
import           System.IO                      ( FilePath
                                                , IO
                                                , print
                                                , putStr
                                                , putStrLn
                                                )
import           Text.Show                      ( Show(show) )
import           TextShow                       ( TextShow(showt)
                                                , showtl
                                                )
import           WCL                            ( wcl )

fileDictDuplicates :: FilePath
fileDictDuplicates = "buildDict-duplicates.txt"

fileLost :: FilePath
fileLost = "buildDict-lostwords.txt"

fileCollisions :: FilePath
fileCollisions = "buildDict-collisions.txt"

average :: forall a t . (Real a, Foldable t) => t a -> Double
average ls =
    let (t, n) = foldl' (\(!b, !c) a -> (a + b, (c :: Int) + 1)) (0, 0) ls
    in  realToFrac t / realToFrac n

buildDict :: OptionsStenoDict -> IO ()
buildDict (OStDArg lang str) = do
    let parseSeries' = case lang of
            DE -> parseSeries @DE.Key
            EN -> parseSeries @EN.Key

    case parseSeries' str of
        Left  err -> StrictIO.putStrLn $ showt err
        Right sds -> traverse_ (StrictIO.putStrLn <<< showt) sds

buildDict (OStDFile fileInput fileOutput lang) = do

    start <- getTime Monotonic

    let lsFiles =
            [fileDictNoParse, fileDictDuplicates, fileLost, fileCollisions]
    removeFiles lsFiles

    -- initial state with existing steno in output file
    fileExistsOutput <- doesFileExist fileOutput
    mapStenoWord     <- if fileExistsOutput
        then do
            nLO <- wcl fileOutput
            putStrLn
                $  "Reading data from output file: "
                <> fileOutput
                <> " ("
                <> show nLO
                <> " lines)."
            putStrLn "If this is undesired, delete the file first."
            mMap <- Aeson.decodeFileStrict' fileOutput
            pure $ fromMaybe (error "Could not decode file.") mMap
        else pure HashMap.empty
    let accFlip m (steno, word) = HashMap.insertWith (++) word [steno] m
        mapWordStenos =
            foldl' accFlip HashMap.empty $ HashMap.toList mapStenoWord

    runBuildDict mapWordStenos

    putStrLn ""
    -- putStrLn $ "Number of words with steno code: " <> show (length newScores)

    putStrLn "Number of lines in"

    for_ (fileInput : fileOutput : lsFiles) $ \file -> do
        exists <- doesFileExist file
        when exists $ do
            nl <- wcl file
            putStrLn $ show nl <> "\t" <> file

    -- TODO: scoring: move elsewhere
    -- nNoParse <- wcl fileDictNoParse
    -- let newZeroScores = replicate nNoParse (0 :: Double)

    --     scores        = newZeroScores <> (fromRational <$> newScores)
    --     meanScore     = average scores
    -- writeFile (fileScores now)
    --     $ Lazy.unlines (Lazy.fromStrict . showt <$> scores)

    -- putStrLn ""
    -- StrictIO.putStrLn $ "Average score: " <> showt meanScore

    putStrLn ""

    stop <- getTime Monotonic
    putStr "StenoWords runtime: "
    fprint (timeSpecs % "\n") start stop

  where
    fileDictNoParse = "buildDict-noparse.txt"

    runBuildDict mapWordStenosExisting = do

        ls <- Lazy.lines <$> readFile fileInput
        let l = length ls

        putStrLn $ "Creating steno chords for " <> show l <> " entries."

        let parseSeries' = case lang of
                DE -> parseSeries @DE.Key
                EN -> parseSeries @EN.Key

        nj <- getNumCapabilities
        StrictIO.putStr $ "\nRunning " <> showt nj <> " jobs.\n\n"
        StrictIO.putStrLn "Optimizing steno chords ..."

        let d    = ceiling $ (fromIntegral l :: Double) / fromIntegral nj
            jobs = chunksOf d ls

        lsStenos <- fmap mconcat $ forConcurrently jobs $ \hyphs ->
            fmap catMaybes $ for hyphs $ \hyph -> do
                let hyph'     = Lazy.toStrict hyph
                    word'     = Text.replace "|" "" hyph'
                    word      = Lazy.fromStrict word'
                    eRaws     = parseSeries' hyph'
                    mExisting = HashMap.lookup word' mapWordStenosExisting

                case (eRaws, mExisting) of
                    (_           , Just stenos) -> pure $ Just (hyph', stenos)
                    (Right stenos, _          ) -> pure $ Just (hyph', stenos)
                    (Left (PEExceptionTable orig), _) ->
                        print ("Error in exception table for: " <> orig)
                            $> Nothing
                    (Left (PEParsec _ _), _) ->
                        appendLine fileDictNoParse word $> Nothing

        let
            --      collision resolution stays sequential
            accDict :: DictState -> (Text, [RawSteno]) -> IO DictState
            accDict dst@DictState {..} (hyph', raws) = do
                let word'      = Text.replace "|" "" hyph'
                    word       = Lazy.fromStrict word'
                    mDuplicate = HashMap.lookup word' dstMapWordStenos

                case mDuplicate of
                    Just _  -> appendLine fileDictDuplicates word $> dst
                    Nothing -> do
                        let (_, dst', isLost) = foldl'
                                (Collision.resolve word')
                                (length raws, dst, False)
                                raws
                        when isLost
                            $  appendLine fileCollisions
                            $  word
                            <> " "
                            <> Lazy.intercalate " " (showtl <$> raws)
                        pure dst'

        StrictIO.putStrLn "Resolving collisions ..."
        DictState {..} <- foldM accDict
                                (DictState HashMap.empty HashMap.empty)
                                lsStenos

        -- checking for lost words
        putStrLn $ "Writing lost words to " <> fileLost
        let setAll =
                HashSet.fromList $ Text.replace "|" "" . Lazy.toStrict <$> ls
        StrictIO.putStrLn $ "Number of unique words: " <> showt
            (HashSet.size setAll)
        for_ (HashSet.toList setAll) $ \w ->
            case HashMap.lookup w dstMapWordStenos of
                Just _  -> pure ()
                Nothing -> appendLine fileLost (Lazy.fromStrict w)

        removeFiles [fileOutput]

        -- TODO: write to output file, use exception handling for ctrl + c
        writeJSONFile fileOutput $ HashMap.toList dstMapStenoWord

        nLO <- wcl fileOutput
        StrictIO.putStrLn
            $  "Written "
            <> Text.pack fileOutput
            <> " ( "
            <> showt nLO
            <> " lines)"
        -- TODO
        -- check for double entries in the final result

        -- TODO scoring stats
        -- pure $ HashMap.toList stMapWordStenos <&> \(_, sds) ->
        --     maximum $ scorePrimary . sdScore <$> sds
