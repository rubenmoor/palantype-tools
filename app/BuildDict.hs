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
import           Control.Category               ( (<<<)
                                                , Category((.))
                                                )
import           Control.Concurrent             ( getNumCapabilities )
import           Control.Concurrent.Async       ( forConcurrently )
import qualified Control.Concurrent.Lock       as Lock
import           Control.Monad                  ( (<$!>)
                                                , when
                                                )
import qualified Data.Aeson                    as Aeson
import           Data.Either                    ( Either(..) )
import           Data.Foldable                  ( Foldable(foldl')
                                                , for_
                                                , traverse_
                                                )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( ($>)
                                                , (<$>)
                                                )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Int                       ( Int )
import           Data.Maybe                     ( Maybe(..)
                                                , fromMaybe
                                                )
import           Data.Monoid                    ( (<>)
                                                , mconcat
                                                )
import           Data.Strict.Tuple              ( Pair((:!:)) )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as Text
import           Data.Traversable               ( traverse )
import qualified Data.Vector                   as Vector
import           Data.Vector                    ( (++)
                                                , Vector
                                                )
import qualified Data.Vector.Split             as Vector
import           Formatting                     ( (%)
                                                , fprint
                                                )
import           Formatting.Clock               ( timeSpecs )
import           GHC.Err                        ( error )
import           GHC.Exts                       ( seq )
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
                                                , hFlush
                                                , print
                                                , putStr
                                                , putStrLn
                                                , stdout
                                                )
import           Text.Show                      ( Show(show) )
import           TextShow                       ( TextShow(showt) )
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
        Left  err -> Text.putStrLn $ showt err
        Right sds -> traverse_ (Text.putStrLn <<< showt) sds

buildDict (OStDFile fileInput fileOutput bAppend lang) = do

    start <- getTime Monotonic

    let lsFiles =
            [fileDictNoParse, fileDictDuplicates, fileLost, fileCollisions]
    removeFiles lsFiles

    -- initial state with existing steno in output file
    mapStenoWord     <- if bAppend
        then do
            nLO <- wcl fileOutput
            putStr
                $  "Reading data from output file: "
                <> fileOutput
                <> " ("
                <> show nLO
                <> " lines) ..."
            hFlush stdout
            mMap <- Aeson.decodeFileStrict' fileOutput
            putStrLn $ mMap `seq` " done."
            pure $ fromMaybe (error "Could not decode file.") mMap
        else pure HashMap.empty
    let accFlip m (steno, word) =
            HashMap.insertWith (++) word (Vector.singleton steno) m
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

        putStr $ "Reading input file " <> fileInput <> " ..."
        hFlush stdout
        vecWord <- Vector.fromList . Text.lines <$> Text.readFile fileInput
        let l = Vector.length vecWord
        putStrLn $ l `seq` " done."

        putStrLn $ "Creating steno chords for " <> show l <> " entries."

        let parseSeries' = case lang of
                DE -> parseSeries @DE.Key
                EN -> parseSeries @EN.Key

        nj <- getNumCapabilities
        putStr $ "\nRunning " <> show nj <> " jobs.\n\n"
        putStr "Optimizing steno chords ..."
        hFlush stdout

        lock <- Lock.new

        let d    = ceiling $ (fromIntegral l :: Double) / fromIntegral nj
            jobs = Vector.chunksOf d vecWord

            parseWord hyph = do
                let word      = Text.replace "|" "" hyph
                    eRaws     = Vector.fromList <$!> parseSeries' hyph
                    mExisting = HashMap.lookup word mapWordStenosExisting

                case (eRaws, mExisting) of
                    (_           , Just stenos) -> pure (hyph :!: stenos)
                    (Right stenos, _          ) -> pure (hyph :!: stenos)
                    (Left (PEExceptionTable orig), _) ->
                        print ("Error in exception table for: " <> orig)
                            $> (hyph :!: Vector.empty)
                    (Left (PEParsec raw _), _) -> do
                        Lock.with lock
                            $  appendLine fileDictNoParse (Text.unwords [word, hyph, showt raw])
                            $> (hyph :!: Vector.empty)

        vecStenos <- mconcat <$> forConcurrently jobs (traverse parseWord)
        putStrLn $ vecStenos `seq` " done."

        let
            --      collision resolution stays sequential
            accDict :: DictState -> Pair Text (Vector RawSteno) -> IO DictState
            accDict dst@DictState {..} (hyph :!: raws) = do
                let word       = Text.replace "|" "" hyph
                    mDuplicate = HashMap.lookup word dstMapWordStenos

                case mDuplicate of
                    Just _  -> appendLine fileDictDuplicates word $> dst
                    Nothing -> do
                        let (dst', isLost) =
                                Collision.resolve word (Vector.toList raws) dst
                        when isLost
                            $  appendLine fileCollisions
                            $  word
                            <> " "
                            <> Text.intercalate
                                   " "
                                   (Vector.toList $ showt <$> raws)
                        pure dst'

        putStr "Resolving collisions ..."
        hFlush stdout
        DictState {..} <- Vector.foldM'
            accDict
            (DictState HashMap.empty HashMap.empty)
            vecStenos
        putStrLn $ dstMapWordStenos `seq` " done."

        -- checking for lost words
        putStr $ "Writing lost words to " <> fileLost <> " ..."
        u <- for_ vecWord $ \w ->
            case HashMap.lookup (Text.replace "|" "" w) dstMapWordStenos of
                Just _  -> pure ()
                Nothing -> appendLine fileLost w
        putStrLn $ u `seq` " done."

        removeFiles [fileOutput]

        -- TODO: write to output file, use exception handling for ctrl + c
        writeJSONFile fileOutput $ HashMap.toList dstMapStenoWord

        nLO <- wcl fileOutput
        Text.putStrLn
            $  "Written "
            <> Text.pack fileOutput
            <> " ( "
            <> showt nLO
            <> " lines)"

        -- TODO scoring stats
        -- pure $ HashMap.toList stMapWordStenos <&> \(_, sds) ->
        --     maximum $ scorePrimary . sdScore <$> sds
