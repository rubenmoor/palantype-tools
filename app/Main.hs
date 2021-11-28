{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Args                           ( OptionsStenoWords(..)
                                                , OptionsStenoWordsRun(..)
                                                , OptionsSyllables
                                                    ( OSylArg
                                                    , OSylFile
                                                    )
                                                , Task(..)
                                                , argOpts, OptionsFrequency (..)
                                                )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( Category((.), id) )
import           Control.Concurrent.Async       ( forConcurrently )
import           Control.Monad                  ( Monad((>>=))
                                                , foldM
                                                , forever
                                                , when, foldM_
                                                )
import           Data.Bool                      ( Bool(..) )
import           Data.Char                      ( isUpper, isLetter )
import           Data.Either                    ( Either(..) )
import           Data.Eq                        ( (/=)
                                                , Eq((==))
                                                )
import           Data.Foldable                  ( Foldable
                                                    ( foldl'
                                                    , length
                                                    , maximum
                                                    )
                                                , for_
                                                )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$)
                                                , (<$>)
                                                , (<&>)
                                                , Functor(fmap)
                                                )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.HashSet                  as HashSet
import           Data.Int                       ( Int )
import           Data.List                      ( (++)
                                                , concatMap
                                                , head
                                                , replicate
                                                , sortOn
                                                , tail
                                                , take
                                                , zip
                                                )
import           Data.List.Split                ( chunksOf )
import           Data.Maybe                     ( Maybe(Just, Nothing)
                                                , catMaybes
                                                , maybeToList, fromMaybe
                                                )
import           Data.Monoid                    ( (<>)
                                                , Monoid(mconcat, mempty)
                                                )
import           Data.Ord                       ( Ord((>))
                                                , Down (Down)
                                                )
import           Data.Text                      ( Text
                                                , splitOn
                                                , toLower
                                                )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as StrictIO
import qualified Data.Text.Lazy                as Lazy
import           Data.Text.Lazy.IO              ( appendFile
                                                , getLine
                                                , hGetContents
                                                , hPutStrLn
                                                , readFile
                                                , writeFile
                                                )
import           Data.Text.Lazy.Read            ( double )
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                )
import           Data.Traversable               ( for )
import           Data.Tuple                     ( fst
                                                , snd
                                                )
import           Formatting                     ( (%)
                                                , fprint
                                                )
import           Formatting.Clock               ( timeSpecs )
import           GHC.Conc                       ( getNumCapabilities )
import           GHC.Err                        ( error )
import           GHC.Float                      ( Double )
import           GHC.Num                        ( (+) )
import           GHC.Real                       ( Fractional((/), fromRational)
                                                , Real
                                                , fromIntegral
                                                , realToFrac
                                                )
import           Options.Applicative            ( execParser )
import           Palantype.Common.RawSteno      ( RawSteno(RawSteno) )
import qualified Palantype.Common.RawSteno     as RawSteno
import qualified Palantype.DE.Keys             as DE
import           Palantype.Tools.Collision      ( StateCollision(..)
                                                , freq
                                                , readFrequencies
                                                , resolveCollisions
                                                )
import           Palantype.Tools.Statistics     ( plotScoresShow )
import           Palantype.Tools.Steno          ( ParseError(..)
                                                , PartsData(..)
                                                , Path
                                                , Score(scorePrimary)
                                                , SeriesData(..)
                                                , parseSeries
                                                , partsToSteno
                                                )
import           Palantype.Tools.Syllables      ( Exception(..)
                                                , Result(..)
                                                , SyllableData(..)
                                                , parseSyllables
                                                , sdWord
                                                )
import           Prelude                        ( Applicative((*>))
                                                , RealFrac(ceiling)
                                                , Semigroup
                                                )
import           System.Clock                   ( Clock(Monotonic)
                                                , getTime
                                                )
import           System.Directory               ( doesFileExist
                                                , listDirectory
                                                , removeFile

                                                )
import           System.FilePath                ( (</>)
                                                , takeDirectory
                                                )
import           System.IO                      ( FilePath
                                                , Handle
                                                , IO
                                                , IOMode
                                                    ( ReadMode
                                                    , ReadWriteMode
                                                    )
                                                , SeekMode(AbsoluteSeek)
                                                , hSeek
                                                , hSetNewlineMode
                                                , openFile
                                                , print
                                                , putStr
                                                , putStrLn
                                                , universalNewlineMode
                                                )
import           Text.Show                      ( Show(show) )
import           TextShow                       ( TextShow(showt) )
import           WCL                            ( wcl )
import Palantype.Common (Palantype)

main :: IO ()
main = execParser argOpts >>= \case
    TaskRawSteno   str  -> rawSteno str
    TaskSyllables  opts -> syllables opts
    TaskStenoWords opts -> stenoWords opts
    TaskGermanDict _    -> pure ()
    TaskFrequency  c    -> frequency c

fileSyllableEntries :: FilePath
fileSyllableEntries = "entries.txt"

fileSyllableEntriesExtra :: FilePath
fileSyllableEntriesExtra = "entries-extra.txt"

fileSyllables :: FilePath
fileSyllables = "syllables.txt"

rawSteno :: Text -> IO ()
rawSteno str =
    StrictIO.putStrLn $ showt $ RawSteno.parseSteno @DE.Key (RawSteno str)

data StateSyllables = StateSyllables
    { stsyllLastResult :: Result
    , stsyllMap        :: HashMap Text SyllableData
    }

initialState :: StateSyllables
initialState = StateSyllables (Success $ SyllableData "" []) HashMap.empty

syllables :: OptionsSyllables -> IO ()
syllables (OSylArg str) = do
    freqs <- readFrequencies
    for_ (parseSyllables str) $ \case
        Success sd -> do
            let f = freq freqs (sdWord sd)
            StrictIO.putStrLn $ showt sd <> " " <> showt f
        other -> StrictIO.putStrLn $ showt other

syllables OSylFile = do
    start <- getTime Monotonic
    let lsFiles =
            [ fileSyllables
            , fileSyllablesNoParse
            , fileSyllablesAbbreviations
            , fileSyllablesMultiple
            , fileSyllablesSpecialChar
            , fileSyllablesSingleLetter
            , fileSyllablesEllipsis
            , fileSyllablesAcronyms
            , fileSyllablesExplicitExceptions
            ]
    for_ lsFiles $ \file -> do
        exists <- doesFileExist file
        when exists $ do
            putStrLn $ "Deleting " <> file
            removeFile file

    runSyllables

    putStrLn ""
    putStrLn "Number of lines in"

    for_ (fileSyllableEntries : fileSyllableEntriesExtra : lsFiles) $ \file -> do
        nl <- wcl file
        putStrLn $ show nl <> "\t" <> file

    putStrLn ""
    stop <- getTime Monotonic
    putStr "Syllables runtime: "
    fprint (timeSpecs % "\n") start stop

  where
    fileSyllablesAbbreviations      = "syllables-abbreviations.txt"
    fileSyllablesMultiple           = "syllables-multiple.txt"
    fileSyllablesSpecialChar        = "syllables-specialchar.txt"
    fileSyllablesSingleLetter       = "syllables-singleletter.txt"
    fileSyllablesEllipsis           = "syllables-ellipsis.txt"
    fileSyllablesAcronyms           = "syllables-acronyms.txt"
    fileSyllablesNoParse            = "syllables-noparse.txt"
    fileSyllablesExplicitExceptions = "syllables-explicitexceptions.txt"

    runSyllables = do

        handle <- openFile fileSyllableEntries ReadMode
        hSetNewlineMode handle universalNewlineMode
        entries <- Lazy.lines <$> hGetContents handle
        entriesExtra <- Lazy.lines <$> readFile fileSyllableEntriesExtra
        let entriesAll = entriesExtra <> entries

        let
            -- | in case of duplicate entries because of
            --   capitalization, e.g. "Rechnen" and "rechnen",
            --   select the lower-case version
            preferLC :: SyllableData -> SyllableData -> SyllableData

            -- real duplicate
            preferLC s1 s2 | s1 == s2 = s1

            -- weird entry
            -- more syllables means, better entry
            preferLC s1@(SyllableData w1 ss1) s2@(SyllableData w2 ss2)
                | w1 == w2 = if length ss1 > length ss2 then s1 else s2

            preferLC s1@(SyllableData w1 _) s2@(SyllableData w2 _) =
                let
                    c1 = Text.head w1
                    c2 = Text.head w2
                in
                    if toLower w1 /= toLower w2
                        then error $ "preferLC: impossible1: " <> Text.unpack
                            (w1 <> " " <> w2)
                        else case (isUpper c1, isUpper c2) of
                            (True , False) -> s2
                            (False, True ) -> s1
                            _ ->
                                error
                                    $  Text.unpack
                                    $  "preferLC: impossible2: "
                                    <> showt s1
                                    <> " "
                                    <> showt s2

            acc st entry = do
                -- eliminate duplicate entries after parsing
                let lsHyphenated = parseSyllables $ Lazy.toStrict entry
                    hyphenated   = head lsHyphenated
                    last         = stsyllLastResult st
                    m            = stsyllMap st
                m' <- if hyphenated == last
                    then pure HashMap.empty
                    else do
                        lsMWordSyllableData <- for lsHyphenated $ \case
                            Failure err -> do
                                print err
                                appendLine fileSyllablesNoParse entry
                                pure Nothing
                            Success sd ->
                                -- TODO: persist hashmap
                                -- appendLine fileSyllables $ Lazy.fromStrict $ showt sd
                                pure $ Just (toLower $ sdWord sd, sd)
                            Exception exc -> Nothing <$ case exc of
                                ExceptionAbbreviation ->
                                    appendLine fileSyllablesAbbreviations entry
                                ExceptionMultiple ->
                                    appendLine fileSyllablesMultiple entry
                                ExceptionSpecialChar c ->
                                    appendLine fileSyllablesSpecialChar
                                        $  Lazy.singleton c
                                        <> " "
                                        <> entry
                                ExceptionSingleLetter ->
                                    appendLine fileSyllablesSingleLetter entry
                                ExceptionEllipsis ->
                                    appendLine fileSyllablesEllipsis entry
                                ExceptionAcronym ->
                                    appendLine fileSyllablesAcronyms entry
                                ExceptionExplicit -> appendLine
                                    fileSyllablesExplicitExceptions
                                    entry
                        pure $ HashMap.fromList $ catMaybes lsMWordSyllableData
                pure $ st { stsyllLastResult = hyphenated
                          , stsyllMap        = HashMap.unionWith preferLC m m'
                          }
        StateSyllables {..} <- foldM acc initialState entriesAll

        putStrLn $ "Writing file " <> fileSyllables
        writeFile fileSyllables
            $  Lazy.intercalate
                   "\n"
                   (Lazy.fromStrict . showt <$> HashMap.elems stsyllMap)
            <> "\n"

fileStenoWordsWordStenos :: FilePath
fileStenoWordsWordStenos = "stenoWordsWordStenos.txt"

fileStenoParts :: FilePath
fileStenoParts = "stenoparts.txt"

fileStenoWordsScores :: UTCTime -> FilePath
fileStenoWordsScores time =
    "stenowordsstats/stenowords-stats_"
        <> formatTime defaultTimeLocale "%y%m%d-%T" time
        <> ".txt"

fileStenoWordsDuplicates :: FilePath
fileStenoWordsDuplicates = "stenowords-duplicates.txt"

fileSmallDict :: FilePath
fileSmallDict = "smalldict.json"

fileTop2k :: FilePath
fileTop2k = "top2k.json"

fileCollisions :: FilePath
fileCollisions = "stenowords-collisions-v1.txt"

fileCollisionsV2 :: FilePath
fileCollisionsV2 = "stenowords-collisions-v2.txt"

readDouble :: Lazy.Text -> Double
readDouble str = case double str of
    Right (d, _) -> d
    Left  err    -> error $ "Could not read double: " <> err

readScores :: FilePath -> IO [Double]
readScores file = do
    fmap readDouble . Lazy.lines <$> readFile file

average :: forall a t . (Real a, Foldable t) => t a -> Double
average ls =
    let (t, n) = foldl' (\(!b, !c) a -> (a + b, (c :: Int) + 1)) (0, 0) ls
    in  realToFrac t / realToFrac n

data StenoWordsState key = StenoWordsState
    { swsMapWordStenos :: HashMap Text [SeriesData key]
    , swsMapChordParts :: HashMap (PartsData key) [Text]
    , swsMapStenoWords :: HashMap RawSteno [(Path, Text)]
    , swsNoParse       :: [Lazy.Text]
    }

instance Palantype key => Semigroup (StenoWordsState key) where
    StenoWordsState mWordSteno1 mChordParts1 mStenoWord1 noparse1 <> StenoWordsState mWordSteno2 mChordParts2 mStenoWord2 noparse2
        = StenoWordsState (HashMap.union mWordSteno1 mWordSteno2)
                          (HashMap.unionWith (++) mChordParts1 mChordParts2)
                          (HashMap.unionWith (++) mStenoWord1 mStenoWord2)
                          (noparse1 <> noparse2)

instance Palantype key => Monoid (StenoWordsState key) where
    mempty = StenoWordsState HashMap.empty HashMap.empty HashMap.empty []

stenoWords :: OptionsStenoWords -> IO ()
stenoWords (OStwRun greediness (OStwArg str)) =
    case parseSeries @DE.Key greediness str of
        Left  err -> StrictIO.putStrLn $ showt err
        Right sds -> do
            freqs <- readFrequencies
            StrictIO.putStrLn
                $  showt (freq freqs $ Text.replace "|" "" str)
                <> " "
                <> showSeriesData (head sds)
                <> " – "
                <> Text.intercalate "; " (showAlt <$> tail sds)
  where
    showSeriesData SeriesData {..} =
        sdHyphenated
            <> " "
            <> showt sdScore
            <> " "
            <> showt sdPath
            <> " "
            <> Text.intercalate "/" (showt . snd <$> sdParts)
    showAlt SeriesData {..} =
        showt sdPath <> " " <> Text.intercalate "/" (showt . snd <$> sdParts)

stenoWords (OStwRun greediness OStwStdin) = forever $ do
    l <- Lazy.toStrict <$> getLine
    StrictIO.putStrLn $ showt $ parseSeries @DE.Key greediness l

stenoWords (OStwRun greediness (OStwFile showChart mFileOutput)) = do

    start <- getTime Monotonic
    now   <- getCurrentTime

    let lsFiles =
            [ fileStenoWordsWordStenos
            , fileStenoWordsNoParse
            , fileStenoWordsDuplicates
            , fileStenoParts
            , fileSmallDict
            , fileCollisions
            , fileCollisionsV2
            ]
    for_ lsFiles $ \file -> do
        exists <- doesFileExist file
        when exists $ do
            putStrLn $ "Deleting " <> file
            removeFile file

    newScores <- runStenoWords

    putStrLn ""
    putStrLn $ "Number of words with steno code: " <> show (length newScores)

    putStrLn "Number of lines in"

    for_ lsFiles $ \file -> do
        exists <- doesFileExist file
        when exists $ do
            nl <- wcl file
            putStrLn $ show nl <> "\t" <> file

    nNoParse <- wcl fileStenoWordsNoParse
    let newZeroScores = replicate nNoParse 0

        scores        = newZeroScores <> (fromRational <$> newScores)
        meanScore     = average scores
    writeFile (fileStenoWordsScores now)
        $  Lazy.unlines (Lazy.fromStrict . showt <$> scores)

    putStrLn ""
    StrictIO.putStrLn $ "Average score: " <> showt meanScore

    putStrLn ""

    stop <- getTime Monotonic
    putStr "StenoWords runtime: "
    fprint (timeSpecs % "\n") start stop

    when showChart $ plotScoresShow scores
  where
    fileStenoWordsNoParse = "stenowords-noparse.txt"

    runStenoWords         = do

        freqs      <- readFrequencies
        lsSyllable <- Lazy.lines <$> readFile fileSyllables
        let l          = length lsSyllable
        putStrLn $ "Creating steno chords for " <> show l <> " entries."

        let acc :: Handle -> StenoWordsState DE.Key -> Lazy.Text -> IO (StenoWordsState DE.Key)
            acc h sws@StenoWordsState {..} str =
                let
                    (word, hyphenated) =
                        case splitOn " " $ Lazy.toStrict str of
                            [w, hy] -> (w, hy)
                            _ ->
                                error
                                    $  Text.unpack
                                    $  Lazy.toStrict
                                    $  "illegal entry: "
                                    <> str
                in
                    case parseSeries @DE.Key greediness hyphenated of
                        Left err -> case err of
                            PEExceptionTable orig -> do
                                print $ "Error in exception table for: " <> orig
                                pure sws
                            PEParsec _ _ ->
                                pure $ sws { swsNoParse = str : swsNoParse }
                        Right sds -> do
                            let acc' m (path, (o, c)) = HashMap.insertWith
                                    (++)
                                    (PartsData o c path)
                                    [word]
                                    m
                                partsData =
                                    foldl' acc' HashMap.empty
                                        $ concatMap
                                              (\sd -> (sdPath sd, )
                                                  <$> sdParts sd
                                              )
                                              sds

                                lsStenoPathWord =
                                    sds
                                        <&> \sd ->
                                                ( partsToSteno $ sdParts sd
                                                , (sdPath sd, word)
                                                )

                                ciAppend [pw1@(_, w1)] [pw2@(_, w2)] =
                                    if toLower w1 == toLower w2
                                        then [pw1]
                                        else [pw1, pw2]
                                ciAppend ws1 ws2 = ws1 ++ ws2

                                showSteno sd =
                                    Text.intercalate "/" (showt . snd <$> sdParts sd)

                            -- write to file
                            hPutStrLn h $  Lazy.fromStrict $
                                   word
                                <> " "
                                <> Text.intercalate " " (showSteno <$> sds)

                            pure $ sws
                                { swsMapWordStenos = HashMap.insert
                                                         word
                                                         sds
                                                         swsMapWordStenos
                                , swsMapChordParts = HashMap.unionWith
                                                         (++)
                                                         partsData
                                                         swsMapChordParts
                                , swsMapStenoWords = foldl'
                                    (\m (raw, pathWord) -> HashMap.insertWith
                                        ciAppend
                                        raw
                                        [pathWord]
                                        m
                                    )
                                    swsMapStenoWords
                                    lsStenoPathWord
                                }

        nj <- getNumCapabilities
        putStrLn ""
        putStrLn $ "Running " <> show nj <> " jobs."
        putStrLn ""

        let d    = ceiling ((fromIntegral l :: Double) / fromIntegral nj)
            jobs = zip [1 :: Int ..] $ chunksOf d lsSyllable
        lsResult <- forConcurrently jobs $ \(i, ls) -> do
            let filePart = "stenoWordsPart" <> show i <> ".txt"
            exists <- doesFileExist filePart
            when exists $ removeFile filePart
            h   <- openFile filePart ReadWriteMode
            sws <- foldM (acc h) mempty ls
            pure (h, sws)

        let lsSws                = snd <$> lsResult
            handles              = fst <$> lsResult
            StenoWordsState {..} = mconcat lsSws

        writeFile fileStenoWordsNoParse $ Lazy.unlines swsNoParse <> "\n"

        putStrLn $ "Writing file " <> fileStenoWordsWordStenos
        let hReadFile h = hSeek h AbsoluteSeek 0 *> hGetContents h
        content <- foldM (\c h -> (c <>) <$> hReadFile h) "" handles
        writeFile fileStenoWordsWordStenos $ content <> "\n"

        case mFileOutput of
            Just fileOutput -> do
                putStrLn $ "Writing " <> fileOutput

                for_ (HashMap.toList swsMapWordStenos) $ \(word, sd) -> do
                    let str = Lazy.fromStrict $ word <> " " <> showt sd
                    appendLine fileOutput str
            Nothing -> pure ()

        putStrLn $ "Writing file " <> fileStenoParts
        for_ (sortOn (length . snd) $ HashMap.toList swsMapChordParts)
            $ \(pd, words) -> do
                  let n = length words
                      str =
                          Lazy.fromStrict
                              $  showt pd
                              <> "; "
                              <> showt n
                              <> (if n > 1 then " words: " else " word: ")
                              <> Text.unwords (take 3 words)
                              <> if n > 3 then " ..." else ""
                  appendLine fileStenoParts str

        putStrLn "Writing small json dictionary"

        -- remove collisions
        let mapWordStenos = (partsToSteno . sdParts <$>) <$> swsMapWordStenos
            StateCollision {..} =
                resolveCollisions freqs mapWordStenos swsMapStenoWords

        -- check for double entries in the final result
            accDupls (m, ls) (r, w) = case HashMap.lookup r m of
                Just dupl -> (m, (r, w, dupl) : ls)
                Nothing   -> (HashMap.insert r w m, ls)

            dupls = snd $ foldl' accDupls (HashMap.empty, []) stcLsStenoWord

        writeFile fileStenoWordsDuplicates
            $  Lazy.unlines
                   (dupls <&> \(r, w, dupl) ->
                       Lazy.fromStrict $ showt r <> " " <> w <> " " <> dupl
                   )
            <> "\n"

        let formatJSONLine (raw, word) =
                Lazy.fromStrict $ "\"" <> showt raw <> "\": \"" <> word <> "\""
            lsStenoWord = sortOn snd stcLsStenoWord

        writeFile fileSmallDict
            $  "{\n"
            <> Lazy.intercalate ",\n" (formatJSONLine <$> lsStenoWord)
            <> "\n}\n"

        -- write most frequent 2'000 words in extra dictionary file
        let lsStenoWord2k = take 2000 $ sortOn (Down . freq freqs . snd) lsStenoWord
        writeFile fileTop2k
            $ "{\n"
            <> Lazy.intercalate ",\n" (formatJSONLine <$> lsStenoWord2k)
            <> "\n}\n"

        putStrLn $ "Writing " <> fileCollisions

        let
            -- get list of words that have been removed due to collisions
            setWord = HashSet.fromList $ toLower . snd <$> stcLsStenoWord
            accCollisions ls w =
                if HashSet.member (toLower w) setWord then ls else w : ls
            lsCollisions =
                foldl' accCollisions [] $ HashMap.keys swsMapWordStenos

        writeFile fileCollisions
            $  Lazy.intercalate
                   "\n"
                   (lsCollisions <&> \w ->
                       Lazy.fromStrict
                           $  w
                           <> ": "
                           <> showt (freq freqs w)
                           <> " "
                           <> Text.intercalate
                                  " "
                                  (   showt
                                  <$> mconcat
                                          (maybeToList $ HashMap.lookup
                                              w
                                              mapWordStenos
                                          )
                                  )
                   )
            <> "\n"

        putStrLn $ "Writing " <> fileCollisionsV2
        writeFile fileCollisionsV2
            $  Lazy.intercalate "\n" (Lazy.fromStrict . showt <$> stcLosers)
            <> "\n"

        pure $ HashMap.toList swsMapWordStenos <&> \(_, sds) ->
            maximum $ scorePrimary . sdScore <$> sds

--     showPathFreqWord :: [(Path, Text)] -> Text
--     showPathFreqWord pairs = Text.intercalate ", " $ showPathWord <$> pairs
--       where
--         showPathWord (p, w) =

stenoWords OStwShowChart = do
    now <- getCurrentTime
    let dir = takeDirectory $ fileStenoWordsScores now
    files <- listDirectory dir
    let latest = maximum files
    putStrLn $ "Reading scores from " <> dir </> latest
    scores <- readScores $ dir </> latest
    plotScoresShow scores

filePartsDict :: FilePath
filePartsDict = "partsdict.txt"

appendLine :: FilePath -> Lazy.Text -> IO ()
appendLine file str = appendFile file $ str <> "\n"

fileFrequencyAll :: FilePath
fileFrequencyAll = "frequencyAll.txt"

fileFrequency2k :: FilePath
fileFrequency2k = "frequency2k.txt"

fileFrequencyMissingAll :: FilePath
fileFrequencyMissingAll = "frequencyMissingAll.txt"

fileFrequencyMissing2k :: FilePath
fileFrequencyMissing2k = "frequencyMissing2k.txt"

frequency :: OptionsFrequency -> IO ()
frequency = \case
    OFreAll -> frequency' fileFrequencyMissingAll fileFrequencyAll id
    OFre2k  -> frequency' fileFrequencyMissing2k fileFrequency2k (take 2000)
  where
    frequency' fileMissing fileOutput fTake = do

      existsFileMissing <- doesFileExist fileMissing
      when existsFileMissing $ do
        putStrLn $ "Deleting " <> fileMissing
        removeFile fileMissing

      existsFileOutput <- doesFileExist fileOutput
      when existsFileOutput $ do
        putStrLn $ "Deleting " <> fileOutput
        removeFile fileOutput

      nLines <- wcl fileStenoWordsWordStenos
      StrictIO.putStrLn $ showt nLines <> " in " <> Text.pack fileStenoWordsWordStenos

      -- read steno words
      lsWordStenos <- Lazy.lines <$> readFile fileStenoWordsWordStenos
      let acc m str =
            case Lazy.splitOn " " str of
              w:stenos -> HashMap.insert w stenos m
              _        -> error $ "could not read: " <> Lazy.unpack str
          mapWordStenos = foldl' acc HashMap.empty lsWordStenos

      StrictIO.putStrLn "Writing files: "
      StrictIO.putStrLn $ Text.pack fileOutput
      StrictIO.putStrLn $ Text.pack fileMissing
      StrictIO.putStrLn ""

      -- read frequencies
      ls <- fTake . Lazy.lines <$> readFile "deu_news_2020_freq.txt"

      let accM :: Int -> Lazy.Text -> IO Int
          accM i l = case Lazy.head l of
            c | isLetter c -> case Lazy.splitOn "\t" l of
                [w, strF] ->
                  let mStenos = case HashMap.lookup w mapWordStenos of
                        Just stenos -> Just stenos
                        Nothing     -> HashMap.lookup (Lazy.toLower w) mapWordStenos
                  in  case mStenos of
                        Just stenos -> do
                          for_ stenos $ \s -> appendLine fileOutput $ s <> " " <> w
                          pure $ i + 1
                        Nothing -> do
                            appendLine fileMissing $ Lazy.fromStrict (showt i) <> " " <> strF <> " " <> w
                            pure $ i + 1
                _ -> error $ "could not read: " <> Lazy.unpack l
            _ -> pure i
      foldM_ accM (1 :: Int) ls

      putStrLn "Number of lines in"

      for_ [fileOutput, fileMissing] $ \file -> do
          nl <- wcl file
          putStrLn $ show nl <> "\t" <> file
