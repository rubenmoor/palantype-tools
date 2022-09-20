{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module MakeSteno where

import           Args                           ( OptionsMakeSteno
                                                    ( OMkStArg
                                                    , OMkStFile
                                                    )
                                                )
import           Common                         ( appendLine
                                                , moveFileDotOld
                                                , writeJSONFile
                                                )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Concurrent             ( getNumCapabilities )
import           Control.Concurrent.Async       ( replicateConcurrently_ )
import qualified Control.Concurrent.Lock       as Lock
import           Control.Concurrent.MVar        ( modifyMVar
                                                , modifyMVar_
                                                , newMVar
                                                , readMVar
                                                )
import           Control.Monad                  ( Monad((>>))
                                                , when, foldM
                                                )
import           Data.Either                    ( Either(..)
                                                , isRight
                                                )
import           Data.Foldable                  ( Foldable(length)
                                                , for_
                                                , traverse_
                                                )
import           Data.Function                  ( ($), flip )
import           Data.Functor                   ( (<$>)
                                                , Functor(fmap)
                                                )
import           Data.List                      ( minimumBy
                                                , sortOn
                                                , take, zip
                                                )
import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Formatting                     ( (%)
                                                , fprint
                                                )
import           Formatting.Clock               ( timeSpecs )
import           GHC.Exts                       ( seq )

import           Control.Arrow                  ( Arrow((***)) )
import           Control.Category               ( (<<<)
                                                , Category((.))
                                                )
import           Control.DeepSeq                ( force )
import           Control.Exception              ( evaluate )
import qualified Data.Aeson.Encode.Pretty      as Aeson
import           Data.Bool                      ( (&&)
                                                , Bool(False, True)
                                                , not
                                                )
import qualified Data.ByteString.Lazy          as LBS
import           Data.Eq                        ( Eq((==)) )
import           Data.Int                       ( Int )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( Maybe(Just, Nothing) )
import           Data.Ord                       ( Down(Down)
                                                , comparing
                                                )
import qualified Data.Set                      as Set
import           Data.Set                       ( (\\), Set )
import qualified Data.Text.Encoding            as Text
import           Data.Tuple                     ( fst
                                                , snd
                                                )
import           Palantype.Common               ( Greediness
                                                , Lang(DE, EN)
                                                , Palantype(PatternGroup, mapExceptions)
                                                , RawSteno
                                                , triePrimitives, parseWord, unparts, fromChord, ExceptionInterpretation (..)
                                                )
import qualified Palantype.DE.Keys             as DE
import qualified Palantype.EN.Keys             as EN
import           Palantype.Tools.Collision      ( DictState
                                                    ( DictState
                                                    , dstMapWordStenos
                                                    )
                                                )
import qualified Palantype.Tools.Collision     as Collision
import           Palantype.Tools.Steno          ( ParseError(..)
                                                , acronym
                                                , isCapitalized
                                                , parseSeries
                                                )
import           Sort                           ( getMapFrequencies )
import           System.Clock                   ( Clock(Monotonic)
                                                , getTime
                                                )
import           System.Console.ANSI            ( setCursorColumn )
import           System.Directory               ( doesFileExist )
import           System.IO                      ( FilePath
                                                , IO
                                                , hFlush
                                                , putStr
                                                , putStrLn
                                                , stdout
                                                )
import           Text.Parsec                    ( runParser )
import           Text.Show                      ( Show(show) )
import           TextShow                       ( TextShow(showt) )
import           WCL                            ( wcl )
-- import Control.Scheduler (traverseConcurrently, Comp (ParN, ParOn))

fileNoParse :: FilePath
fileNoParse = "makeSteno-noparse.txt"

fileLost :: FilePath
fileLost = "makeSteno-lostwords.txt"

fileCollisions :: FilePath
fileCollisions = "makeSteno-collisions.txt"

fileDuplicates :: FilePath
fileDuplicates = "makeSteno-duplicates.txt"

makeSteno :: OptionsMakeSteno -> IO ()
makeSteno (OMkStArg lang str) = case lang of
    DE -> parseSeries' @DE.Key
    EN -> parseSeries' @EN.Key
  where
    parseSeries' :: forall key . Palantype key => IO ()
    parseSeries' = case parseSeries @key triePrimitives str of
        Left  err -> Text.putStrLn $ showt err
        Right sds -> traverse_ (Text.putStrLn <<< showt) sds
makeSteno (OMkStFile fileInput fileOutputPlover fileOutputPloverMin fileOutputDoc lang)
    = case lang of
        DE -> makeSteno' @DE.Key
        EN -> makeSteno' @EN.Key
  where
    makeSteno' :: forall key . Palantype key => IO ()
    makeSteno' = do
        start <- getTime Monotonic

        let lsFiles =
                [ fileNoParse
                , fileOutputPlover
                , fileOutputPloverMin
                , fileOutputDoc
                , fileCollisions
                , fileDuplicates
                , fileLost
                ]
        traverse_ moveFileDotOld lsFiles

        -- first: read exception file

        let accExceptions
              :: ( Map Text [(Int, (RawSteno, (Greediness, PatternGroup key)))]
                 , Map RawSteno Text
                 , Set Text
                 )
              -> ( Text
                 , ( ExceptionInterpretation
                   , [(Greediness, RawSteno, PatternGroup key, Bool)]
                   )
                 )
              -> IO ( Map Text [(Int, (RawSteno, (Greediness, PatternGroup key)))]
                    , Map RawSteno Text
                    , Set Text
                    )
            accExceptions (mapExcWordStenos, mapExcStenoWord, set) (word, (interp, lsExcEntry)) = do
              let accExcEntry
                    :: ([(RawSteno, (Greediness, PatternGroup key))], Map RawSteno Text)
                    -> (Greediness, RawSteno, PatternGroup key, Bool)
                    -> IO ([(RawSteno, (Greediness, PatternGroup key))], Map RawSteno Text)
                  accExcEntry (ls, mapEEStenoWord) (g, raw, pg, _) = do

                    case parseWord @key raw of
                      Right chords -> do
                        let rawParsed = unparts $ fromChord <$> chords
                        pure ( (rawParsed, (g, pg)) : ls
                             , Map.insert rawParsed word mapEEStenoWord
                             )
                      Left  err    -> do
                        Text.putStrLn $ "Error in exception table: "
                                     <> word <> ": "
                                     <> showt raw <> "; "
                                     <> Text.pack (show err)
                        pure (ls, mapEEStenoWord)
              (lsStenoInfo, m) <- foldM accExcEntry ([], mapExcStenoWord) lsExcEntry
              pure ( Map.insert word (zip [0..] lsStenoInfo) mapExcWordStenos
                   , m
                   , case interp of
                       ExcRuleAddition -> set
                       ExcSubstitution -> Set.insert word set
                   )

        (mapInitWordStenos, mapInitStenoWord, setReplByExc) <-
          foldM accExceptions (Map.empty, Map.empty, Set.empty) (Map.toList mapExceptions)

        -- moving on to regular input

        putStr $ "Reading input file " <> fileInput <> " ..."
        hFlush stdout
        ls <- Text.lines <$> Text.readFile fileInput

        let l     = length ls
            setLs = Set.fromList ls

        putStrLn $ l `seq` " done."

        putStrLn $ "Creating steno chords for " <> show l <> " entries."

        nj <- getNumCapabilities
        putStr $ "\nRunning " <> show nj <> " jobs.\n\n"
        putStr "Optimizing steno chords ..."
        hFlush stdout

        lock      <- Lock.new

        dictState <- newMVar $ DictState mapInitWordStenos mapInitStenoWord
        mvarLs    <- newMVar ls

        let isAcronym = isRight <<< runParser acronym () ""

            parseWordIO :: Text -> IO ()
            parseWordIO hyph = do
                mapWordStenos <- dstMapWordStenos <$> readMVar dictState

                let
                    isReplByExc = word `Set.member` setReplByExc
                    isDupl      = word `Map.member` mapWordStenos
                               && word `Map.notMember` mapExceptions @key
                    isCapl      = isCapitalized hyph && not (isAcronym hyph)
                    isCaplDupl  = isCapl && Text.toLower hyph `Set.member` setLs
                    word        = Text.replace "|" "" hyph

                case (isReplByExc, isDupl, isCaplDupl) of
                    (True , _   , _     ) -> pure ()
                    (False, True, _     ) -> appendLine fileDuplicates word
                    (False, False, True ) -> appendLine fileDuplicates $ word <> " capitalized"
                    (False, False, False) ->
                        case parseSeries @key triePrimitives hyph of
                            Right stenos -> modifyMVar_
                                dictState
                                \dst -> do
                                    let
                                        (dst', isLost) = Collision.resolve
                                            word
                                            (force stenos)
                                            dst
                                    _ <- evaluate dst'
                                    when isLost
                                        $  appendLine fileCollisions
                                        $  word
                                        <> " "
                                        <> Text.intercalate
                                               " "
                                               (showt <$> stenos)
                                    pure dst'
                            Left pe -> case pe of
                                PEParsec raw _ ->
                                    Lock.with lock
                                        $ appendLine fileNoParse
                                        $ Text.unwords [word, hyph, showt raw]
                                PEImpossible str -> do
                                    Text.putStrLn
                                        $  "Seemingly impossible: "
                                        <> str
                                    Lock.with lock
                                        $ appendLine fileNoParse
                                        $ Text.unwords [word, hyph]

            loop = do
                mJob <- modifyMVar
                    mvarLs
                    \ls' -> pure $ case ls' of
                        []       -> ([], Nothing)
                        (j : js) -> (js, Just j)
                case mJob of
                    Just hyph -> parseWordIO hyph >> loop
                    Nothing   -> pure ()

        if nj == 1
            then traverse_ parseWordIO ls
            else replicateConcurrently_ nj loop

        setCursorColumn 28
        putStrLn "done.                 "

        DictState {..} <- readMVar dictState

        mapFrequencies <- getMapFrequencies "deu_news_2020_freq.txt"

        let criterion = Down <<< (\w -> Map.findWithDefault 0 w mapFrequencies)
            sorted =
                sortOn (criterion <<< snd)
                    $   (Text.encodeUtf8 . showt *** Text.encodeUtf8)
                    <$> Map.toList dstMapStenoWord

            mapStenoWordDoc
                :: Map (PatternGroup key) (Map Greediness [(Text, RawSteno)])
            mapStenoWordDoc = Map.foldrWithKey
                (\w stenos m ->
                    let (_, (raw, (g, pat))) = minimumBy (comparing fst) stenos
                    in  Map.insertWith (Map.unionWith (<>))
                                       pat
                                       (Map.singleton g [(w, raw)])
                                       m
                )
                Map.empty
                dstMapWordStenos

            -- cf. https://hackage.haskell.org/package/relude-1.1.0.0/docs/Relude-Functor-Fmap.html
            (<<&>>) :: forall m n a b. (Functor m, Functor n) => m (n a) -> (a -> b) -> m (n b)
            (<<&>>) = flip (fmap . fmap)

            mapStenoWordTake100
                :: Map (PatternGroup key)
                       (Map Greediness (Int, [(Text, RawSteno)]))
            mapStenoWordTake100 =
              mapStenoWordDoc <<&>> \lsWordSteno ->
                  ( length lsWordSteno
                  , take 100 $ sortOn (criterion <<< Text.encodeUtf8 <<< fst) lsWordSteno
                  )

            mapStenoWordMin :: Map Text RawSteno
            mapStenoWordMin = Map.foldrWithKey
                (\w stenos m ->
                    let (_, (raw, _)) = minimumBy (comparing fst) stenos
                    in  Map.insert w raw m
                )
                Map.empty
                dstMapWordStenos

        -- checking for lost words
        putStr $ "Writing lost words to " <> fileLost <> " ..."
        hFlush stdout
        traverse_ (appendLine fileLost)
            $  Set.map (Text.replace "|" "") setLs
            \\ Map.keysSet dstMapWordStenos
        putStrLn " done."

        putStr $ "Writing file " <> fileOutputDoc <> " ..."
        hFlush stdout
        uDoc <- LBS.writeFile fileOutputDoc
            $ Aeson.encodePretty mapStenoWordTake100
        putStrLn $ uDoc `seq` " done."

        writeJSONFile fileOutputPlover sorted
        LBS.writeFile fileOutputPloverMin $ Aeson.encodePretty mapStenoWordMin

        putStrLn ""
        putStrLn "Number of lines in"

        for_ lsFiles $ \file -> do
            exists <- doesFileExist file
            when exists $ do
                nl <- wcl file
                putStrLn $ show nl <> "\t" <> file

        putStrLn ""

        stop <- getTime Monotonic
        putStr "StenoWords runtime: "
        fprint (timeSpecs % "\n") start stop
