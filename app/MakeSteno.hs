{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module MakeSteno
    ( makeSteno
    ) where

import           Control.Applicative            ( Applicative(pure) )
import           Control.Arrow                  ( Arrow((***)) )
import           Control.Category               ( (<<<)
                                                , Category((.))
                                                )
import           Control.Concurrent             ( MVar
                                                , getNumCapabilities
                                                )
import           Control.Concurrent.Async       ( replicateConcurrently_ )
import qualified Control.Concurrent.Lock       as Lock
import           Control.Concurrent.Lock        ( Lock )
import           Control.Concurrent.MVar        ( modifyMVar
                                                , modifyMVar_
                                                , newMVar
                                                , readMVar
                                                )
import           Control.DeepSeq                ( force )
import           Control.Exception              ( evaluate )
import           Control.Monad                  ( Monad((>>))
                                                , foldM
                                                , unless
                                                , when
                                                )
import qualified Data.Aeson.Encode.Pretty      as Aeson
import           Data.Bool                      ( (&&)
                                                , Bool
                                                , not
                                                , (||)
                                                )
import qualified Data.ByteString.Lazy          as LBS
import           Data.Either                    ( Either(..)
                                                , isRight
                                                )
import           Data.Eq                        ( Eq((==)) )
import           Data.Foldable                  ( Foldable(length)
                                                , for_
                                                , traverse_
                                                )
import           Data.Function                  ( ($)
                                                , flip
                                                )
import           Data.Functor                   ( (<$>)
                                                , Functor(fmap)
                                                )
import           Data.Int                       ( Int )
import           Data.List                      ( minimumBy
                                                , sortOn
                                                , take
                                                , zip
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( Maybe(Just, Nothing) )
import           Data.Monoid                    ( (<>) )
import           Data.Ord                       ( Down(Down)
                                                , comparing
                                                )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set
                                                , (\\)
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import qualified Data.Text.IO                  as Text
import           Data.Tuple                     ( fst
                                                , snd
                                                )
import           Formatting                     ( (%)
                                                , fprint
                                                )
import           Formatting.Clock               ( timeSpecs )
import           GHC.Exts                       ( seq )
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

-- my-palantype
import           Palantype.Common               ( ExceptionInterpretation(..)
                                                , Greediness
                                                , Lang(DE, EN)
                                                , Palantype
                                                    ( PatternGroup
                                                    , mapExceptions
                                                    )
                                                , RawSteno
                                                , fromChord
                                                , parseWord
                                                , triePrimitives
                                                , unparts
                                                )
import qualified Palantype.DE.Keys             as DE
import qualified Palantype.EN.Keys             as EN

-- lib
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

-- exec
import           Args                           ( OptionsMakeSteno
                                                    ( OMkStArg
                                                    , OMkStFile
                                                    )
                                                )
import           Common                         ( appendLine
                                                , moveFileDotOld
                                                , writeJSONFile
                                                )
import           Sort                           ( getMapFrequencies )
import GHC.Num (Num(negate))


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
        DE -> makeSteno' @DE.Key fileInput
                                 fileOutputPlover
                                 fileOutputPloverMin
                                 fileOutputDoc
        EN -> makeSteno' @EN.Key fileInput
                                 fileOutputPlover
                                 fileOutputPloverMin
                                 fileOutputDoc

makeSteno'
    :: forall key
     . Palantype key
    => FilePath
    -> FilePath
    -> FilePath
    -> FilePath
    -> IO ()
makeSteno' fileInput fileOutputPlover fileOutputPloverMin fileOutputDoc = do
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

    putStr "Reading exceptions file ..."
    hFlush stdout

    (mapInitWordStenos, mapInitStenoWord, setReplByExc) <-
        foldM accExceptions (Map.empty, Map.empty, Set.empty)
            $ Map.toList mapExceptions

    putStrLn $ mapInitStenoWord `seq` " done."
    putStrLn
        $  "Added "
        <> show (Map.size mapInitStenoWord)
        <> " entries based on "
        <> show (Map.size mapInitWordStenos)
        <> " words in exceptions file."

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

    lock         <- Lock.new

    varDictState <- newMVar $ DictState mapInitWordStenos mapInitStenoWord
    varLs        <- newMVar ls

    if nj == 1
        then traverse_ (parseWordIO lock varDictState setReplByExc setLs) ls
        else
            let
                loop = do
                    mJob <- modifyMVar varLs $ pure . \case
                        []       -> ([], Nothing)
                        (j : js) -> (js, Just j)
                    case mJob of
                        Just hyph ->
                            parseWordIO lock
                                        varDictState
                                        setReplByExc
                                        setLs
                                        hyph
                                >> loop
                        Nothing -> pure ()
            in  replicateConcurrently_ nj loop

    setCursorColumn 28
    putStrLn "done.                 "

    DictState {..} <- readMVar varDictState

    mapFrequencies <- getMapFrequencies "deu_news_2020_freq.txt"

    let
        criterion = Down <<< (\w -> Map.findWithDefault 0 w mapFrequencies)

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

        mapStenoWordTake100
            :: Map (PatternGroup key) (Map Greediness (Int, [(Text, RawSteno)]))
        mapStenoWordTake100 = mapStenoWordDoc <<&>> \lsWordSteno ->
            ( length lsWordSteno
            , take 100
                $ sortOn (criterion <<< Text.encodeUtf8 <<< fst) lsWordSteno
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
    uDoc <- LBS.writeFile fileOutputDoc $ Aeson.encodePretty mapStenoWordTake100
    putStrLn $ uDoc `seq` " done."

    writeJSONFile fileOutputPlover $
        sortOn (criterion <<< snd) $
            (Text.encodeUtf8 . showt *** Text.encodeUtf8)
                <$> Map.toList dstMapStenoWord

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

-- exceptions

accExceptions
    :: forall key
     . Palantype key
    => ( Map Text [(Int, (RawSteno, (Greediness, PatternGroup key)))]
       , Map RawSteno Text
       , Set Text
       )
    -> ( Text
       , ( ExceptionInterpretation
         , [(Greediness, RawSteno, PatternGroup key, Bool)]
         )
       )
    -> IO
           ( Map
                 Text
                 [(Int, (RawSteno, (Greediness, PatternGroup key)))]
           , Map RawSteno Text
           , Set Text
           )
accExceptions (mapExcWordStenos, mapExcStenoWord, set) (word, (interp, lsExcEntry))
    = do
        let accExcEntry
                :: ( [(RawSteno, (Greediness, PatternGroup key))]
                   , Map RawSteno Text
                   )
                -> (Greediness, RawSteno, PatternGroup key, Bool)
                -> IO
                       ( [(RawSteno, (Greediness, PatternGroup key))]
                       , Map RawSteno Text
                       )
            accExcEntry (ls, mapEEStenoWord) (g, raw, pg, _) = do

                case parseWord @key raw of
                    Right chords -> do
                        let rawParsed = unparts $ fromChord <$> chords
                        pure
                            ( (rawParsed, (g, pg)) : ls
                            , Map.insert rawParsed word mapEEStenoWord
                            )
                    Left err -> do
                        Text.putStrLn
                            $  "Error in exception table: "
                            <> word
                            <> ": "
                            <> showt raw
                            <> "; "
                            <> Text.pack (show err)
                        pure (ls, mapEEStenoWord)

        (lsStenoInfo, m) <- foldM accExcEntry ([], mapExcStenoWord) lsExcEntry
        pure
            ( Map.insert word (zip (negate <$> [1 ..]) lsStenoInfo) mapExcWordStenos
            , m
            , case interp of
                ExcRuleAddition -> set
                -- mark the exceptions of type "substitution" for later
                ExcSubstitution -> Set.insert word set
            )

-- no exceptions

isAcronym :: Text -> Bool
isAcronym = isRight <<< runParser acronym () ""

parseWordIO
    :: forall key
     . Palantype key
    => Lock
    -> MVar (DictState key)
    -> Set Text
    -> Set Text
    -> Text
    -> IO ()
parseWordIO lock varDictState setReplByExc setLs hyph = do
    mapWordStenos <- dstMapWordStenos <$> readMVar varDictState

    let word        = Text.replace "|" "" hyph

-- exceptions marked as "substitution" replace the regular
-- steno algorithm and are not computed again
        isReplByExc = word `Set.member` setReplByExc

-- duplicate? don't compute any word twice!
-- but: words from the exception file marked
--     "rule-addition" do not count as duplicates
        isDupl =
            word
                `Map.member`    mapWordStenos
                &&              word
                `Map.notMember` mapExceptions @key

-- a capitalized word that also appears in its lower-case
-- version counts as duplicate
        isCaplDupl =
            isCapitalized hyph
                &&           not (isAcronym hyph)
                &&           Text.toLower hyph
                `Set.member` setLs

    when isDupl $ appendLine fileDuplicates word
    when isCaplDupl $ appendLine fileDuplicates $ word <> " capitalized"
    unless (isReplByExc || isDupl || isCaplDupl)
        $ case parseSeries @key triePrimitives hyph of
              Right stenos -> modifyMVar_
                  varDictState
                  \dst -> do
                      let (dst', isLost) =
                              Collision.resolve word (force stenos) dst
                      _ <- evaluate dst'
                      when isLost
                          $  appendLine fileCollisions
                          $  word
                          <> " "
                          <> Text.intercalate " " (showt <$> stenos)
                      pure dst'
              Left pe -> case pe of
                  PEParsec raw _ ->
                      Lock.with lock $ appendLine fileNoParse $ Text.unwords
                          [word, hyph, showt raw]
                  PEImpossible str -> do
                      Text.putStrLn $ "Seemingly impossible: " <> str
                      Lock.with lock $ appendLine fileNoParse $ Text.unwords
                          [word, hyph]

-- cf. https://hackage.haskell.org/package/relude-1.1.0.0/docs/Relude-Functor-Fmap.html
(<<&>>)
    :: forall m n a b
     . (Functor m, Functor n)
    => m (n a)
    -> (a -> b)
    -> m (n b)
(<<&>>) = flip (fmap . fmap)
