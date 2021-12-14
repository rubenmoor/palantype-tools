{-# LANGUAGE DerivingStrategies #-}

module RunPrepare where

import           Args                           ( OptionsPrepare(..) )
import           Common                         ( appendLine
                                                , removeFiles
                                                )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Monad                  ( foldM )
import           Data.Eq                        ( Eq((==)) )
import           Data.Foldable                  ( Foldable(length)
                                                , for_
                                                )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$)
                                                , (<$>)
                                                )
import qualified Data.HashMap.Strict           as HashMap
import           Data.HashMap.Strict            ( HashMap )
import           Data.List                      ( head )
import           Data.Maybe                     ( Maybe(Just, Nothing)
                                                , catMaybes
                                                )
import           Data.Monoid                    ( (<>) )
import           Data.Ord                       ( Ord((>)) )
import           Data.Text                      ( Text )
import qualified Data.Text                      as Text
import qualified Data.Text.IO                  as Text
import           Data.Traversable               ( for )
import           Formatting                     ( (%)
                                                , fprint
                                                )
import           Formatting.Clock               ( timeSpecs )
import           Palantype.Tools.Hyphenate      ( Hyphenated(Hyphenated), toWord )
import           Palantype.Tools.Prepare        ( Exception(..)
                                                , Result(..)
                                                , parseEntry
                                                )
import           System.Clock                   ( Clock(Monotonic)
                                                , getTime
                                                )
import           System.IO                      ( IO
                                                , IOMode(ReadMode)
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

data StatePrepare = StatePrepare
    { stLastResult :: Result
    , stMap        :: HashMap Text Hyphenated
    }

initialState :: StatePrepare
initialState = StatePrepare (Success $ Hyphenated []) HashMap.empty

prepare :: OptionsPrepare -> IO ()
prepare (OPrepArg str) = do
    for_ (parseEntry str) $ \case
        Success hyph -> do
            Text.putStrLn $ showt hyph
        other -> Text.putStrLn $ showt other

prepare (OPrepFile fileInput fileOutput) = do
    start <- getTime Monotonic
    let lsFiles =
            [ fileOutput
            , filePrepareNoParse
            , filePrepareAbbreviations
            , filePrepareMultiple
            , filePrepareSpecialChar
            , filePrepareSingleLetter
            , filePrepareEllipsis
            , filePrepareExplicitExceptions
            ]
    removeFiles lsFiles

    handle <- openFile fileInput ReadMode
    hSetNewlineMode handle universalNewlineMode
    entries <- Text.lines <$> Text.hGetContents handle

    let
        -- | in case of duplicate entries, try and find the
        --   higher-quality entry
        preferLessWeird :: Hyphenated -> Hyphenated -> Hyphenated

-- real duplicate
        preferLessWeird h1 h2 | h1 == h2 = h1

-- weird entry
-- more syllables means, better entry
        preferLessWeird h1@(Hyphenated ls1) h2@(Hyphenated ls2) =
            if length ls1 > length ls2 then h1 else h2

        accM st entry | Text.head entry == '#' = pure st
        accM st entry                          = do

        -- eliminate duplicate entries after parsing
            let
                lsResult   = parseEntry entry
                hyphenated = head lsResult
                last       = stLastResult st
                m          = stMap st
            m' <- if hyphenated == last
                then pure HashMap.empty
                else do
                    lsMSyllables <- for lsResult $ \case
                        Failure err -> do
                            print err
                            appendLine filePrepareNoParse entry
                            pure Nothing
                        Success   txts -> pure $ Just (toWord txts, txts)
                        Exception exc  -> Nothing <$ case exc of
                            ExceptionAbbreviation ->
                                appendLine filePrepareAbbreviations entry
                            ExceptionMultiple ->
                                appendLine filePrepareMultiple entry
                            ExceptionSpecialChar c ->
                                appendLine filePrepareSpecialChar
                                    $  Text.singleton c
                                    <> " "
                                    <> entry
                            ExceptionSingleLetter ->
                                appendLine filePrepareSingleLetter entry
                            ExceptionEllipsis ->
                                appendLine filePrepareEllipsis entry
                            ExceptionExplicit ->
                                appendLine filePrepareExplicitExceptions entry
                            ExceptionMisspelling -> pure ()
                    pure $ HashMap.fromList $ catMaybes lsMSyllables
            pure $ st { stLastResult = hyphenated
                      , stMap        = HashMap.unionWith preferLessWeird m m'
                      }

    StatePrepare { stMap = m } <- foldM accM initialState entries

    putStrLn $ "Writing file " <> fileOutput
    Text.writeFile fileOutput
        $  Text.intercalate "\n" (showt <$> HashMap.elems m)
        <> "\n"

    putStrLn ""
    putStrLn "Number of lines in"

    for_ (fileInput : lsFiles) $ \file -> do
        nl <- wcl file
        putStrLn $ show nl <> "\t" <> file

    putStrLn ""
    stop <- getTime Monotonic
    putStr "Prepare runtime: "
    fprint (timeSpecs % "\n") start stop

  where
    filePrepareAbbreviations      = "prepare-abbreviations.txt"
    filePrepareMultiple           = "prepare-multiple.txt"
    filePrepareSpecialChar        = "prepare-specialchar.txt"
    filePrepareSingleLetter       = "prepare-singleletter.txt"
    filePrepareEllipsis           = "prepare-ellipsis.txt"
    filePrepareNoParse            = "prepare-noparse.txt"
    filePrepareExplicitExceptions = "prepare-explicitexceptions.txt"
