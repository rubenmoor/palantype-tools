{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Args                      (Task (..), argOpts)
import           Control.Applicative       (Alternative ((<|>)),
                                            Applicative (pure, (*>), (<*>)))
import           Control.Category          (Category ((.)))
import           Control.Monad             (Monad ((>>=)), MonadPlus (mzero),
                                            foldM, join, when)
import           Data.Bifunctor            (Bifunctor (first, second))
import           Data.Bool                 (Bool (False))
import           Data.Char                 (Char)
import           Data.Either               (Either (..))
import           Data.Eq                   (Eq)
import           Data.Foldable             (Foldable (foldl, length, maximum),
                                            for_, maximumBy, traverse_)
import           Data.Function             (($))
import           Data.Functor              (Functor (fmap), void, ($>), (<$>),
                                            (<&>))
import           Data.Int                  (Int)
import           Data.List                 (concat, dropWhile, head,
                                            intersperse, last, splitAt, (!!),
                                            (++))
import           Data.List.NonEmpty        (NonEmpty)
import           Data.Maybe                (Maybe (..), fromMaybe, maybe)
import           Data.Monoid               (Monoid (mconcat, mempty), (<>))
import           Data.Ord                  (Ord ((>=)), comparing)
import           Data.String               (String)
import           Data.Text                 (Text, intercalate, replace, splitOn,
                                            toLower)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import qualified Data.Text.IO              as StrictIO
import qualified Data.Text.Lazy            as Lazy
import           Data.Text.Lazy.IO         (appendFile, readFile)
import           Data.Traversable          (for)
import           Data.Tuple                (fst, snd)
import           GHC.Err                   (error)
import           GHC.Float                 (Double)
import           GHC.Num                   (Num ((-)), (+))
import           GHC.Real                  (Fractional ((/)), fromIntegral, (^))
import           Options.Applicative       (Parser, argument, command,
                                            execParser, idm, info, progDesc,
                                            str, subparser)
import           Palantype.Tools.Steno     (parseSeries)
import           Palantype.Tools.Syllables (Exception (..), Result (..),
                                            parseSyllables)
import           System.Directory          (doesFileExist, removeFile,
                                            renamePath)
import           System.Environment        (getArgs)
import           System.IO                 (IO, print, putStrLn)
import           TextShow                  (TextShow (showt))
import Data.Time (getCurrentTime, diffUTCTime, formatTime, defaultTimeLocale)

main :: IO ()
main =
  execParser argOpts >>= \case
    TaskWord str         -> StrictIO.putStrLn $ parseSeries str
    TaskSyllables reset  -> syllables reset
    TaskStenoWords reset -> stenoWords reset
    TaskPartsDict reset  -> partsDict reset

syllables :: Bool -> IO ()
syllables reset = do
    start <- getCurrentTime
    when reset $ traverse_ removeFile
      [ fileSyllables
      , fileSyllablesNoParse
      , fileSyllablesAbbreviations
      , fileSyllablesMultiple
      , fileSyllablesSpecialChar
      ]

    existsFileNoParse <- doesFileExist fileSyllablesNoParse

    runSyllables $
      if existsFileNoParse
        then fileSyllablesNoParse
        else "entries.txt"

    stop <- getCurrentTime
    putStrLn $ formatTime defaultTimeLocale "%2M:%2S" $ diffUTCTime stop start

  where
    fileSyllables = "syllables.txt"
    fileSyllablesNoParse = "syllables-noparse.txt"
    fileSyllablesAbbreviations = "syllables-abbreviations.txt"
    fileSyllablesMultiple = "syllables-multiple.txt"
    fileSyllablesSpecialChar = "syllables-specialchar.txt"

    runSyllables file = do

      let tmpFileNoParse = "syllables-noparse.tmp.txt"
      entries <- Lazy.lines <$> readFile file

      for_ entries $ \entry ->
        for_ (parseSyllables $ Lazy.toStrict entry) $ \case
          Failure err     -> do
            print err
            appendFile tmpFileNoParse entry
          Success sd -> appendFile fileSyllables $ Lazy.fromStrict $ showt sd
          Exception exc -> case exc of
            ExceptionAbbreviation  -> appendFile fileSyllablesAbbreviations entry
            ExceptionMultiple      -> appendFile fileSyllablesMultiple entry
            ExceptionSpecialChar c -> appendFile fileSyllablesSpecialChar $ Lazy.singleton c <> " " <> entry
      renamePath tmpFileNoParse fileSyllablesNoParse

stenoWords :: Bool -> IO ()
stenoWords reset = do
  putStrLn "Starting over ..."

partsDict :: Bool -> IO ()
partsDict reset = do
  putStrLn "Starting over ..."
