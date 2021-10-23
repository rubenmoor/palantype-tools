{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Args                (Task (..), argOpts)
import           Control.Applicative (Alternative ((<|>)),
                                      Applicative (pure, (*>), (<*>)))
import           Control.Category    (Category ((.)))
import           Control.Monad       (Monad ((>>=)), MonadPlus (mzero), foldM,
                                      join)
import           Data.Bifunctor      (Bifunctor (first, second))
import           Data.Bool           (Bool (False))
import           Data.Char           (Char)
import           Data.Either         (Either (..))
import           Data.Eq             (Eq)
import           Data.Foldable       (Foldable (foldl, length, maximum),
                                      maximumBy)
import           Data.Function       (($))
import           Data.Functor        (Functor (fmap), void, ($>), (<$>), (<&>))
import           Data.Int            (Int)
import           Data.List           (concat, dropWhile, head, intersperse,
                                      last, splitAt, (!!), (++))
import           Data.List.NonEmpty  (NonEmpty)
import           Data.Maybe          (Maybe (..), fromMaybe, maybe)
import           Data.Monoid         (Monoid (mconcat, mempty), (<>))
import           Data.Ord            (Ord ((>=)), comparing)
import           Data.String         (String)
import           Data.Text           (Text, intercalate, replace, splitOn,
                                      toLower)
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text
import           Data.Text.IO        (interact, putStrLn)
import           Data.Traversable    (for)
import           Data.Tuple          (fst, snd)
import           GHC.Err             (error)
import           GHC.Float           (Double)
import           GHC.Num             (Num ((-)), (+))
import           GHC.Real            (Fractional ((/)), fromIntegral, (^))
import           Options.Applicative (Parser, argument, command, execParser,
                                      idm, info, progDesc, str, subparser)
import           Palantype.Tools     (parseSeries)
import           System.Environment  (getArgs)
import           System.IO           (IO)

main :: IO ()
main =
  execParser argOpts >>= \case
    TaskWord str         -> putStrLn $ parseSeries str
    TaskSyllables reset  -> syllables reset
    TaskStenoWords reset -> stenoWords reset
    TaskPartsDict reset  -> partsDict reset

syllables :: Bool -> IO ()
syllables reset = do
  putStrLn "Starting over ..."

stenoWords :: Bool -> IO ()
stenoWords reset = do
  putStrLn "Starting over ..."

partsDict :: Bool -> IO ()
partsDict reset = do
  putStrLn "Starting over ..."
