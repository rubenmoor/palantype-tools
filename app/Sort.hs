{-# LANGUAGE BangPatterns #-}

module Sort where

import Args (OptionsSort (..))
import Common (writeJSONFile)
import Control.Applicative (Applicative (pure))
import Control.Category (Category ((.)), (<<<))
import Control.Monad.ST (runST)
import Control.DeepSeq (deepseq)
import qualified Data.Aeson as Aeson
import Data.Eq (Eq ((==)))
import Data.Foldable (Foldable (foldl, foldl'), traverse_)
import Data.Function (($))
import Data.Functor ((<$>), Functor (fmap), (<&>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Int (Int)
import Data.List (head, (!!))
import Data.Maybe (fromMaybe, catMaybes, Maybe (..))
import Data.Ord (Down (Down), comparing)
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.IO as Lazy
import qualified Data.Text.IO as Text
import Data.Vector (
    Vector,
    freeze,
    thaw,
 )
import qualified Data.Vector as Vector
import Data.Vector.Algorithms.Intro (sortBy)
import GHC.Err (error)
import GHC.Exts (seq)
import GHC.IO (IO)
import System.FilePath (takeExtension)
import System.IO (
    FilePath,
    hFlush,
    putStr,
    putStrLn,
    stdout,
 )
import Text.Read (read)
import Text.Show (Show (show))
import WCL (wcl)
import Data.Tuple (snd)
import GHC.Base (($!))

readFrequencies :: FilePath -> IO (HashMap Text Int)
readFrequencies fileFrequencies = do
    putStr $ "Reading frequency data from " <> fileFrequencies <> " ..."
    hFlush stdout
    ls <- fmap Lazy.toStrict . Lazy.lines <$> Lazy.readFile fileFrequencies
    let
        map = HashMap.fromList $ catMaybes $ ls <&> \l -> case Text.head l of
            '#' -> Nothing
            _   -> let [w, strFrequency] = Text.splitOn "\t" l
                   in  Just (w, read $ Text.unpack strFrequency)
    putStrLn $ ({-# SCC mapFreqs #-} map) `seq` " done."
    pure map

freq :: HashMap Text Int -> Text -> Int
freq freqs w = fromMaybe 0 $ HashMap.lookup w freqs

quicksort ::
    forall a. HashMap Text Int -> (a -> Text) -> Vector a -> Vector a
quicksort freqs f vec = runST $ do
    mvec <- thaw vec
    sortBy (comparing $ Down <<< freq freqs <<< f) mvec
    freeze mvec

sortByFrequency :: OptionsSort -> IO ()
sortByFrequency (OptionsSort fileFrequencies files) = do
    freqs <- readFrequencies fileFrequencies

    traverse_ (sortFile freqs) files
  where
    sortFile freqs file = do
        putStr $ "Getting number of lines in " <> file <> " ..."
        size <- wcl file
        putStrLn $ size `seq` " done."
        putStr $
            "Reading words from file "
                <> file
                <> " ("
                <> show size
                <> " lines) ..."
        hFlush stdout

        if takeExtension file == ".json"
            then do
                map <-
                        fromMaybe (error "Could not decode json file")
                    <$> Aeson.decodeFileStrict' file
                let
                    ls = HashMap.toList map
                    vec = ls `deepseq` Vector.fromListN (HashMap.size map) ({-# SCC jsonls #-} ls)
                putStrLn $  ({-# SCC vec #-} vec) `seq` " done."

                putStr "Sorting ..."
                hFlush stdout
                let sorted = {-# SCC vecSorted #-} quicksort freqs snd vec
                putStrLn $ sorted `seq` " done."

                writeJSONFile file sorted
            else do
                ls <- fmap Lazy.toStrict . Lazy.lines <$> Lazy.readFile file
                let vec = {-# SCC vec #-}
                        -- Vector.fromListN size . fmap (Lazy.toStrict $!) .
                        -- Vector.generate size $ \i -> ls !! i
                        Vector.fromListN size ({-# SCC lsFile #-} ls)
                putStrLn $ vec `deepseq` " done."

                putStr "Sorting ..."
                hFlush stdout

                let sorted = {-# SCC vecSorted #-}
                      quicksort freqs
                                (    Text.replace "|" ""
                                 <<< head
                                 <<< Text.splitOn " "
                                ) vec

                putStrLn $ sorted `seq` " done."
                putStr $ "Writing file " <> file <> " ..."
                hFlush stdout
                u <- Text.writeFile file $ Text.unlines $ Vector.toList sorted
                putStrLn $ u `seq` " done."
