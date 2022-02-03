module Sort where

import Args (OptionsSort (..))
import Common (writeJSONFile, writeFile)
import Control.Category (Category ((.)), (<<<))
import qualified Data.Aeson as Aeson
import Data.Eq (Eq ((==), (/=)))
import Data.Foldable (traverse_, Foldable (foldMap))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (head, sortOn)
import Data.Maybe (fromMaybe, Maybe (..))
import Data.Ord (Down (Down))
import Data.Semigroup (Semigroup ((<>)))
import GHC.Err (error)
import GHC.Exts (seq)
import GHC.IO (IO)
import System.FilePath (takeExtension)
import System.IO (
    FilePath,
    hFlush,
    putStr,
    putStrLn,
    stdout
 )
import Text.Show (Show (show))
import Data.Tuple (snd)
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Builder as BSB
import Data.Word (Word8)
import Data.Monoid (Monoid(mconcat))
import Data.Map.Strict (Map)
import qualified Data.Text.Encoding as Text
import Control.Arrow ((***))
import Control.Applicative (Applicative(pure))
import Control.Exception (evaluate)

getMapFrequencies :: FilePath -> IO (Map ByteString Int)
getMapFrequencies file = do
    putStr $ "Reading frequency information from " <> file <> " ..."
    hFlush stdout
    m <- Map.fromList . parseFrequencies <$> BS.readFile file
    putStrLn $ m `seq` " done."
    pure m

sortByFrequency :: OptionsSort -> IO ()
sortByFrequency (OptionsSort fileFrequencies files) = do

    mapFrequencies <- getMapFrequencies fileFrequencies
    traverse_ (sortFile mapFrequencies) files
  where
    sortFile :: Map ByteString Int -> FilePath -> IO ()
    sortFile m file = do
        putStr $
            "Reading words from file "
                <> file
                <> " ..."
        hFlush stdout

        if takeExtension file == ".json"
            then do
                map <-
                        fromMaybe (error "Could not decode json file")
                    <$> Aeson.decodeFileStrict' file
                let ls = (Text.encodeUtf8 *** Text.encodeUtf8) <$> Map.toList map
                putStrLn $ ls `seq` " done."
                putStrLn $ show (Map.size map) <> " entries read."

                let
                    crit = Down <<< (\x -> Map.findWithDefault 0 x m) <<< snd
                    sorted = sortOn crit ls

                writeJSONFile file sorted
            else do
                ls <- Char8.lines <$> BS.readFile file
                _ <- evaluate ls
                putStrLn " done."

                let
                    crit = Down <<< (\x -> Map.findWithDefault 0 x m)
                                <<< mconcat
                                <<< BS.split pipe
                                <<< head
                                <<< BS.split space
                    sorted = sortOn crit ls

                putStr $ "Writing file " <> file <> " ..."
                hFlush stdout
                u <- writeFile file $ foldMap ((<> "\n") . BSB.byteString) sorted
                putStrLn $ u `seq` " done."

parseFrequencies :: ByteString -> [(ByteString, Int)]
parseFrequencies bs = case BS.uncons bs of
    Nothing -> []
    Just (w, _) | w == pound -> parseFrequencies $ BS.unsafeTail $ BS.dropWhile (/= linefeed) bs
    _ ->
        let (w, f) = BS.break (== tab) bs
         in case Char8.readInt (BS.unsafeTail f) of
                Just (i, bs') -> (w, i) : parseFrequencies (BS.unsafeTail bs')
                Nothing -> []

pound :: Word8
pound = 35 -- '#'

linefeed :: Word8
linefeed = 10 -- '\n'

tab :: Word8
tab = 9 -- '\t'

space :: Word8
space = 32 -- ' '

pipe :: Word8
pipe = 124 -- '|'
