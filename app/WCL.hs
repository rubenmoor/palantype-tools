module WCL where

import           Control.Monad                     (Monad)
import           Data.Eq                           (Eq ((==)))
import           Data.Function                     ((&))
import           Data.Int                          (Int)
import           Data.String                       (String)
import           Data.Word                         (Word8)
import           GHC.Num                           (Num ((+)))
import qualified Streamly.Data.Fold                as Fold
import qualified Streamly.Internal.FileSystem.File as File

import qualified Streamly.Prelude                  as Stream
import           System.IO                         (IO)

countl :: Int -> Word8 -> Int
countl n ch =
  if ch == 10 then n + 1 else n

-- The fold accepts a stream of `Word8` and returns a line count (`Int`).
nlines :: Monad m => Fold.Fold m Word8 Int
nlines =
  Fold.foldl' countl 0

wcl :: String -> IO Int
wcl file =
    File.toBytes file  -- SerialT IO Word8
  & Stream.fold nlines -- IO Int
