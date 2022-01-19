module WCL
  ( wcl
  ) where

import           Data.Int                          (Int)
import           Data.String                       (String)
import           Data.Word                         (Word8)

import           System.IO                         (IO)
import Data.ByteString (split)
import qualified Data.ByteString as BS
import Data.Functor ((<$>))
import Data.Foldable (Foldable(length))
import Control.Category (Category((.)))

wcl :: String -> IO Int
wcl file = length . split linefeed <$> BS.readFile file

linefeed :: Word8
linefeed = 10 -- '\n'
