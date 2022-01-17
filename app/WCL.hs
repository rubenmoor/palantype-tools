module WCL
  ( wcl
  ) where

import           Data.Eq                           (Eq ((==)))
import           Data.Int                          (Int)
import           Data.String                       (String)
import           Data.Word                         (Word8)
import           GHC.Num                           (Num ((+)))

import           System.IO                         (IO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Functor ((<$>))

wcl :: String -> IO Int
wcl file = parseLinebreaks 0 <$> BS.readFile file

parseLinebreaks :: Int -> ByteString -> Int
parseLinebreaks i "" = i
parseLinebreaks i bs =
        let (_, bs') = BS.break (== linefeed) bs
         in parseLinebreaks (i + 1) bs'

linefeed :: Word8
linefeed = 10 -- '\n'
