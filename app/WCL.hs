module WCL
    ( wcl
    ) where

import           Data.Int                       ( Int )
import           Data.String                    ( String )
import           Data.Word                      ( Word8 )

import           Control.Category               ( Category((.)) )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Data.ByteString                ( split )
import qualified Data.ByteString               as BS
import           Data.Foldable                  ( Foldable(length) )
import           Data.Functor                   ( (<$>) )

wcl :: MonadIO m => String -> m Int
wcl file = length . split linefeed <$> liftIO (BS.readFile file)

linefeed :: Word8
linefeed = 10 -- '\n'
