{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Palantype.Tools.TraceWords
    ( TraceWords
    , traceSample
    , runTraceWords
    ) where

import           Control.Applicative            ( Applicative )
import           Control.Monad                  ( Monad
                                                , when
                                                )
import           Control.Monad.Reader           ( MonadReader(ask)
                                                , ReaderT(runReaderT, ReaderT)
                                                )
import           Data.Function                  ( ($), const )
import           Data.Functor                   ( Functor )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as Text
import           System.IO                      ( IO )
import Control.Category ((<<<))
import UnliftIO ( MonadUnliftIO(..), MonadIO(..) )

newtype TraceWords a = TraceWords ( ReaderT (Set Text) IO a )
    deriving newtype
        ( Applicative
        , Functor
        , Monad
        , MonadReader (Set Text)
        )

instance MonadIO TraceWords where
  liftIO = TraceWords <<< ReaderT <<< const

instance MonadUnliftIO TraceWords where
  withRunInIO inner =
    TraceWords $ ReaderT $ \r -> inner $ runTraceWords r

runTraceWords :: Set Text -> TraceWords a -> IO a
runTraceWords setWords (TraceWords a) = runReaderT a setWords

traceSample :: Text -> Text -> TraceWords ()
traceSample sample msg = do
    setWords <- ask
    when (sample `Set.member` setWords) $ liftIO $ Text.putStrLn msg
