{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bot.Command.Feeder.Types where

import Bot.Channel
import Bot.Command.Feeder.Database.Types
import Bot.Command.Types
import Bot.Types
import Control.Concurrent.STM.TChan
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Int
import Data.Pool (Pool)
import Data.Text
import Database.Persist.Sql (Entity)
import Database.Persist.Sql (SqlBackend)
import Network.HTTP.Client (Manager)
import System.Log.FastLogger

data FeederEvent = News String
                 | SubscribeUrl Int64 Text
                 | UnsubscribeUrl Int64 Text
                 | ListFeeds String (TChan FeederEvent)
                 | Feeds [Entity Feed]

data Feeder next = Subscribe Text next
                 | Unsubscribe Text next
                 | Validate Text (Bool -> next)
                 | GetFeeds ([Entity Feed] -> next)
  deriving (Functor)

data FeederConfig = FeederConfig { fPool :: Pool SqlBackend
                                 , fCmdChan :: TChan ChannelCmd
                                 , fLogOut :: LoggerSet
                                 }

newtype FeederMonad a = FeederMonad { runFeeder :: ReaderT FeederConfig IO a }
  deriving (Functor, Applicative, Monad, MonadReader FeederConfig,
            MonadIO, MonadBase IO, MonadThrow, MonadCatch)

instance MonadBaseControl IO FeederMonad where
  type StM FeederMonad a = a
  liftBaseWith f = FeederMonad $ liftBaseWith $ \q -> f (q . runFeeder)
  restoreM = FeederMonad . restoreM

instance MonadLogger FeederMonad where
  monadLoggerLog loc source ll msg = do
    out <- asks fLogOut
    liftIO $ pushLogStr out $ defaultLogStr loc source ll $ toLogStr msg
