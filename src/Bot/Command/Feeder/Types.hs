{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bot.Command.Feeder.Types where

import           Bot.Command.Types
import           Bot.Types
import           Control.Concurrent.STM.TChan
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.Int
import           Data.Pool (Pool)
import           Database.Persist.Sql (SqlBackend)
import           Network.HTTP.Client (Manager)

import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T

data FeederEvent = News String | SubscribeUrl Int64 T.Text

data Feeder next = Subscribe (TChan FeederEvent) T.Text next
                 | Validate T.Text (Bool -> next)
  deriving (Functor)

data FeederConfig = FeederConfig {fPool :: Pool SqlBackend,
                                  fTelegramConfig :: TelegramConfig,
                                  fManager :: Manager}

newtype FeederMonad a = FeederMonad { runFeeder :: ReaderT FeederConfig IO a }
  deriving (Functor, Applicative, Monad, MonadReader FeederConfig,
            MonadIO, MonadBase IO, MonadThrow, MonadCatch)

instance MonadBaseControl IO FeederMonad where
  type StM FeederMonad a = a
  liftBaseWith f = FeederMonad $ liftBaseWith $ \q -> f (q . runFeeder)
  restoreM = FeederMonad . restoreM
