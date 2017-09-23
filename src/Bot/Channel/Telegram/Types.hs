{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Bot.Channel.Telegram.Types where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Network.HTTP.Client
import           Servant.Common.Req (ServantError)

import qualified Web.Telegram.API.Bot.API as TG
import qualified Web.Telegram.API.Bot.Requests as TG

data TelegramMessage a = SMR TG.SendMessageRequest
                       | SAR (TG.SendAudioRequest TG.FileUpload)
  deriving (Functor)

data TelegramConfig = TelegramConfig
  { tcManager :: Manager
  , tcToken :: TG.Token
  }

newtype TI m a = TI { runTI :: ReaderT TelegramConfig (ExceptT ServantError m) a }
  deriving (Monad, Applicative, Functor, MonadIO,
            MonadReader TelegramConfig, MonadError ServantError)
