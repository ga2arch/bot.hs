{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Bot.Channel.Telegram.Types where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Text
import           Network.HTTP.Client
import           Servant.Common.Req (ServantError)

import qualified Web.Telegram.API.Bot.API as TG
import qualified Web.Telegram.API.Bot.Requests as TG

data TelegramMessage a = SMR TG.SendMessageRequest
                       | SVR (TG.SendVideoRequest Text)
                       | SAR (TG.SendAudioRequest TG.FileUpload)
  deriving (Functor)

data TelegramConfig = TelegramConfig
  { tcManager :: Manager
  , tcToken :: TG.Token
  }

newtype TI m a = TI { runTI :: ReaderT TelegramConfig (ExceptT ServantError m) a }
  deriving (Monad, Applicative, Functor, MonadIO,
            MonadReader TelegramConfig, MonadError ServantError)
