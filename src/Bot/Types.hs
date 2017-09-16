{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bot.Types where

import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad
import           Control.Monad.Reader
import           Data.Int
import           Data.Text
import           Network.HTTP.Client

import qualified Web.Telegram.API.Bot.Data as TG
import qualified Web.Telegram.API.Bot.API as TG
import qualified STMContainers.Map as M


data BotConfig = BotConfig { botUsers :: M.Map Int (TChan TG.Message)
                           , channelConfig :: TelegramConfig
                           }

data TelegramConfig = TelegramConfig { tgToken :: TG.Token
                                     , tgManager :: Manager
                                     }

data UserConfig = UserConfig { userChan :: TChan TG.Message
                             , userBot :: BotConfig
                             , userChatId :: Int64
                             , userNamespace :: MVar Text
                             }


newtype UserMonad a = UserMonad { runUser :: ReaderT UserConfig IO a }
  deriving (Monad, Applicative, Functor, MonadReader UserConfig, MonadIO)
