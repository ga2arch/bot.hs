{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
module Bot where

import           Bot.Channel
import           Bot.Channel.Telegram
import           Bot.Channel.Telegram.Types
import           Bot.Channel.Types
import           Bot.Command
import           Bot.Command.Feeder
import           Bot.Command.Route
import           Bot.Dispatcher
import           Bot.Types
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.Monoid
import           Data.Typeable
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import qualified Data.Proxy as P
import qualified Data.Text as T
import qualified STMContainers.Map as M
import qualified Web.Telegram.API.Bot.API as TG
import qualified Web.Telegram.API.Bot.Data as TG
import qualified Web.Telegram.API.Bot.Responses as TG

-- * Bot
onUpdate :: TG.Update -> BotMonad ()
onUpdate TG.Update{..} = onMessage message
  where
   onMessage (Just msg@TG.Message{..}) = onUser msg from
   onMessage _ = return ()

   onUser msg@TG.Message{chat=TG.Chat{chat_id=chatId}} (Just TG.User{..}) = do
     users <- asks botUsers
     user <- liftIO . atomically $ M.lookup user_id users
     case user of
       Just userChan -> void . liftIO . atomically $ writeTChan userChan msg
       Nothing  -> do
         config <- mkUser chatId
         startUser config
         void . liftIO . atomically $ do
           let chan = userChan config
           M.insert chan user_id users
           writeTChan chan msg

   onUser _ _ = return ()

   mkUser chatId = do
     botState <- ask
     userChan <- liftIO newTChanIO
     userNamespace <- liftIO $ newMVar T.empty
     listeners <- asks botListeners
     return $ UserConfig userChan botState chatId userNamespace (dispatch' listeners)

   startUser config = void . liftIO . forkIO $ userLoop config

userLoop :: UserConfig -> IO ()
userLoop userConfig = (flip runReaderT) userConfig $ runUser $ forever $ do
  inChan <- asks userChan
  m <- liftIO . atomically $ readTChan inChan
  go m
 where
  go TG.Message{text=Just text} = do
    namespace <- asks userNamespace
    ns <- liftIO $ readMVar namespace
    serve (P.Proxy :: P.Proxy Commands) handleCommands ns text

  go _ = return ()

register
  :: (MonadReader BotConfig m, MonadIO m, Typeable event) =>
     (TChan ChannelCmd -> IO (TChan event)) -> m ()
register f = do
  cmdChan <- asks botCmdChan
  listeners <- asks botListeners

  chan <- liftIO $ f cmdChan
  liftIO $ addListener' listeners chan

startChannel :: BotMonad ()
startChannel = do
  config <- asks channelConfig
  chan <- asks botCmdChan

  void . liftIO . forkIO . forever $ do
    m <- atomically $ readTChan chan
    evalTI config $ foldExpr sendChannel m

runBot :: BotConfig -> BotMonad a -> IO a
runBot config f = runReaderT (unBot f) config

startBot :: T.Text -> IO b
startBot tk = do
  let token = TG.Token ("bot" <> tk)
  channelConfig <- mkTelegram token
  botConfig <- mkBot channelConfig

  runBot botConfig $ do
    register feeder
    startChannel
    go token onUpdate

 where
  go token f = do
    manager <- liftIO $ newManager tlsManagerSettings
    getUpdates token manager Nothing f

  getUpdates token manager lastId f = do
    updates <- liftIO $ TG.getUpdates token lastId (Just 20) (Just 10) manager
    case updates of
      Right (TG.Response [] _) -> getUpdates token manager lastId f
      Right (TG.Response result _) -> do
        mapM_ f result
        let (TG.Update {TG.update_id=newLastId}) = last result
        getUpdates token manager (Just $ newLastId+1) f

      Left _ -> getUpdates token manager lastId f

  mkTelegram token = do
    manager <- newManager tlsManagerSettings
    return $ TelegramConfig manager token

  mkBot channelConfig = do
    users <- M.newIO
    listener <- M.newIO
    chan <- newTChanIO
    return $ BotConfig users channelConfig listener chan
