{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
module Bot where

import           Bot.Channel
import           Bot.Channel.Telegram
import           Bot.Channel.Types
import           Bot.Command
import           Bot.Command.Feeder
import           Bot.Command.Feeder.Types
import           Bot.Command.Route
import           Bot.Command.Types
import           Bot.Dispatcher
import           Bot.Types
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad.Reader
import           Data.Monoid
import           Data.Typeable
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import qualified Data.Proxy as P
import qualified Data.Text as T
import qualified STMContainers.Map as M
import qualified Web.Telegram.API.Bot.API as TG
import qualified Web.Telegram.API.Bot.API.Updates as TG
import qualified Web.Telegram.API.Bot.Data as TG
import qualified Web.Telegram.API.Bot.Requests as TG
import qualified Web.Telegram.API.Bot.Responses as TG

-- * Bot
dispatch' :: forall event. (Typeable event) => Listeners -> event -> IO ()
dispatch' listeners event = runD (dispatch event) listeners

onUpdate :: TG.Update -> BotMonad ()
onUpdate TG.Update{..} = do
  onMessage message
  where
   onMessage (Just message@TG.Message{..}) = onUser message from
   onMessage _ = return ()

   onUser message@TG.Message{chat=TG.Chat{chat_id=chatId}} (Just TG.User{..}) = do
     users <- asks botUsers
     user <- liftIO . atomically $ M.lookup user_id users
     case user of
       Just userChan -> do
         liftIO . atomically $ writeTChan userChan message
         return ()
       Nothing  -> do
         botState <- ask
         userChan <- liftIO $ newTChanIO
         userNamespace <- liftIO $ newMVar T.empty
         listeners <- asks botListeners
         liftIO . forkIO $
           runReaderT (runUser processMessage)
           $ UserConfig userChan botState chatId userNamespace (dispatch' listeners)
         liftIO . atomically $ do
           M.insert userChan user_id users
           writeTChan userChan message
         return ()
   onUser _ _ = return ()

processMessage :: UserMonad ()
processMessage = forever $ do
  inChan <- asks userChan
  m <- liftIO . atomically $ readTChan inChan
  go m
 where
  go TG.Message{chat=TG.Chat{chat_id=chatId}, text=Just text} = do
    namespace <- asks userNamespace
    ns <- liftIO $ readMVar namespace
    serve (P.Proxy :: P.Proxy Commands) handleCommands ns text

  go x = return ()

runBot tk = do
  let token = TG.Token ("bot" <> tk)

  botCmdChan <- newTChanIO
  channelConfig <- mkTelegram token
  feederChan <- feeder botCmdChan
  botConfig <- mkBot channelConfig botCmdChan

  runD (addListener feederChan) (botListeners botConfig)

  forkIO $ forever $ do
    m <- atomically $ readTChan botCmdChan
    evalTI channelConfig $ foldExpr sendChannel m

  go token $ \message -> runReaderT (unBot $ onUpdate message) botConfig
 where
  go token f = do
    manager <- newManager tlsManagerSettings
    getUpdates token manager Nothing f

  getUpdates token manager lastId f = do
    updates <- TG.getUpdates token lastId (Just 20) (Just 10) manager
    case updates of
      Right (TG.Response result _) -> do
        mapM_ f result
        if (not $ null result)
          then do
            let (TG.Update {TG.update_id=lastId}) = last result
            getUpdates token manager (Just $ lastId+1) f
          else
            getUpdates token manager lastId f
      Left  x -> getUpdates token manager lastId f

  mkTelegram token = do
    manager <- newManager tlsManagerSettings
    return $ TelegramConfig manager token

  mkBot channelConfig botCmdChan = do
    users <- M.newIO
    listener <- M.newIO
    return $ BotConfig users channelConfig listener botCmdChan
