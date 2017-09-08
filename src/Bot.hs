{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitParams #-}
module Bot where

import Bot.Types
import Bot.Command.Types
import Bot.Command.Endpoints
import Bot.Command.Feeder.Types
import Bot.Command.Feeder.Feeder

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent
import Control.Monad.Reader
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import qualified Data.Proxy as P
import qualified Web.Telegram.API.Bot.Data as TG
import qualified Web.Telegram.API.Bot.Responses as TG
import qualified Web.Telegram.API.Bot.Requests as TG
import qualified Web.Telegram.API.Bot.API as TG
import qualified Web.Telegram.API.Bot.API.Updates as TG
import qualified STMContainers.Map as M

-- * Bot
dispatch :: (?feederChan :: TChan FeederEvent) => TG.Update -> ReaderT BotConfig IO ()
dispatch TG.Update{..} = do
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
         liftIO . forkIO $ runReaderT (runUser processMessage) $ UserConfig userChan botState chatId
         liftIO . atomically $ do
           M.insert userChan user_id users
           writeTChan userChan message
         return ()
   onUser _ _ = return ()

processMessage :: (?feederChan :: TChan FeederEvent) => UserMonad ()
processMessage = forever $ do
  inChan <- asks userChan
  m <- liftIO . atomically $ readTChan inChan
  go m
 where
  go TG.Message{chat=TG.Chat{chat_id=chatId}, text=Just text} = do
    serve (P.Proxy :: P.Proxy Commands) handleCommands text
    return ()
  go _ = return ()

runBot tk = do
  let token = TG.Token tk

  channelConfig <- mkTelegram token
  feederChan <- feeder channelConfig
  botConfig <- mkBot channelConfig

  let ?feederChan = feederChan
    in go token $ \message -> runReaderT (dispatch message) botConfig
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
      Left  x -> print x

  mkTelegram token = do
    manager <- newManager tlsManagerSettings
    return $ TelegramConfig token manager

  mkBot channelConfig = do
    users <- M.newIO
    return $ BotConfig users channelConfig
