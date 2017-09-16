{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
module Bot where

import           Bot.Command
import           Bot.Command.Feeder
import           Bot.Command.Feeder.Types
import           Bot.Command.Types
import           Bot.Types
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad.Reader
import           Data.Monoid
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
  go TG.Message{chat=TG.Chat{chat_id=chatId}, text=Just text, entities=Nothing} =
    serve (P.Proxy :: P.Proxy Commands) handleCommands text
  go TG.Message{chat=TG.Chat{chat_id=chatId}, text=Just text, entities=Just entities} = do
    let x = T.pack $ concat $ map show entities
    serve (P.Proxy :: P.Proxy Commands) handleCommands (text <> " " <> x)
  go x = return ()

sendMessage chatId text = do
   TG.sendMessageM TG.SendMessageRequest {
      message_chat_id = TG.ChatId chatId,
      message_text = text,
      message_parse_mode = Nothing,
      message_disable_web_page_preview = Just True,
      message_disable_notification = Nothing,
      message_reply_to_message_id = Nothing,
      message_reply_markup = Nothing
      }

call action = do
  botConfig <- asks userBot
  let config = channelConfig botConfig
  liftIO $ TG.runClient action (tgToken config) (tgManager config)

runBot tk = do
  let token = TG.Token ("bot" <> tk)

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
      Left  x -> getUpdates token manager lastId f

  mkTelegram token = do
    manager <- newManager tlsManagerSettings
    return $ TelegramConfig token manager

  mkBot channelConfig = do
    users <- M.newIO
    return $ BotConfig users channelConfig
