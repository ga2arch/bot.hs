{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
module Bot.Channel.Telegram where

import           Bot.Channel.Telegram.Types
import           Bot.Channel.Types
import           Bot.Command.Types
import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Int
import           Data.Text
import           Network.HTTP.Client
import           Servant.Common.Req (ServantError)

import qualified Web.Telegram.API.Bot.API as TG
import qualified Web.Telegram.API.Bot.Requests as TG

instance (MonadIO m) => Channel TelegramMessage (TI m) where
  sendChannel (SMR smr) = runTG TG.sendMessage smr
  sendChannel (SAR sar) = runTG TG.uploadAudio sar

runTG
  :: (MonadReader TelegramConfig m, MonadIO m, MonadError e m) =>
     (TG.Token -> t1 -> Manager -> IO (Either e t)) -> t1 -> m ()
runTG tgFun payload = do
  manager <- asks tcManager
  token <- asks tcToken
  res <- liftIO $ tgFun token payload manager
  case res of
    Left x -> throwError x
    Right x -> return ()

evalTI :: TelegramConfig -> TI m a -> m (Either ServantError a)
evalTI config e = runExceptT (runReaderT (runTI e) config)

sendMessage' :: (TelegramMessage :<: f) => Int64 -> Text -> TChan (Expr f) -> IO ()
sendMessage' userId message chan = do
  let action = inject $ SMR $ TG.sendMessageRequest (TG.ChatId userId) message
  void $ liftIO $ atomically $ writeTChan chan action

uploadAudio' :: (TelegramMessage :<: f) => Int64 -> Text -> FilePath -> TChan (Expr f) -> IO ()
uploadAudio' chatId title filepath chan = do
  let m = TG.SendAudioRequest {
    _audio_chat_id = TG.ChatId chatId,
    _audio_audio = TG.FileUpload
                   (Just "application/octet-stream") (TG.FileUploadFile filepath),
    _audio_caption = Nothing,
    _audio_duration = Nothing,
    _audio_performer = Nothing,
    _audio_title = Just title,
    _audio_disable_notification = Nothing,
    _audio_reply_to_message_id = Nothing,
    _audio_reply_markup = Nothing
    }
  void $ liftIO $ atomically $ writeTChan chan $ inject $ SAR m
