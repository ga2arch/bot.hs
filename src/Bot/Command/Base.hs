{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Bot.Command.Base where

import           Bot.Command.Base.Types
import           Bot.Command.Types
import           Bot.Types
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad.Free
import           Control.Monad.Reader

import qualified Data.Text as T
import qualified Web.Telegram.API.Bot.Data as TG
import qualified Web.Telegram.API.Bot.Responses as TG
import qualified Web.Telegram.API.Bot.Requests as TG
import qualified Web.Telegram.API.Bot.API as TG
import qualified Web.Telegram.API.Bot.API.Updates as TG

send :: (Functor f, MonadFree f m, Base :<: f) => T.Text -> m ()
send text = liftF . inj $ Send text ()

uploadAudio :: (Functor f, MonadFree f m, Base :<: f) => T.Text -> FilePath -> m ()
uploadAudio title filepath = liftF . inj $ UploadAudio title filepath ()

prompt :: (Functor f, MonadFree f m, Base :<: f) => T.Text -> m T.Text
prompt name = liftF . inj $ Prompt name id

instance Eval UserMonad Base where
  runAlgebra (Send text next) = do
    bot <- asks userBot
    chatId <- asks userChatId
    call $ sendMessage chatId text
    next

  runAlgebra (UploadAudio title filepath next) = do
    bot <- asks userBot
    chatId <- asks userChatId
    call $ uploadAudio chatId title filepath
    next
   where
     uploadAudio chatId title filepath = do
       TG.uploadAudioM TG.SendAudioRequest {
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

  runAlgebra (Prompt text next) = do
    bot <- asks userBot
    inChan <- asks userChan
    chatId <- asks userChatId
    call $ sendMessage chatId text
    TG.Message{chat=TG.Chat{chat_id=chatId}, text=Just text} <- liftIO . atomically
      $ readTChan inChan
    next text

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
