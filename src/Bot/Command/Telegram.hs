{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Bot.Command.Telegram where

import           Bot.Command.Telegram.Types
import           Bot.Command.Types
import Bot.Command.Route
import           Bot.Types
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad.Free
import           Control.Monad.Reader
import Data.Monoid
import Data.Maybe

import qualified Data.Text as T
import qualified Web.Telegram.API.Bot.Data as TG
import qualified Web.Telegram.API.Bot.Responses as TG
import qualified Web.Telegram.API.Bot.Requests as TG
import qualified Web.Telegram.API.Bot.API as TG
import qualified Web.Telegram.API.Bot.API.Updates as TG

sendKeyboard text buttons = liftF . inj $ SendKeyboard text buttons ()

instance Eval UserMonad Telegram where
  runAlgebra (SendKeyboard text buttons next) = do
    bot <- asks userBot
    chatId <- asks userChatId

    call $ sendMessage chatId text $ mkKeyboard buttons
    next

mkKeyboard buttons = TG.ReplyInlineKeyboardMarkup $ map mkRow buttons
mkRow row = map mkButton row
mkButton InlineKeyboardButton{..} = TG.InlineKeyboardButton {
  ikb_text = ikbLabel,
  ikb_url = Nothing,
  ikb_callback_data = Just $ help ikbCommand T.empty,
  ikb_switch_inline_query = Nothing,
  ikb_callback_game = Nothing,
  ikb_switch_inline_query_current_chat = Nothing,
  ikb_pay = Nothing
  }

sendMessage chatId text keyboard = do
  TG.sendMessageM TG.SendMessageRequest {
    message_chat_id = TG.ChatId chatId,
    message_text = text,
    message_parse_mode = Nothing,
    message_disable_web_page_preview = Just True,
    message_disable_notification = Nothing,
    message_reply_to_message_id = Nothing,
    message_reply_markup = Just keyboard
    }


call action = do
  botConfig <- asks userBot
  let config = channelConfig botConfig
  liftIO $ TG.runClient action (tgToken config) (tgManager config)
