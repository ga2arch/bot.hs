{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Bot.Command.Base where

import           Bot.Command.Base.Types
import           Bot.Command.Types
import Bot.Channel.Telegram
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

send text = liftF . inj $ Send text ()
uploadAudio title filepath = liftF . inj $ UploadAudio title filepath ()
prompt name = liftF . inj $ Prompt name id
pushNamespace name = liftF . inj $ PushNamespace name ()
popNamespace name = liftF . inj $ PopNamespace name ()

instance Eval UserMonad Base where
  runAlgebra (Send text next) = do
    bot <- asks userBot
    chatId <- asks userChatId
    liftIO $ sendMessage' chatId text (botCmdChan bot)
    next

  runAlgebra (PushNamespace name next) = do
    namespace <- asks userNamespace
    liftIO $ modifyMVar_ namespace (\n -> return $ n <> name)
    next

  runAlgebra (PopNamespace name next) = do
    namespace <- asks userNamespace
    liftIO $ modifyMVar_ namespace (\n -> return $ fromJust $ T.stripSuffix n name)
    next

  runAlgebra (UploadAudio title filepath next) = do
    bot <- asks userBot
    chatId <- asks userChatId
    liftIO $ uploadAudio' chatId title filepath (botCmdChan bot)
    next

  runAlgebra (Prompt text next) = do
    bot <- asks userBot
    inChan <- asks userChan
    chatId <- asks userChatId
    liftIO $ sendMessage' chatId text (botCmdChan bot)
    TG.Message{chat=TG.Chat{chat_id=chatId}, text=Just text} <- liftIO . atomically $ readTChan inChan
    next text
