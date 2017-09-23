{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Bot.Command.Base where

import           Bot.Channel.Telegram
import           Bot.Command.Base.Types
import           Bot.Command.Types
import           Bot.Types
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad.Free
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)

import qualified Data.Text as T
import qualified Web.Telegram.API.Bot.Data as TG

send :: (MonadFree f m, Base :<: f) => Text -> m ()
send text = liftF . inj $ Send text ()

uploadAudio :: (MonadFree f m, Base :<: f) => Text -> FilePath -> m ()
uploadAudio title filepath = liftF . inj $ UploadAudio title filepath ()

prompt :: (MonadFree f m, Base :<: f) => Text -> m Text
prompt name = liftF . inj $ Prompt name id

pushNamespace :: (MonadFree f m, Base :<: f) => Text -> m ()
pushNamespace name = liftF . inj $ PushNamespace name ()

popNamespace :: (MonadFree f m, Base :<: f) => Text -> m ()
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
