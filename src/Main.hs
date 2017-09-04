{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor, RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Free
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Servant.Common.Req
import qualified STMContainers.Map as M
import qualified Web.Telegram.API.Bot.Data as TG
import qualified Web.Telegram.API.Bot.Responses as TG
import qualified Web.Telegram.API.Bot.Requests as TG
import qualified Web.Telegram.API.Bot.API as TG
import qualified Web.Telegram.API.Bot.API.Updates as TG
import qualified Data.Text as T

-- * A la carte
data (f :+: g) e = Inl (f e) | Inr (g e)
infixr 7 :+:

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl e) = Inl (fmap f e)
  fmap f (Inr e) = Inr (fmap f e)

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f => f :<: f where
  inj = id

instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj

-- * DSL
data Bot next  = Send next
               | Prompt String (String -> next)
  deriving (Functor)

data Feeder next = Parse next
  deriving (Functor, Show)

data Youtube next = Download String next
  deriving (Functor, Show)

send :: (Functor f, MonadFree f m, Bot :<: f) => m ()
send = liftF . inj $ Send ()

prompt :: (Functor f, MonadFree f m, Bot :<: f) => String -> m String
prompt name = liftF . inj $ Prompt name id

parse :: (Functor f, MonadFree f m, Feeder :<: f) => m ()
parse = liftF . inj $ Parse ()

download :: String -> (Functor f, MonadFree f m, Youtube :<: f) => m ()
download url = liftF . inj $ Download url ()

-- * Interpreters

data User = User {userChan :: TChan String
                 }

newtype UserMonad a = UserMonad { runUser :: ReaderT User IO a }
  deriving (Monad, Applicative, Functor, MonadReader User, MonadIO)

class (Functor f, Monad m, MonadReader User m) => Eval m f where
  runAlgebra :: f (m a) -> m a

instance Eval UserMonad Bot where
  runAlgebra (Send next) = do
    liftIO $ print "send"
    next

  runAlgebra (Prompt name next) = do
    liftIO $ print "prompt"
    chan <- asks userChan
    resp <- liftIO $ atomically $ readTChan chan
    next resp

instance Eval UserMonad Feeder where
  runAlgebra (Parse next) = do
    liftIO $ print "parse"
    next

instance Eval UserMonad Youtube where
  runAlgebra (Download url next) = do
    liftIO $ print $ "download: " ++ url
    next

instance (Monad m, Eval m f, Eval m g) => Eval m (f :+: g) where
  runAlgebra (Inl r) = runAlgebra r
  runAlgebra (Inr r) = runAlgebra r

--run = iterM runAlgebra

-- * Plugins
plugin :: Free (Bot :+: Feeder :+: Youtube) ()
plugin = do
  send
  parse
  name <- prompt "url to download: "
  download name

-- * Bot

getUpdates token manager lastId f = do
  updates <- TG.getUpdates token lastId (Just 20) (Just 10) manager
  case updates of
    Right (TG.Response result _) -> do
      mapM_ f result
      let (TG.Update {TG.update_id=lastId}) = last result
      getUpdates token manager (Just $ lastId+1) f
    Left  x -> print x

data BotState = BotState {
  botUsers :: M.Map Int (TChan TG.Message)
, botRun :: forall a. (TG.TelegramClient a) -> IO (Either ServantError a)}

dispatch :: TG.Update -> ReaderT BotState IO ()
dispatch TG.Update{..} = onMessage message
  where
   onMessage (Just m@TG.Message{..}) = onUser m from
   onMessage Nothing = return ()
   onUser m (Just TG.User{..}) = do
     users <- asks botUsers
     user <- liftIO . atomically $ M.lookup user_id users
     case user of
       Just chan -> do
         liftIO . atomically $ writeTChan chan m
         return ()
       Nothing  -> do
         botState <- ask
         chan <- liftIO $ newTChanIO
         _ <- liftIO . forkIO $ runReaderT (processMessage chan) botState
         liftIO . atomically $ do
           M.insert chan user_id users
           writeTChan chan m
         return ()
   onUser _ Nothing = return ()

processMessage :: TChan TG.Message -> ReaderT BotState IO ()
processMessage chan = forever $ do
  m <- liftIO . atomically $ readTChan chan
  go m
 where
  go TG.Message{chat=TG.Chat{chat_id=chatId}, text=Just text} = do
    run <- asks botRun
    liftIO . run $ TG.sendMessageM TG.SendMessageRequest {
      message_chat_id = TG.ChatId chatId,
      message_text = text,
      message_parse_mode = Nothing,
      message_disable_web_page_preview = Just True,
      message_disable_notification = Nothing,
      message_reply_to_message_id = Nothing,
      message_reply_markup = Nothing
      }
    return ()
  go _ = return ()

main = do
  let token = TG.Token "bot130053600:AAFKjzm0YiyVBQa8mYfKkNNXTJ8mDd2zhgI"
  users <- M.newIO
  manager <- newManager tlsManagerSettings
  let botState = BotState users $ flip (flip TG.runClient token) manager
  go token $ \m -> runReaderT (dispatch m) botState
 where
  go token f = do
    manager <- newManager tlsManagerSettings
    getUpdates token manager Nothing f
