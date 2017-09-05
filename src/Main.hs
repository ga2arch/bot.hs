{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor, RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Maybe
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
import Data.Int
import GHC.TypeLits
import qualified STMContainers.Map as M
import qualified Web.Telegram.API.Bot.Data as TG
import qualified Web.Telegram.API.Bot.Responses as TG
import qualified Web.Telegram.API.Bot.Requests as TG
import qualified Web.Telegram.API.Bot.API as TG
import qualified Web.Telegram.API.Bot.API.Updates as TG
import qualified Data.Text as T
import Text.Read
import Debug.Trace
import Data.List

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
data Bot next  = Send T.Text next
               | Prompt T.Text (T.Text -> next)
  deriving (Functor)

data Feeder next = Subscribe T.Text next
  deriving (Functor, Show)

data Youtube next = Download String next
  deriving (Functor, Show)

send :: (Functor f, MonadFree f m, Bot :<: f) => T.Text -> m ()
send text = liftF . inj $ Send text ()

prompt :: (Functor f, MonadFree f m, Bot :<: f) => T.Text -> m T.Text
prompt name = liftF . inj $ Prompt name id

subscribe :: (Functor f, MonadFree f m, Feeder :<: f) => T.Text -> m ()
subscribe url = liftF . inj $ Subscribe url ()

download :: String -> (Functor f, MonadFree f m, Youtube :<: f) => m ()
download url = liftF . inj $ Download url ()

-- * Interpreters

data FeederEvent = News String | SubscribeUrl String String (TChan FeederEvent)
data YoutubeEvent = DownloadUrl String
data CommandEvent = FE FeederEvent | YE YoutubeEvent

subscribeUrl :: String -> String -> UserMonad (TChan FeederEvent)
subscribeUrl chatId url = do
  cmdChan <- asks userCmdChan
  out <- liftIO newTChanIO
  liftIO . atomically $ writeTChan cmdChan (FE $ SubscribeUrl chatId url out)
  return out

data UserConfig = UserConfig { userChan :: TChan TG.Message
                             , userBot :: BotState
                             , userChatId :: Int64
                             , userCmdChan :: TChan CommandEvent
                             }

newtype UserMonad a = UserMonad { runUser :: ReaderT UserConfig IO a }
  deriving (Monad, Applicative, Functor, MonadReader UserConfig, MonadIO)

class (Functor f, Monad m, MonadReader UserConfig m) => Eval m f where
  runAlgebra :: f (m a) -> m a

instance Eval UserMonad Bot where
  runAlgebra (Send text next) = do
    bot <- asks userBot
    chatId <- asks userChatId
    liftIO . (botRun bot) $ sendMessage chatId text
    next

  runAlgebra (Prompt text next) = do
    bot <- asks userBot
    chan <- asks userChan
    chatId <- asks userChatId
    liftIO . (botRun bot) $ sendMessage chatId text
    TG.Message{chat=TG.Chat{chat_id=chatId}, text=Just text} <- liftIO $ atomically $ readTChan chan
    next text

instance Eval UserMonad Feeder where
  runAlgebra (Subscribe url next) = do
    config <- ask
    chatId <- asks userChatId
    newsChan <- subscribeUrl (show chatId) (T.unpack url)
    _ <- liftIO . forkIO $ flip runReaderT config $ (runUser $ readNews newsChan)
    next
   where
    readNews newsChan = forever $ do
      bot <- asks userBot
      chatId <- asks userChatId
      (News title) <- liftIO $ atomically $ readTChan newsChan
      liftIO . (botRun bot) $ sendMessage chatId $ T.pack title
      return ()

instance Eval UserMonad Youtube where
  runAlgebra (Download url next) = do
    liftIO $ print $ "download: " ++ url
    next

instance (Monad m, Eval m f, Eval m g) => Eval m (f :+: g) where
  runAlgebra (Inl r) = runAlgebra r
  runAlgebra (Inr r) = runAlgebra r

-- * Servant like
data Proxy a = Proxy

data a :<|> b = a :<|> b
infixr 8 :<|>

data (a :: k) :> (b :: *)
infixr 9 :>

data Capture (a :: *)
data Run a

type family Server layout :: *
type instance Server (Run a) = Free a ()
type instance Server ((s :: Symbol) :> r) = Server r
type instance Server (a :<|> b) = Server a :<|> Server b
type instance Server (Capture a :> r) = a -> Server r

class HasServer layout where
  route :: Main.Proxy layout -> Server layout -> T.Text -> Maybe (UserMonad ())

serve :: (HasServer layout) => Main.Proxy layout -> Server layout -> T.Text -> UserMonad ()
serve p h xs = case route p h xs of
  Just m  -> m
  Nothing -> return ()

instance (Functor f, Eval UserMonad f) => HasServer (Run f) where
  route :: Main.Proxy (Run f) -> Free f () -> T.Text -> Maybe (UserMonad ())
  route _ handler msg = Just $ iterM runAlgebra handler

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  route :: Main.Proxy(a :<|> b) -> (Server a :<|> Server b) -> T.Text -> Maybe (UserMonad ())
  route _ (handlera :<|> handlerb) xs =
        route (Main.Proxy :: Main.Proxy a) handlera xs
    <|> route (Main.Proxy :: Main.Proxy b) handlerb xs

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  route :: Main.Proxy (s :> r) -> Server r -> T.Text -> Maybe (UserMonad ())
  route _ handler text = do
    let prefix = T.pack $ symbolVal (Main.Proxy :: Main.Proxy s)
    if prefix `T.isPrefixOf` text
      then route (Main.Proxy :: Main.Proxy r) handler $ fromJust $ T.stripPrefix prefix text
      else Nothing

instance (Read a, HasServer r) => HasServer (Capture a :> r) where
  route :: Main.Proxy (Capture a :> r) -> (a -> Server r) -> T.Text -> Maybe (UserMonad ())
  route _ handler text = do
    a <- readMaybe $ T.unpack text
    route (Main.Proxy :: Main.Proxy r) (handler a) text

instance {-# OVERLAPPING #-} (HasServer r) => HasServer (Capture T.Text :> r) where
  route :: Main.Proxy (Capture T.Text :> r) -> (T.Text -> Server r) -> T.Text -> Maybe (UserMonad ())
  route _ handler text | T.length text > 0 = route (Main.Proxy :: Main.Proxy r) (handler text) text
  route _ _       _        = Nothing

-- * Commands
type HelloCmd = Bot
hello = do
  send "hi, i'm the hello command"
  name <- prompt "tell me your name: "
  send $ "hi " <> name

type SubscribeCmd = Bot :+: Feeder
subscribeCmd url = do
  subscribe url
  send $ "the url: " <> url

type Commands =
  "/subscribe" :> Capture T.Text :> Run SubscribeCmd
  :<|> "/hello" :> Run HelloCmd

handleCommands :: Server Commands
handleCommands = subscribeCmd :<|> hello

-- * Bot
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

data BotState = BotState {
  botUsers :: M.Map Int (TChan TG.Message)
  , botCmdChan :: TChan CommandEvent
  , botRun :: forall a. (TG.TelegramClient a) -> IO (Either ServantError a)
  }

dispatch :: TG.Update -> ReaderT BotState IO ()
dispatch TG.Update{..} = onMessage message
  where
   onMessage (Just m@TG.Message{..}) = onUser m from
   onMessage _ = return ()

   onUser m@TG.Message{chat=TG.Chat{chat_id=chatId}} (Just TG.User{..}) = do
     users <- asks botUsers
     user <- liftIO . atomically $ M.lookup user_id users
     case user of
       Just chan -> do
         liftIO . atomically $ writeTChan chan m
         return ()
       Nothing  -> do
         botState <- ask
         cmdChan <- asks botCmdChan
         cmdChan' <- liftIO . atomically $ dupTChan cmdChan
         chan <- liftIO $ newTChanIO
         _ <- liftIO . forkIO $ runReaderT (runUser processMessage) $
           UserConfig chan botState chatId cmdChan'
         liftIO . atomically $ do
           M.insert chan user_id users
           writeTChan chan m
         return ()
   onUser _ _ = return ()

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

processMessage :: UserMonad ()
processMessage = forever $ do
  chan <- asks userChan
  m <- liftIO . atomically $ readTChan chan
  go m
 where
  go TG.Message{chat=TG.Chat{chat_id=chatId}, text=Just text} = do
    serve (Main.Proxy :: Main.Proxy Commands) handleCommands text
    return ()
  go _ = return ()

feeder chan = do
  _ <- forkIO $ forever $ do
    event <- atomically $ readTChan chan
    case event of
      (FE (SubscribeUrl chatId url out)) -> onSubscribe chatId url out

  forever $ do
    threadDelay 10000000
    print "checking"
  where
   onSubscribe chatId url out = atomically $ writeTChan out $ News "prova"

main = do
  let token = TG.Token "bot130053600:AAFKjzm0YiyVBQa8mYfKkNNXTJ8mDd2zhgI"
  users <- M.newIO
  manager <- newManager tlsManagerSettings
  cmdChan <- newTChanIO
  forkIO $ feeder cmdChan
  let botState = BotState users cmdChan $ flip (flip TG.runClient token) manager
  go token $ \m -> runReaderT (dispatch m) botState
 where
  go token f = do
    manager <- newManager tlsManagerSettings
    getUpdates token manager Nothing f
