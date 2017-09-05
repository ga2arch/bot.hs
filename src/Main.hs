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

data Feeder next = Parse next
  deriving (Functor, Show)

data Youtube next = Download String next
  deriving (Functor, Show)

send :: (Functor f, MonadFree f m, Bot :<: f) => T.Text -> m ()
send text = liftF . inj $ Send text ()

prompt :: (Functor f, MonadFree f m, Bot :<: f) => T.Text -> m T.Text
prompt name = liftF . inj $ Prompt name id

parse :: (Functor f, MonadFree f m, Feeder :<: f) => m ()
parse = liftF . inj $ Parse ()

download :: String -> (Functor f, MonadFree f m, Youtube :<: f) => m ()
download url = liftF . inj $ Download url ()

-- * Interpreters

data User = User { userChan :: TChan TG.Message
                 , userBot :: BotState
                 , userChatId :: Int64
                 }

newtype UserMonad a = UserMonad { runUser :: ReaderT User IO a }
  deriving (Monad, Applicative, Functor, MonadReader User, MonadIO)

class (Functor f, Monad m, MonadReader User m) => Eval m f where
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
type Hello = Bot :+: Feeder :+: Youtube
hello = do
  send "hi, i'm the hello command"
  name <- prompt "tell me your name: "
  send $ "hi " <> name

subscribe :: Int -> Free (Bot) ()
subscribe url = do
  send $ "the url: " <> (T.pack $ show url)

-- * Servant like
data Proxy a = Proxy

data a :<|> b = a :<|> b
infixr 8 :<|>

data (a :: k) :> (b :: *)
infixr 9 :>

data Capture (a :: *)
data Done a

type family Server layout :: *
type instance Server (Done a) = Free a ()
type instance Server ((s :: Symbol) :> r) = Server r
type instance Server (a :<|> b) = Server a :<|> Server b
type instance Server (Capture a :> r) = a -> Server r

type Commands =
  "/subscribe" :> Capture Int :> Done Bot
  :<|> "/hello" :> Done Hello

handleCommands :: Server Commands
handleCommands = subscribe :<|> hello

class HasServer layout where
  route :: Main.Proxy layout -> Server layout -> T.Text -> Maybe (UserMonad ())

serve :: (HasServer layout) => Main.Proxy layout -> Server layout -> T.Text -> UserMonad ()
serve p h xs = case route p h xs of
  Just m  -> m
  Nothing -> return ()

instance (Functor f, Eval UserMonad f) => HasServer (Done f) where
  route :: Main.Proxy (Done f) -> Free f () -> T.Text -> Maybe (UserMonad ())
  route _ handler msg = Just $ iterM runAlgebra handler

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  route :: Main.Proxy(a :<|> b) -> (Server a :<|> Server b) -> T.Text -> Maybe (UserMonad ())
  route _ (handlera :<|> handlerb) xs =
        route (Main.Proxy :: Main.Proxy a) handlera xs
    <|> route (Main.Proxy :: Main.Proxy b) handlerb xs

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  route :: Main.Proxy (s :> r) -> Server r -> T.Text -> Maybe (UserMonad ())
  route _ handler text
    | symbolVal (Main.Proxy :: Main.Proxy s) `isPrefixOf` (T.unpack text) = do
        let prefix = T.pack $ symbolVal (Main.Proxy :: Main.Proxy s)
        route (Main.Proxy :: Main.Proxy r) handler $ T.drop 1 $ fromJust $ T.stripPrefix prefix text
  route _ _       _                     = Nothing

instance (Read a, HasServer r) => HasServer (Capture a :> r) where
  route :: Main.Proxy (Capture a :> r) -> (a -> Server r) -> T.Text -> Maybe (UserMonad ())
  route _ handler text = do
    a <- readMaybe $ T.unpack text
    route (Main.Proxy :: Main.Proxy r) (handler a) text
  route _ _       _        = Nothing

instance {-# OVERLAPPING #-} (HasServer r) => HasServer (Capture T.Text :> r) where
  route :: Main.Proxy (Capture T.Text :> r) -> (T.Text -> Server r) -> T.Text -> Maybe (UserMonad ())
  route _ handler text | T.length text > 0 = route (Main.Proxy :: Main.Proxy r) (handler text) text
  route _ _       _        = Nothing

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
, botRun :: forall a. (TG.TelegramClient a) -> IO (Either ServantError a)}

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
         chan <- liftIO $ newTChanIO
         _ <- liftIO . forkIO $ runReaderT (runUser processMessage) $ User chan botState chatId
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
