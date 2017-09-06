{-# LANGUAGE ExistentialQuantification #-}
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
{-# LANGUAGE ScopedTypeVariables, ImplicitParams #-}
module Main where

import Control.Monad
import Control.Applicative
import Control.Monad.Catch
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
import Network.URI
import Data.Int
import GHC.TypeLits
import Text.Feed.Import
import Text.Feed.Types
import qualified Text.Atom.Feed as Atom
import qualified Text.RSS.Syntax as RSS
import qualified Text.RSS1.Syntax as RSS1
import qualified STMContainers.Map as M
import qualified Web.Telegram.API.Bot.Data as TG
import qualified Web.Telegram.API.Bot.Responses as TG
import qualified Web.Telegram.API.Bot.Requests as TG
import qualified Web.Telegram.API.Bot.API as TG
import qualified Web.Telegram.API.Bot.API.Updates as TG
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.Redis as R
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
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
      then route (Main.Proxy :: Main.Proxy r) handler $ T.drop 1 $ fromJust $ T.stripPrefix prefix text
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

-- * DSL
data Bot next  = Send T.Text next
               | Prompt T.Text (T.Text -> next)
  deriving (Functor)

data Feeder next = Subscribe (TChan FeederEvent) T.Text next
  deriving (Functor)

data Youtube next = Download String next
  deriving (Functor, Show)

send :: (Functor f, MonadFree f m, Bot :<: f) => T.Text -> m ()
send text = liftF . inj $ Send text ()

prompt :: (Functor f, MonadFree f m, Bot :<: f) => T.Text -> m T.Text
prompt name = liftF . inj $ Prompt name id

subscribe :: (?inChan :: TChan FeederEvent, Functor f, MonadFree f m, Feeder :<: f) => T.Text -> m ()
subscribe url = liftF . inj $ Subscribe ?inChan url ()

download :: String -> (Functor f, MonadFree f m, Youtube :<: f) => m ()
download url = liftF . inj $ Download url ()

-- * Interpreters

data FeederEvent = News String | SubscribeUrl Int64 T.Text
data YoutubeEvent = DownloadUrl String
data CommandEvent = FE FeederEvent | YE YoutubeEvent

subscribeUrl :: TChan FeederEvent -> Int64 -> T.Text -> UserMonad ()
subscribeUrl inChan chatId url =
  liftIO . atomically $ writeTChan inChan (SubscribeUrl chatId url)

data UserConfig = UserConfig { userChan :: TChan TG.Message
                             , userBot :: BotConfig
                             , userChatId :: Int64
                             }

call action = do
  botConfig <- asks userBot
  let config = channelConfig botConfig
  liftIO $ TG.runClient action (tgToken config) (tgManager config)

newtype UserMonad a = UserMonad { runUser :: ReaderT UserConfig IO a }
  deriving (Monad, Applicative, Functor, MonadReader UserConfig, MonadIO)

class (Functor f, Monad m, MonadReader UserConfig m) => Eval m f where
  runAlgebra :: f (m a) -> m a

instance Eval UserMonad Bot where
  runAlgebra (Send text next) = do
    bot <- asks userBot
    chatId <- asks userChatId
    call $ sendMessage chatId text
    next

  runAlgebra (Prompt text next) = do
    bot <- asks userBot
    inChan <- asks userChan
    chatId <- asks userChatId
    call $ sendMessage chatId text
    TG.Message{chat=TG.Chat{chat_id=chatId}, text=Just text} <- liftIO . atomically
      $ readTChan inChan
    next text

instance Eval UserMonad Feeder where
  runAlgebra (Subscribe inChan url next) = do
    config <- ask
    chatId <- asks userChatId
    subscribeUrl inChan chatId url
    next

instance Eval UserMonad Youtube where
  runAlgebra (Download url next) = do
    liftIO $ print $ "download: " ++ url
    next

instance (Monad m, Eval m f, Eval m g) => Eval m (f :+: g) where
  runAlgebra (Inl r) = runAlgebra r
  runAlgebra (Inr r) = runAlgebra r

-- * Commands
type HelloCmd = Bot
hello = do
  send "hi, i'm the hello command"
  name <- prompt "tell me your name: "
  send $ "hi " <> name

type SubscribeCmd = Bot :+: Feeder
subscribeCmd :: (?inChan :: TChan FeederEvent) => T.Text -> Free SubscribeCmd ()
subscribeCmd url = do
  if isURI $ T.unpack url
    then do
      subscribe url
      send $ "subscribed to: " <> url
    else do
      send "invalid url"

type Commands =
  "/subscribe" :> Capture T.Text :> Run SubscribeCmd
  :<|> "/hello" :> Run HelloCmd

handleCommands :: (?inChan :: TChan FeederEvent) => Server Commands
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

data BotConfig = BotConfig {
  botUsers :: M.Map Int (TChan TG.Message)
  , channelConfig :: TelegramConfig
  }

data TelegramConfig = TelegramConfig {
  tgToken :: TG.Token
  , tgManager :: Manager
  }

dispatch :: (?inChan :: TChan FeederEvent) => TG.Update -> ReaderT BotConfig IO ()
dispatch TG.Update{..} = do
  onMessage message
  where
   onMessage (Just m@TG.Message{..}) = onUser m from
   onMessage _ = return ()

   onUser m@TG.Message{chat=TG.Chat{chat_id=chatId}} (Just TG.User{..}) = do
     users <- asks botUsers
     user <- liftIO . atomically $ M.lookup user_id users
     case user of
       Just inChan -> do
         liftIO . atomically $ writeTChan inChan m
         return ()
       Nothing  -> do
         botState <- ask
         inChan <- liftIO $ newTChanIO
         liftIO . forkIO $ runReaderT (runUser processMessage) $ UserConfig inChan botState chatId
         liftIO . atomically $ do
           M.insert inChan user_id users
           writeTChan inChan m
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

processMessage :: (?inChan :: TChan FeederEvent) => UserMonad ()
processMessage = forever $ do
    inChan <- asks userChan
    m <- liftIO . atomically $ readTChan inChan
    go m
 where
  go TG.Message{chat=TG.Chat{chat_id=chatId}, text=Just text} = do
    serve (Main.Proxy :: Main.Proxy Commands) handleCommands text
    return ()
  go _ = return ()

mkTelegram token = do
  manager <- newManager tlsManagerSettings
  return $ TelegramConfig token manager

runBot channelConfig = do
  users <- M.newIO
  return $ BotConfig users channelConfig

main = do
  let token = TG.Token "bot130053600:AAFKjzm0YiyVBQa8mYfKkNNXTJ8mDd2zhgI"

  channelConfig <- mkTelegram token
  feederChan <- feeder channelConfig
  botConfig <- runBot channelConfig

  let ?inChan = feederChan
    in go token $ \m -> runReaderT (dispatch m) botConfig
 where
  go token f = do
    manager <- newManager tlsManagerSettings
    getUpdates token manager Nothing f

-- * Jobs
feeder config = do
  inChan <- newTChanIO
  conn <- R.checkedConnect R.defaultConnectInfo
  manager <- newManager tlsManagerSettings

  _ <- forkIO $ forever $ do
    event <- atomically $ readTChan inChan
    case event of
      (SubscribeUrl chatId url) -> onSubscribe conn config chatId url

  forkIO . forever $ do
    print "checking feed"
    checkFeeds conn config manager
    threadDelay 10000000

  return inChan
  where
   onSubscribe conn config chatId url = do
     let url' = T.encodeUtf8 url
     R.runRedis conn $ do
       R.sadd "feeds" [url']
       R.lpush ("feed:" <> url') [C.pack $ show chatId]

   checkFeeds conn config manager = do
     (Right urls) <- R.runRedis conn $ R.smembers "feeds"
     forM_ urls $ \url ->
                    catchAll
                    (loadFeed conn config manager url)
                    (\ex -> do
                        (Right users) <- R.runRedis conn $ R.lrange ("feed:" <> url) 0 (-1)
                        forM_ users $ \user -> do
                          let message = "error processing " <> T.decodeUtf8 url <> " unsubscribing"
                          call config $ sendMessage (read $ C.unpack user) message
                        R.runRedis conn $ do
                          R.srem "feeds" [url]
                          R.del [("feed:" <> url)]
                          return ())

   loadFeed conn config manager url = do
     print $ "loading " ++ C.unpack url
     request <- parseRequest $ C.unpack url
     response <- httpLbs request manager
     case parseFeedSource $ responseBody response of
       Just feed -> processFeed conn config url feed
       Nothing -> return ()

   processFeed conn config url (AtomFeed Atom.Feed{..}) = undefined
   processFeed conn config url (RSSFeed RSS.RSS{rssChannel=RSS.RSSChannel{..}}) = do
     let firstTitle = C.pack $ fromJust $ RSS.rssItemTitle $ head rssItems
     lastKnownTitle <- R.runRedis conn $ R.get ("feed:" <> url <> ":last")
     case lastKnownTitle of
       Right (Just title) -> when (firstTitle /= title) $ do
         sendMessages conn config url rssItems
         updateLastSeen conn url rssItems
       Right Nothing  -> updateLastSeen conn url rssItems
       Left x -> return ()

   sendMessages conn config url rssItems = do
     (Right users) <- R.runRedis conn $ R.lrange ("feed:" <> url) 0 (-1)
     forM_ users $ \user -> do
       forM_ rssItems $ \RSS.RSSItem{..} -> do
         let title = T.pack $ fromJust rssItemTitle
         let url = T.pack $ fromJust rssItemLink
         let message = title <> "\n\n" <> url
         call config $ sendMessage (read $ C.unpack user) message

   updateLastSeen conn url rssItems = do
     R.runRedis conn $ do
       let lastTitle = C.pack $ fromJust $ RSS.rssItemTitle $ head rssItems
       R.set ("feed:" <> url <> ":last") lastTitle
       return ()

   call config action = liftIO $ TG.runClient action (tgToken config) (tgManager config)
