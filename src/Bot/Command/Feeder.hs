{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Bot.Command.Feeder where

import Bot.Types
import Bot.Command.Types
import Bot.Command.Feeder.Types
import Bot.Command.Feeder.Database
import Bot.Command.Base
import Bot.Command.Base.Types

import Control.Monad.Free
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Data.Int
import Data.Monoid
import Database.Persist (entityVal)
import Data.Maybe
import Data.List
import Text.Feed.Import
import Text.Feed.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.URI
import Network.HTTP.Types.Header

import qualified Data.CaseInsensitive as CI
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

subscribe :: (?feederChan :: TChan FeederEvent, Functor f, MonadFree f m, Feeder :<: f) => T.Text -> m ()
subscribe url = liftF . inj $ Subscribe ?feederChan url ()

validate :: (Functor f, MonadFree f m, Feeder :<: f) => T.Text -> m Bool
validate url = liftF . inj $ Validate url id

subscribe' :: TChan FeederEvent -> Int64 -> T.Text -> UserMonad ()
subscribe' feederChan userId url =
  liftIO . atomically $ writeTChan feederChan (SubscribeUrl userId url)

instance Eval UserMonad Feeder where
  runAlgebra (Subscribe inChan url next) = do
    config <- ask
    chatId <- asks userChatId
    subscribe' inChan chatId url
    next

  runAlgebra (Validate url next) = do
    if (isURI (T.unpack url))
      then do
        manager <- liftIO $ newManager tlsManagerSettings
        request <- liftIO $ parseRequest (T.unpack url)
        response <- liftIO $ httpNoBody request manager
        let headers = responseHeaders response
        case find ((== hContentType).fst) headers of
          Just header -> next $ "xml" `C.isInfixOf` (snd header)
          Nothing -> next False
      else next False


subscribeCommand :: (?feederChan :: TChan FeederEvent) => T.Text -> Free (Base :+: Feeder) ()
subscribeCommand url = do
  valid <- validate url
  if valid
    then do
      subscribe url
      send $ "subscribed to: " <> url
    else do
      send "invalid url"

feeder :: TelegramConfig -> IO (TChan FeederEvent)
feeder botConfig = do
  feederChan <- newTChanIO
  pool <- initDb
  manager <- newManager tlsManagerSettings
  let feederConfig = FeederConfig pool botConfig manager

  liftIO . forkIO . forever $ do
    event <- atomically $ readTChan feederChan
    case event of
      (SubscribeUrl userId url) -> runReaderT (runFeeder $ onSubscribe userId url) feederConfig

  liftIO . forkIO . forever $ do
    print "checking feed"
    runReaderT (runFeeder checkFeeds) feederConfig
    threadDelay 10000000

  return feederChan
  where
   onSubscribe userId url = do
     let url' = T.encodeUtf8 url
     addSubscription (show userId) url
     return url'

   checkFeeds = do
     feeds <- getFeeds
     forM_ feeds $ \feed ->
                    catchAll
                    (loadFeed feed)
                    (\ex -> return ())

   loadFeed feed@(entityVal -> (Feed feedUrl feedUpdateDate)) = do
     manager <- asks fManager
     liftIO $ print $ "loading " ++ (T.unpack feedUrl)
     request <- liftIO $ parseRequest $ T.unpack feedUrl
     response <- liftIO $ httpLbs request manager
     case parseFeedSource $ responseBody response of
       Just content -> processFeed feed content
       Nothing -> return ()

   processFeed feed (AtomFeed Atom.Feed{..}) = undefined
   processFeed feed@(entityVal -> Feed feedUrl lastGuid) (RSSFeed RSS.RSS{rssChannel=RSS.RSSChannel{..}}) = do
     let firstGuid = rssGuid $ head rssItems
     when (isJust lastGuid) $ do
       users <- getUsersByFeed feed
       sendMessages users $
         takeWhile (\item -> (rssGuid item) /= (fromJust lastGuid)) rssItems
     updateLastGuid feed (rssGuid $ head rssItems)

   sendMessages users rssItems = do
     forM_ rssItems $ \RSS.RSSItem{..} -> do
       let title = T.pack $ fromJust rssItemTitle
       let url = T.pack $ fromJust rssItemLink
       let message = title <> "\n\n" <> url
       forM_ users $ \(entityVal -> User userId) ->
         call $ sendMessage (read userId) message

   rssGuid = T.pack . RSS.rssGuidValue . fromJust . RSS.rssItemGuid

   call action = do
     config <- asks fTelegramConfig
     liftIO $ TG.runClient action (tgToken config) (tgManager config)
