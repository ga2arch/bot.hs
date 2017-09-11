{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Bot.Command.Feeder where

import           Bot.Command.Base
import           Bot.Command.Base.Types
import           Bot.Command.Feeder.Database
import           Bot.Command.Feeder.Database.Types
import           Bot.Command.Feeder.Types
import           Bot.Command.Types
import           Bot.Types
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad.Catch
import           Control.Monad.Free
import Control.Monad.Logger
import Data.Time.LocalTime
import Data.Time.Format
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Data.Int
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Database.Persist (entityVal, Entity)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Header
import           Network.URI
import           Text.Feed.Import
import           Text.Feed.Types
import System.Log.FastLogger

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.Redis as R
import qualified STMContainers.Map as M
import qualified Text.Atom.Feed as Atom
import qualified Text.RSS.Syntax as RSS
import qualified Text.RSS1.Syntax as RSS1
import qualified Web.Telegram.API.Bot.API as TG
import qualified Web.Telegram.API.Bot.API.Updates as TG
import qualified Web.Telegram.API.Bot.Data as TG
import qualified Web.Telegram.API.Bot.Requests as TG
import qualified Web.Telegram.API.Bot.Responses as TG

subscribe :: (?feederChan :: TChan FeederEvent, Functor f, MonadFree f m, Feeder :<: f)
          => T.Text -> m ()
subscribe url = liftF . inj $ Subscribe ?feederChan url ()

unsubscribe :: (?feederChan :: TChan FeederEvent, Functor f, MonadFree f m, Feeder :<: f)
          => T.Text -> m ()
unsubscribe url = liftF . inj $ Unsubscribe ?feederChan url ()

validate :: (Functor f, MonadFree f m, Feeder :<: f) => T.Text -> m Bool
validate url = liftF . inj $ Validate url id

getFeeds :: (?feederChan :: TChan FeederEvent, Functor f, MonadFree f m, Feeder :<: f)
         => m ([Entity Bot.Command.Feeder.Database.Types.Feed])
getFeeds = liftF . inj $ GetFeeds ?feederChan id

instance Eval UserMonad Feeder where
  runAlgebra (Subscribe feederChan url next) = do
    config <- ask
    userId <- asks userChatId
    liftIO . atomically $ writeTChan feederChan (SubscribeUrl userId url)
    next

  runAlgebra (Unsubscribe feederChan url next) = do
    config <- ask
    userId <- asks userChatId
    liftIO . atomically $ writeTChan feederChan (UnsubscribeUrl userId url)
    next

  runAlgebra (GetFeeds feederChan next) = do
    chan <- liftIO newTChanIO
    userId <- asks userChatId
    liftIO . atomically $ writeTChan feederChan (ListFeeds (show userId) chan)
    (Feeds feeds) <- liftIO . atomically $ readTChan chan
    next feeds

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

listCommand :: (?feederChan :: TChan FeederEvent) => Free (Base :+: Feeder) ()
listCommand = do
  feeds <- getFeeds
  if (null feeds)
    then send "You don't have any subscription"
    else do
      let urls = map (\(entityVal -> (Feed url _ _)) -> url) feeds
      send $ "You are subscribed to: \n" <> T.intercalate "\n" urls

unsubscribeCommand :: (?feederChan :: TChan FeederEvent) => T.Text -> Free (Base :+: Feeder) ()
unsubscribeCommand url = do
  unsubscribe url
  send "unsubscribed"

feeder :: TelegramConfig -> IO (TChan FeederEvent)
feeder botConfig = do
  feederChan <- newTChanIO
  pool <- initDb
  manager <- newManager tlsManagerSettings
  logOut <- newStdoutLoggerSet defaultBufSize
  let feederConfig = FeederConfig pool botConfig manager logOut

  liftIO . forkIO . forever $ do
    event <- atomically $ readTChan feederChan
    runReaderT (runFeeder (case event of
      SubscribeUrl userId url -> onSubscribe userId url
      ListFeeds userId chan -> onListFeeds userId chan
      UnsubscribeUrl userId url ->  onUnsubscribe userId url)) feederConfig

  liftIO . forkIO . forever $ do
    runReaderT (runFeeder checkFeeds) feederConfig
    threadDelay 60000000

  return feederChan
  where
   onSubscribe userId url = do
     let url' = T.encodeUtf8 url
     addSubscription (show userId) url
     return ()

   onUnsubscribe userId url = do
     let url' = T.encodeUtf8 url
     removeSubscription (show userId) url
     return ()

   onListFeeds userId chan = do
     feeds <- loadFeedsByUser userId
     liftIO . atomically $ writeTChan chan (Feeds feeds)
     return ()

   checkFeeds = do
     $(logDebug) "checking feed"
     feeds <- loadFeeds
     forM_ feeds $ \feed ->
                    catchAll
                    (loadFeed feed)
                    (\ex -> onFeedError ex feed)

   onFeedError ex feed@(entityVal -> Feed feedUrl _ _) = do
     liftIO $ print ex
     users <- loadUsersByFeed feed
     forM_ users $ \(entityVal -> User userId) ->
         call $ sendMessage (read userId)
                            ("error processing feed: " <> feedUrl <> " unsubscribing")
     removeFeed feed

   loadFeed feed@(entityVal -> (Feed feedUrl _ _)) = do
     manager <- asks fManager
     liftIO $ print $ "loading " ++ (T.unpack feedUrl)
     request <- liftIO $ parseRequest $ T.unpack feedUrl
     response <- liftIO $ httpLbs request manager
     case parseFeedSource $ responseBody response of
       Just content -> processFeed feed content
       Nothing -> return ()

   processFeed feed (AtomFeed Atom.Feed{..}) = undefined
   processFeed feed@(entityVal -> Feed feedUrl _ lastDate) (RSSFeed RSS.RSS{rssChannel=RSS.RSSChannel{..}}) = do
     case lastDate of
       Just date -> do
         users <- loadUsersByFeed feed
         let newItems = filter (\item -> (rssDate item) > (fromJust lastDate)) rssItems
         sendMessages users newItems
         when (not $ null newItems) $
           updateLastDate feed (last . sort $ map rssDate newItems)
       Nothing -> when (not $ null rssItems) $
         updateLastDate feed (last . sort $ map rssDate rssItems)

   sendMessages users rssItems = do
     forM_ rssItems $ \RSS.RSSItem{..} -> do
       let title = fromJust rssItemTitle
       let url = fromJust rssItemLink
       let message = title <> "\n\n" <> url
       forM_ users $ \(entityVal -> User userId) ->
         call $ sendMessage (read userId) message

   rssGuid = RSS.rssGuidValue . fromJust . RSS.rssItemGuid
   rssDate x = do
     let pubDate = fromJust $ RSS.rssItemPubDate x
     zonedTimeToUTC $ parseTimeOrError True defaultTimeLocale rfc822DateFormat (T.unpack pubDate)

   call action = do
     config <- asks fTelegramConfig
     void $ liftIO $ TG.runClient action (tgToken config) (tgManager config)
