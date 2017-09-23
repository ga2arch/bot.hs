{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Bot.Command.Feeder where

import           Bot.Channel
import           Bot.Channel.Telegram
import           Bot.Command.Base
import           Bot.Command.Base.Types
import           Bot.Command.Feeder.Database
import           Bot.Command.Feeder.Database.Types
import           Bot.Command.Feeder.Types
import           Bot.Command.Types
import           Bot.Types
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Catch
import           Control.Monad.Free
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import           Data.Time.LocalTime
import           Data.Time.RFC3339
import           Data.Time.RFC822
import           Database.Persist (entityVal, Entity)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Header
import           Network.URI
import           System.Log.FastLogger
import           Text.Feed.Import
import           Text.Feed.Types

import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Text.Atom.Feed as Atom
import qualified Text.RSS.Syntax as RSS

subscribe :: (Functor f, MonadFree f m, Feeder :<: f)=> Text -> m ()
subscribe url = liftF . inj $ Subscribe url ()

unsubscribe :: (Functor f, MonadFree f m, Feeder :<: f) => Text -> m ()
unsubscribe url = liftF . inj $ Unsubscribe url ()

validate :: (Functor f, MonadFree f m, Feeder :<: f) => Text -> m Bool
validate url = liftF . inj $ Validate url id

getFeeds :: (Functor f, MonadFree f m, Feeder :<: f) => m ([Entity Bot.Command.Feeder.Database.Types.Feed])
getFeeds = liftF . inj $ GetFeeds id

instance Eval UserMonad Feeder where
  runAlgebra (Subscribe url next) = do
    userId <- asks userChatId
    dispatcher <- asks userDispacher
    liftIO $ dispatcher (SubscribeUrl userId url)
    next

  runAlgebra (Unsubscribe url next) = do
    userId <- asks userChatId
    dispatcher <- asks userDispacher
    liftIO $ dispatcher (UnsubscribeUrl userId url)
    next

  runAlgebra (GetFeeds next) = do
    chan <- liftIO newTChanIO
    userId <- asks userChatId
    dispatcher <- asks userDispacher
    liftIO $ dispatcher (ListFeeds (show userId) chan)
    (Feeds feeds) <- liftIO . atomically $ readTChan chan
    next feeds

  runAlgebra (Validate url next) = do
    liftIO $ print url
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

subscribeCommand :: T.Text -> Free (Base :+: Feeder) ()
subscribeCommand url = do
  valid <- validate url
  if valid
    then do
      subscribe url
      send $ "subscribed to: " <> url
    else do
      send "invalid url"

listCommand :: Free (Base :+: Feeder) ()
listCommand = do
  feeds <- getFeeds
  if (null feeds)
    then send "You don't have any subscription"
    else do
      let urls = map (\(entityVal -> (Feed url _)) -> url) feeds
      send $ "You are subscribed to: \n" <> T.intercalate "\n" urls

unsubscribeCommand :: T.Text -> Free (Base :+: Feeder) ()
unsubscribeCommand url = do
  unsubscribe url
  send "unsubscribed"

feeder :: TChan ChannelCmd -> IO (TChan FeederEvent)
feeder cmdChan = do
  pool <- initDb
  feederChan <- newTChanIO
  logOut <- newStdoutLoggerSet defaultBufSize
  let feederConfig = FeederConfig pool cmdChan logOut

  void . liftIO . forkIO . forever $ do
    event <- atomically $ readTChan feederChan
    runReaderT (runFeeder (case event of
      SubscribeUrl userId url -> onSubscribe userId url
      ListFeeds userId chan -> onListFeeds userId chan
      UnsubscribeUrl userId url ->  onUnsubscribe userId url
      _ -> return ())) feederConfig

  void . liftIO . forkIO . forever $ do
    runReaderT (runFeeder checkFeeds) feederConfig
    threadDelay 60000000

  return feederChan
  where
   onSubscribe userId url = void $ addSubscription (show userId) url
   onUnsubscribe userId url = void $ removeSubscription (show userId) url
   onListFeeds userId chan = do
     feeds <- loadFeedsByUser userId
     void . liftIO . atomically $ writeTChan chan (Feeds feeds)

   checkFeeds = do
     $(logDebug) "checking feed"
     feeds <- loadFeeds
     forM_ feeds $ \feed ->
                    catchAll
                    (loadFeed feed)
                    (\ex -> onFeedError ex feed)

   onFeedError ex feed@(entityVal -> Feed feedUrl _) = do
     liftIO $ print ex
     users <- loadUsersByFeed feed
     forM_ users $ \(entityVal -> User userId) ->
         sendMessage (read userId)
                     ("error processing feed: " <> feedUrl <> " unsubscribing")
     removeFeed feed

   loadFeed feed@(entityVal -> (Feed feedUrl _)) = do
     manager <- liftIO $ newManager tlsManagerSettings
     liftIO $ print $ "loading " ++ (T.unpack feedUrl)
     request <- liftIO $ parseRequest $ T.unpack feedUrl
     response <- liftIO $ httpLbs request manager
     case parseFeedSource $ responseBody response of
       Just content -> processFeed feed content
       Nothing -> return ()

   processFeed feed@(entityVal -> Feed _ lastDate) (AtomFeed Atom.Feed{..}) = do
     case lastDate of
       Just date -> do
         users <- loadUsersByFeed feed
         let newItems = filter (\item -> (atomDate item) > date) feedEntries
         sendMessages users $
           map (\item -> (T.pack $ Atom.txtToString $ Atom.entryTitle item,
                          Atom.linkHref $ head $ Atom.entryLinks item)) newItems
         when (not $ null newItems) $
           updateLastDate feed $ from3339Date feedUpdated
       Nothing -> when (not $ null feedEntries) $
         updateLastDate feed $ from3339Date feedUpdated

   processFeed feed@(entityVal -> Feed _ lastDate) (RSSFeed RSS.RSS{rssChannel=RSS.RSSChannel{..}}) = do
     case lastDate of
       Just date -> do
         users <- loadUsersByFeed feed
         let newItems = filter (\item -> (rssDate item) > date) rssItems
         sendMessages users $
           map (\item -> (fromJust $ RSS.rssItemTitle item, fromJust $ RSS.rssItemLink item)) newItems
         when (not $ null newItems) $
           updateLastDate feed (last . sort $ map rssDate newItems)
       Nothing -> when (not $ null rssItems) $
         updateLastDate feed (last . sort $ map rssDate rssItems)

   processFeed _ _ = return ()

   sendMessages users rssItems = do
     forM_ rssItems $ \(title, url) -> do
       let message = title <> "\n\n" <> url
       forM_ users $ \(entityVal -> User userId) ->
         sendMessage (read userId) message

   rssDate x = do
     let pubDate = fromJust $ RSS.rssItemPubDate x
     from822Date pubDate

   from822Date date = zonedTimeToUTC $ fromJust $ parseTimeRFC822 date
   from3339Date date = zonedTimeToUTC $ fromJust $ parseTimeRFC3339 date

   atomDate x = do
     let pubDate = Atom.entryUpdated x
     from3339Date pubDate

   sendMessage userId message = do
     chan <- asks fCmdChan
     liftIO $ sendMessage' userId message chan
