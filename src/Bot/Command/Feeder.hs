{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
module Bot.Command.Feeder where

import Bot.Types
import Bot.Command.Types
import Bot.Command.Feeder.Types
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
       R.sadd ("feed:" <> url') [C.pack $ show chatId]

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
     let firstGuid = rssGuid $ head rssItems
     oldFirstGuid <- R.runRedis conn $ R.get ("feed:" <> url <> ":last")
     case oldFirstGuid of
       Right (Just guid) -> do
         sendMessages conn config url $
           takeWhile (\item -> (rssGuid item) /= guid) rssItems
         updateLastSeen conn url rssItems
       Right Nothing  -> updateLastSeen conn url rssItems
       Left x -> return ()

   sendMessages conn config url rssItems = do
     (Right users) <- R.runRedis conn $ R.smembers ("feed:" <> url)
     forM_ rssItems $ \RSS.RSSItem{..} -> do
       let title = T.pack $ fromJust rssItemTitle
       let url = T.pack $ fromJust rssItemLink
       let message = title <> "\n\n" <> url
       forM_ users $ \user ->
         call config $ sendMessage (read $ C.unpack user) message

   updateLastSeen conn url rssItems = do
     R.runRedis conn $ do
       let firstGuid = rssGuid $ head rssItems
       R.set ("feed:" <> url <> ":last") firstGuid
       return ()

   rssGuid = C.pack . RSS.rssGuidValue . fromJust . RSS.rssItemGuid

   call config action = liftIO $ TG.runClient action (tgToken config) (tgManager config)
