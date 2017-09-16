{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Bot.Command.Feeder.Database where

import Bot.Command.Feeder.Types
import Bot.Command.Feeder.Database.Types
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Maybe
import Data.Pool
import Data.Text (Text)
import Data.Time.Clock
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

run f = do
  pool <- asks fPool
  liftSqlPersistMPool f pool

initDb :: (MonadBaseControl IO m, MonadIO m) => m (Pool SqlBackend)
initDb = runStderrLoggingT $ do
  pool <- createPostgresqlPool "postgresql://localhost/feeder?user=ga2bot" 5
  runSqlPool (runMigration migrateAll) pool
  return pool

updateLastDate :: (MonadReader FeederConfig m, MonadBaseControl IO m, MonadIO m)
               => Entity Feed -> UTCTime -> m ()
updateLastDate feed date = do
  run $ update (entityKey feed) [FeedLastDate =. (Just date)]
  return ()

loadFeeds :: (MonadReader FeederConfig m, MonadBaseControl IO m, MonadIO m) => m ([Entity Feed])
loadFeeds = do
  feeds :: [Entity Feed] <- run $ selectList [] []
  return feeds

loadFeedsByUser :: (MonadReader FeederConfig m, MonadBaseControl IO m, MonadIO m)
               => String -> m ([Entity Feed])
loadFeedsByUser userId =
  run $ do
    user <- selectFirst [UserUserId ==. userId] []
    case user of
      Just u -> feedsByUser u
      Nothing -> return []
 where
  feedsByUser user = do
    subs <- selectList [SubscriptionUserId ==. (entityKey user)] []
    let subIds = map (\(Subscription _ feedId) -> feedId) $ map entityVal subs
    selectList [FeedId <-. subIds] []

loadUsersByFeed :: (MonadReader FeederConfig m, MonadBaseControl IO m, MonadIO m)
               => Entity Feed -> m ([Entity User])
loadUsersByFeed feed =
  run $ usersByFeed feed
 where
   usersByFeed feed = do
     subs <- selectList [SubscriptionFeedId ==. (entityKey feed)] []
     let subIds = map (\(Subscription userId _) -> userId) $ map entityVal subs
     selectList [UserId <-. subIds] []

addSubscription :: (MonadReader FeederConfig m, MonadBaseControl IO m, MonadIO m)
                => String -> Text -> m (Key Subscription)
addSubscription userId url =
  run $ do
    user <- selectFirst [UserUserId ==. userId] []
    feed <- selectFirst [FeedUrl ==. url] []
    subscribe userId url (entityKey <$> user) (entityKey <$> feed)
 where
   subscribe _ _ (Just user) (Just feed) =
     insert $ Subscription user feed

   subscribe userId url Nothing Nothing = do
     user <- insert $ User userId
     feed <- insert $ Feed url Nothing
     insert $ Subscription  user feed

   subscribe userId url (Just user) Nothing = do
     feed <- insert $ Feed url Nothing
     insert $ Subscription user feed

   subscribe userId url Nothing (Just feed) = do
     user <- insert $ User userId
     insert $ Subscription user feed

removeSubscriptionsByUser :: (MonadReader FeederConfig m, MonadBaseControl IO m, MonadIO m)
                    => String -> m ()
removeSubscriptionsByUser userId =
  run $ do
    user <- selectFirst [UserUserId ==. userId] []
    when (isJust user) $
      deleteWhere [SubscriptionUserId ==. (entityKey $ fromJust user)]

removeSubscription :: (MonadReader FeederConfig m, MonadBaseControl IO m, MonadIO m)
                    => String -> Text -> m ()
removeSubscription userId url =
  run $ do
    user <- selectFirst [UserUserId ==. userId] []
    feed <- selectFirst [FeedUrl ==. url] []
    when (isJust user && isJust feed) $ do
      deleteWhere [ SubscriptionUserId ==. (entityKey $ fromJust user)
                  , SubscriptionFeedId ==. (entityKey $ fromJust feed)
                  ]
      subs <- selectList [SubscriptionFeedId ==. (entityKey $ fromJust feed)] []
      when (null subs) $ deleteWhere [FeedId ==. (entityKey $ fromJust feed)]

removeFeed :: (MonadReader FeederConfig m, MonadBaseControl IO m, MonadIO m)
           => Entity Feed -> m ()
removeFeed feed =
  run $ do
    deleteWhere [SubscriptionFeedId ==. (entityKey feed)]
    deleteWhere [FeedId ==. (entityKey feed)]
