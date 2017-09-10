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
import Database.Persist.Sqlite
import Database.Persist.TH

initDb :: (MonadBaseControl IO m, MonadIO m) => m (Pool SqlBackend)
initDb = runStderrLoggingT $ do
  pool <- createSqlitePool "feeder.db" 5
  runSqlPool (runMigration migrateAll) pool
  return pool

updateLastGuid :: (MonadReader FeederConfig m, MonadBaseControl IO m, MonadIO m)
               => Entity Feed -> Text -> m ()
updateLastGuid feed guid = do
  pool <- asks fPool
  flip runSqlPool pool $ update (entityKey feed) [FeedLastGuid =. (Just guid)]
  return ()

loadFeeds :: (MonadReader FeederConfig m, MonadBaseControl IO m, MonadIO m) => m ([Entity Feed])
loadFeeds = do
  pool <- asks fPool
  feeds :: [Entity Feed] <- flip runSqlPool pool $ selectList [] []
  return feeds

loadFeedsByUser :: (MonadReader FeederConfig m, MonadBaseControl IO m, MonadIO m)
               => String -> m ([Entity Feed])
loadFeedsByUser userId = do
  pool <- asks fPool
  feeds :: [Entity Feed] <- flip runSqlPool pool $ do
    user <- selectFirst [UserUserId ==. userId] []
    case user of
      Just u -> feedsByUser u
      Nothing -> return []
  return feeds
 where
  feedsByUser user = do
    subs <- selectList [SubscriptionUserId ==. (entityKey user)] []
    let subIds = map (\(Subscription _ feedId) -> feedId) $ map entityVal subs
    selectList [FeedId <-. subIds] []

loadUsersByFeed :: (MonadReader FeederConfig m, MonadBaseControl IO m, MonadIO m)
               => Entity Feed -> m ([Entity User])
loadUsersByFeed feed = do
  pool <- asks fPool
  users :: [Entity User] <- flip runSqlPool pool $ usersByFeed feed
  return users
 where
   usersByFeed feed = do
     subs <- selectList [SubscriptionFeedId ==. (entityKey feed)] []
     let subIds = map (\(Subscription userId _) -> userId) $ map entityVal subs
     selectList [UserId <-. subIds] []

addSubscription :: (MonadReader FeederConfig m, MonadBaseControl IO m, MonadIO m)
                => String -> Text -> m (Key Subscription)
addSubscription userId url = do
  pool <- asks fPool
  (user,feed) <- flip runSqlPool pool $ do
    user <- selectFirst [UserUserId ==. userId] []
    feed <- selectFirst [FeedUrl ==. url] []
    return (user, feed)

  subscribe userId url user feed
 where
   subscribe _ _ (Just user) (Just feed) = do
     pool <- asks fPool
     flip runSqlPool pool $ insert $ Subscription (entityKey user) (entityKey feed)

   subscribe userId url Nothing Nothing = do
     pool <- asks fPool
     flip runSqlPool pool $ do
       user <- insert $ User userId
       feed <- insert $ Feed url Nothing
       insert $ Subscription  user feed

   subscribe userId url (Just user) Nothing = do
     pool <- asks fPool
     flip runSqlPool pool $ do
       feed <- insert $ Feed url Nothing
       insert $ Subscription (entityKey user) feed

   subscribe userId url Nothing (Just feed) = do
     pool <- asks fPool
     flip runSqlPool pool $ do
       user <- insert $ User userId
       insert $ Subscription user (entityKey feed)

removeSubscriptionsByUser :: (MonadReader FeederConfig m, MonadBaseControl IO m, MonadIO m)
                    => String -> m ()
removeSubscriptionsByUser userId = do
  pool <- asks fPool
  flip runSqlPool pool $ do
    user <- selectFirst [UserUserId ==. userId] []
    when (isJust user) $
      deleteWhere [SubscriptionUserId ==. (entityKey $ fromJust user)]

removeSubscription :: (MonadReader FeederConfig m, MonadBaseControl IO m, MonadIO m)
                    => String -> Text -> m ()
removeSubscription userId url = do
  pool <- asks fPool
  flip runSqlPool pool $ do
    user <- selectFirst [UserUserId ==. userId] []
    feed <- selectFirst [FeedUrl ==. url] []
    when (isJust user && isJust feed) $
      deleteWhere [ SubscriptionUserId ==. (entityKey $ fromJust user)
                  , SubscriptionFeedId ==. (entityKey $ fromJust feed)
                  ]

removeFeed :: (MonadReader FeederConfig m, MonadBaseControl IO m, MonadIO m)
           => Entity Feed -> m ()
removeFeed feed = do
  pool <- asks fPool
  flip runSqlPool pool $ do
    deleteWhere [FeedId ==. (entityKey feed)]
    deleteWhere [SubscriptionFeedId ==. (entityKey feed)]
