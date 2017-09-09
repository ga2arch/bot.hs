{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Bot.Command.Feeder.Database where

import Bot.Command.Feeder.Types

import           Control.Monad.IO.Class  (liftIO)
import Control.Monad.Reader
import Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

import Data.Text (Text)
import Data.Time.Clock
import Data.Pool
import Data.Maybe
import Control.Monad.Logger
import Control.Monad.Trans.Control

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    userId String
    deriving Show

Feed
    url Text
    lastGuid Text Maybe
    deriving Show

Subscription
    userId UserId
    feedId   FeedId
    deriving Show
|]

initDb :: (MonadBaseControl IO m, MonadIO m) => m (Pool SqlBackend)
initDb = runStderrLoggingT $ do
  pool <- createSqlitePool "feeder.db" 5
  runSqlPool (runMigration migrateAll) pool
  return pool

updateLastGuid :: (MonadReader FeederConfig m, MonadBaseControl IO m, MonadIO m) => Entity Feed -> Text -> m ()
updateLastGuid feed guid = do
  pool <- asks fPool
  flip runSqlPool pool $ update (entityKey feed) [FeedLastGuid =. (Just guid)]
  return ()

getFeeds :: (MonadReader FeederConfig m, MonadBaseControl IO m, MonadIO m) => m ([Entity Feed])
getFeeds = do
  pool <- asks fPool
  feeds :: [Entity Feed] <- flip runSqlPool pool $ selectList [] []
  return feeds

getUsersByFeed :: (MonadReader FeederConfig m, MonadBaseControl IO m, MonadIO m)
  => Entity Feed -> m ([Entity User])
getUsersByFeed feed = do
  pool <- asks fPool
  users :: [Entity User] <- flip runSqlPool pool $ usersByFeed feed
  return users
 where
   usersByFeed feed = do
     subs <- selectList [SubscriptionFeedId ==. (entityKey feed)] []
     let subUserIds = map (\(Subscription _ userId) -> show userId) $ map entityVal subs
     selectList [UserUserId <-. subUserIds] []

addSubscription :: (MonadReader FeederConfig m, MonadBaseControl IO m, MonadIO m) => String -> Text -> m (Key Subscription)
addSubscription userId url = do
  pool <- asks fPool
  user <- flip runSqlPool pool $ selectFirst [UserUserId ==. userId] []
  feed <- flip runSqlPool pool $ selectFirst [FeedUrl ==. url] []
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
