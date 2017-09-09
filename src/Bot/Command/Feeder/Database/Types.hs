{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GADTs                      #-}
module Bot.Command.Feeder.Database.Types where

import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

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
    feedId FeedId
    deriving Show
|]
