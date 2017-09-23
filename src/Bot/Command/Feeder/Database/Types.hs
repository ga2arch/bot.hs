{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GADTs                      #-}
module Bot.Command.Feeder.Database.Types where

import Data.Text (Text)
import Database.Persist.Postgresql
import Database.Persist.TH
import Data.Time.Clock

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    userId String
    deriving Show

Feed
    url Text
    lastDate UTCTime Maybe
    deriving Show

Subscription
    userId UserId
    feedId FeedId
    deriving Show
|]
