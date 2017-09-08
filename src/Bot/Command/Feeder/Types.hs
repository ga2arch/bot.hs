{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bot.Command.Feeder.Types where

import Bot.Types
import Bot.Command.Types

import Data.Int
import Control.Concurrent.STM.TChan

import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T

data FeederEvent = News String | SubscribeUrl Int64 T.Text

data Feeder next = Subscribe (TChan FeederEvent) T.Text next
                 | Validate T.Text (Bool -> next)
  deriving (Functor)
