{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
module Bot.Command.Telegram.Types where

import Bot.Command.Route
import Data.Text
import Data.Proxy
import Control.Monad.Free
import           GHC.TypeLits
import qualified Web.Telegram.API.Bot.Requests as TG

data InlineKeyboardButton = InlineKeyboardButton
  {ikbLabel :: Text
  ,ikbCommand :: Proxy (Symbol :> Capture Text :> Free Telegram ()) }

data Telegram next  = SendKeyboard Text [[InlineKeyboardButton]] next
  deriving (Functor)
