{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
module Bot.Command where

import Bot.Command.Base.Types
import Bot.Command.Feeder
import Bot.Command.Feeder.Types
import Bot.Command.Types
import Control.Concurrent.STM.TChan
import Data.Text

type Commands =
  "/subscribe" :> Capture Text "url" :> Run (Base :+: Feeder) "subscribe to the url"
  :<|>  "/unsubscribe" :> Capture Text "url" :> Run (Base :+: Feeder) "unsubscribe from the url"
  :<|> "/list" :> Run (Base :+: Feeder) "lists all subscriptions"

handleCommands :: (?feederChan :: TChan FeederEvent) => Server Commands
handleCommands = subscribeCommand :<|> unsubscribeCommand :<|> listCommand
