{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
module Bot.Command where

import Bot.Command.Base
import Bot.Command.Base.Types
import Bot.Command.Feeder
import Bot.Command.Feeder.Types
import Bot.Command.Types
import Control.Concurrent.STM.TChan
import Data.Text
import Data.Proxy

type Commands =
  "/subscribe" :> Capture Text "url" :> Run (Base :+: Feeder) "subscribe to the url"
  :<|> "/unsubscribe" :> Capture Text "url" :> Run (Base :+: Feeder) "unsubscribe from the url"
  :<|> "/list" :> Run (Base :+: Feeder) "list all subscriptions"
  :<|> "/help" :> Run (Base) "show help"

handleCommands :: (?feederChan :: TChan FeederEvent) => Server Commands
handleCommands = subscribeCommand
  :<|> unsubscribeCommand
  :<|> listCommand
  :<|> helpCommand
 where
  helpText = help (Proxy :: Proxy Commands) empty
  helpCommand = send helpText
