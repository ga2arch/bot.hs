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
  "/subscribe" :> Capture Text :> Run (Base :+: Feeder)
  :<|>  "/unsubscribe" :> Capture Text :> Run (Base :+: Feeder)
  :<|> "/list" :> Run (Base :+: Feeder)

handleCommands :: (?feederChan :: TChan FeederEvent) => Server Commands
handleCommands = subscribeCommand :<|> unsubscribeCommand :<|> listCommand
