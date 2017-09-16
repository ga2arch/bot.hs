{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Bot.Command where

import Bot.Command.Base
import Bot.Command.Base.Types
import Bot.Command.Feeder
import Bot.Command.Feeder.Types
import Bot.Command.Youtube
import Bot.Command.Youtube.Types
import Bot.Command.Types
import Control.Concurrent.STM.TChan
import Data.Array (Array, (!))
import Data.Text (Text, empty, pack)
import Data.Proxy
import qualified Text.RE.PCRE as R
import qualified Text.RE.Replace as R
import qualified Text.RE.PCRE.Text as R
import Text.Regex.Base.RegexLike (MatchText, getAllTextMatches)

type Commands =
  "/start" :> Run (Base) "welcome"
  :<|> "/subscribe" :> Capture Text "url" :> Run (Base :+: Feeder) "subscribe to the feed"
  :<|> "/unsubscribe" :> Capture Text "url" :> Run (Base :+: Feeder) "unsubscribe from the feed"
  :<|> "/list" :> Run (Base :+: Feeder) "list all feed subscriptions"
  :<|> "/help" :> Run (Base) "show help"
  :<|> Match "(?:https?://)?(?:www\\.)?youtu(?:be\\.com/watch\\?(?:.*?&(?:amp;)?)?v=|\\.be/)([\\w\\-]+)(?:&(?:amp;)?[\\w\\?=]*)?"
  :> Run (Base :+: Youtube) ""

handleCommands :: (?feederChan :: TChan FeederEvent) => Server Commands
handleCommands = startCommand
  :<|> subscribeCommand
  :<|> unsubscribeCommand
  :<|> listCommand
  :<|> helpCommand
  :<|> youtubeCommand
 where
  helpText = help (Proxy :: Proxy Commands) empty
  helpCommand = send helpText
  startCommand = send "hola\nga2bot allows you to subscribe to feed rss and receive news\n\
                      \/help to see the list of available commands"
