module Bot.Channel where

import Data.Text
import Bot.Channel.Types
import Bot.Channel.Telegram

type ChannelCmd = Expr TelegramMessage
