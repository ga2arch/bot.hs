module Bot.Channel where

import Bot.Channel.Types
import Bot.Channel.Telegram.Types

type ChannelCmd = Expr TelegramMessage
