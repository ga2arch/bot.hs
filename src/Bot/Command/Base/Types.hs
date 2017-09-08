{-# LANGUAGE DeriveFunctor #-}
module Bot.Command.Base.Types where

import Data.Text

data Base next  = Send Text next
               | Prompt Text (Text -> next)
  deriving (Functor)
