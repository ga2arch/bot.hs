{-# LANGUAGE DeriveFunctor #-}
module Bot.Command.Youtube.Types where

import Data.Text

data Youtube next  = Download Text (Either Text FilePath -> next)
                   | GetTitle Text (Either Text Text -> next)
                   | Cleanup FilePath next
  deriving (Functor)
