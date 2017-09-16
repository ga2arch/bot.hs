{-# LANGUAGE DeriveFunctor #-}
module Bot.Command.Base.Types where

import Data.Text

data Base next  = Send Text next
                | Prompt Text (Text -> next)
                | UploadAudio Text FilePath next
                | PushNamespace Text next
                | PopNamespace Text next
  deriving (Functor)
