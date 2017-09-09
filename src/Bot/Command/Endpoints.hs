{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Bot.Command.Endpoints where

import           Bot.Command
import           Bot.Command.Base
import           Bot.Command.Base.Types
import           Bot.Command.Feeder
import           Bot.Command.Feeder.Types
import           Bot.Command.Types
import           Bot.Types
import           Control.Applicative
import           Control.Concurrent.STM.TChan
import           Control.Monad.Free
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Proxy
import           Data.Text (Text)
import           GHC.TypeLits
import           Text.Read

import qualified Data.Text as T

data a :<|> b = a :<|> b
infixr 8 :<|>

data (a :: k) :> (b :: *)
infixr 9 :>

data Capture (a :: *)
data Run a

class HasServer api where
  type Server api :: *
  route :: Proxy api -> Server api -> T.Text -> Maybe (UserMonad ())

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type Server (a :<|> b) = Server a :<|> Server b
  route Proxy (handlera :<|> handlerb) text = route pa handlera text <|> route pb handlerb text
    where
      pa = Proxy :: Proxy a
      pb = Proxy :: Proxy b

instance (Functor f, Eval UserMonad f) => HasServer (Run f) where
  type Server (Run f) = Free f ()
  route Proxy handler msg = Just $ iterM runAlgebra handler

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  type Server (s :> r) = Server r
  route Proxy handler text = do
    let prefix = T.pack $ symbolVal (Proxy :: Proxy s)
    if prefix `T.isPrefixOf` text
      then route (Proxy :: Proxy r) handler $ T.drop 1 $ fromJust $ T.stripPrefix prefix text
      else Nothing

instance (Read a, HasServer r) => HasServer (Capture a :> r) where
  type Server (Capture a :> r) = a -> Server r
  route Proxy handler text = do
    a <- readMaybe $ T.unpack text
    route (Proxy :: Proxy r) (handler a) text

instance {-# OVERLAPPING #-} (HasServer r) => HasServer (Capture T.Text :> r) where
  type Server (Capture T.Text :> r) = T.Text -> Server r
  route Proxy handler text | T.length text > 0 = route (Proxy :: Proxy r) (handler text) text
  route _ _       _        = Nothing

serve :: (HasServer layout) => Proxy layout -> Server layout -> T.Text -> UserMonad ()
serve p h xs = case route p h xs of
  Just m  -> m
  Nothing -> return ()

-- * Endpoints
type Commands =
  "/subscribe" :> Capture T.Text :> Run (Base :+: Feeder)
  :<|>  "/unsubscribe" :> Capture T.Text :> Run (Base :+: Feeder)
  :<|> "/list" :> Run (Base :+: Feeder)

handleCommands :: (?feederChan :: TChan FeederEvent) => Server Commands
handleCommands = subscribeCommand :<|> unsubscribeCommand :<|> listCommand
