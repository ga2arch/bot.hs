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
{-# LANGUAGE OverloadedStrings  #-}
module Bot.Command.Types where

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

data (f :+: g) e = Inl (f e) | Inr (g e)
infixr 7 :+:

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl e) = Inl (fmap f e)
  fmap f (Inr e) = Inr (fmap f e)

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f => f :<: f where
  inj = id

instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj

-- * Eval
class (Functor f, Monad m, MonadReader UserConfig m) => Eval m f where
  runAlgebra :: f (m a) -> m a

instance (Monad m, Eval m f, Eval m g) => Eval m (f :+: g) where
  runAlgebra (Inl r) = runAlgebra r
  runAlgebra (Inr r) = runAlgebra r

-- * Servant like
data a :<|> b = a :<|> b
infixr 8 :<|>

data (a :: k) :> (b :: *)
infixr 9 :>

data Capture (a :: *)
data Run a

class HasServer api where
  type Server api :: *
  route :: Proxy api -> Server api -> T.Text -> Maybe (UserMonad ())
--  help :: Proxy api -> Server api -> [T.Text]

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type Server (a :<|> b) = Server a :<|> Server b
  route Proxy (handlera :<|> handlerb) text = route pa handlera text <|> route pb handlerb text
    where
      pa = Proxy :: Proxy a
      pb = Proxy :: Proxy b
--  help Proxy (handlera :<|> handlerb) = help (Proxy :: Proxy a) handlera
--    <|> help (Proxy :: Proxy b) handlerb

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
  route Proxy handler text | T.length text > 0 =
    route (Proxy :: Proxy r) (handler text) (fst $ T.breakOn " " text)
  route _ _       _        = Nothing

serve :: (HasServer layout) => Proxy layout -> Server layout -> T.Text -> UserMonad ()
serve p h xs = case route p h xs of
  Just m  -> m
  Nothing -> return ()
