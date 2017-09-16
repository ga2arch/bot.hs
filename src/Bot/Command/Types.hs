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
import           Data.Monoid
import           Data.Proxy
import           Data.Text (Text)
import           GHC.TypeLits
import           Text.Read

import qualified Data.Text as T
import qualified Text.RE.PCRE.Text as T

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

data Match (regex :: Symbol)
data Capture (a :: *) (tag :: Symbol)
data Run a (desc :: Symbol)

class HasServer api where
  type Server api :: *
  route :: Proxy api -> Server api -> T.Text -> Maybe (UserMonad ())

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type Server (a :<|> b) = Server a :<|> Server b
  route Proxy (handlera :<|> handlerb) text = route pa handlera text <|> route pb handlerb text
    where
      pa = Proxy :: Proxy a
      pb = Proxy :: Proxy b

instance (Functor f, Eval UserMonad f) => HasServer (Run f (desc :: Symbol)) where
  type Server (Run f b) = Free f ()
  route Proxy handler text = Just $ iterM runAlgebra handler

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  type Server (s :> r) = Server r
  route Proxy handler text = do
    let prefix = T.pack $ symbolVal (Proxy :: Proxy s)
    case T.stripPrefix prefix text of
      Just rest ->
        if ((not $ T.null rest) && T.head rest == '/')
          then route (Proxy :: Proxy r) handler rest
          else route (Proxy :: Proxy r) handler (T.drop 1 rest)
      Nothing -> Nothing

instance {-# OVERLAPPING #-} (KnownSymbol s, KnownSymbol desc, Eval UserMonad f)
  => HasServer (Match (s :: Symbol) :> (Run f (desc :: Symbol))) where
  type Server (Match s :> Run f desc) = T.Match T.Text -> Free f ()
  route Proxy handler text = do
    regex <- T.compileRegex $ symbolVal (Proxy :: Proxy s)
    let match = text T.?=~ regex
    if T.matched match
      then Just $ iterM runAlgebra (handler match)
      else Nothing

instance (Read a, KnownSymbol tag, HasServer r) => HasServer (Capture a (tag :: Symbol) :> r) where
  type Server (Capture a tag :> r) = a -> Server r
  route Proxy handler text = do
    a <- readMaybe $ T.unpack text
    route (Proxy :: Proxy r) (handler a) text

instance {-# OVERLAPPING #-} (HasServer r, KnownSymbol tag) => HasServer (Capture T.Text (tag :: Symbol) :> r) where
  type Server (Capture T.Text tag :> r) = T.Text -> Server r
  route Proxy handler text | T.length text > 0 =
    route (Proxy :: Proxy r) (handler text) (fst $ T.breakOn " " text)
  route _ _       _        = Nothing

-- * Help
class HasHelp api where
  help :: Proxy api -> T.Text -> T.Text

instance (HasHelp a, HasHelp b) => HasHelp (a :<|> b) where
  help Proxy acc = help (Proxy :: Proxy a) acc <> help (Proxy :: Proxy b) acc

instance {-# OVERLAPPING #-} (KnownSymbol s, HasHelp a, HasHelp b) => HasHelp ((s :: Symbol) :> (a :<|> b)) where
  help Proxy acc = acc

instance (Functor f, KnownSymbol desc, Eval UserMonad f) => HasHelp (Run f (desc :: Symbol)) where
  help Proxy acc = acc <> " " <> (T.pack $ symbolVal (Proxy :: Proxy desc)) <> "\n"

instance (Functor f, KnownSymbol s, KnownSymbol desc, Eval UserMonad f)
  => HasHelp (Match (s::Symbol) :> (Run f (desc :: Symbol))) where
  help Proxy acc = ""

instance (KnownSymbol s, HasHelp r) => HasHelp ((s :: Symbol) :> r) where
  help Proxy acc =
    help (Proxy :: Proxy r) (acc <> (T.pack $ symbolVal (Proxy :: Proxy s)))

instance (Read a, KnownSymbol tag, HasHelp r) => HasHelp (Capture a (tag :: Symbol) :> r) where
  help Proxy acc =
    help (Proxy :: Proxy r) (acc <> " <" <> (T.pack $ symbolVal (Proxy :: Proxy tag)) <> ">")

serve :: (HasServer layout) => Proxy layout -> Server layout -> T.Text -> UserMonad ()
serve p h xs = case route p h xs of
  Just m  -> m
  Nothing -> return ()
