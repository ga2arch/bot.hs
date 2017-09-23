{-# LANGUAGE UndecidableInstances #-}
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
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns  #-}
module Bot.Command.Route where

import           Bot.Command.Base
import           Bot.Command.Base.Types (Base)
import           Bot.Command.Types
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

-- * Servant like
data a :<|> b = a :<|> b
infixr 8 :<|>

data (a :: k) :> (b :: *)
infixr 9 :>

data NS (namespace :: Symbol) (desc :: Symbol)
data Match (regex :: Symbol)
data Capture (a :: *) (tag :: Symbol)
data Run a (desc :: Symbol)

data Action = PushNamespace T.Text | PopNamespace T.Text
run handler f = return $ iterM runAlgebra (f >> handler)

class HasServer api where
  type Server api :: *
  route :: Proxy api -> Server api -> Maybe Action -> T.Text -> T.Text -> Maybe (UserMonad ())

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type Server (a :<|> b) = Server a :<|> Server b
  route Proxy (handlera :<|> handlerb) action ns text =
    route pa handlera action ns text
    <|> route pb handlerb action ns text
    where
      pa = Proxy :: Proxy a
      pb = Proxy :: Proxy b

instance (Functor f, Eval UserMonad f, Base :<: f) => HasServer (Run f (desc :: Symbol)) where
  type Server (Run f b) = Free f ()
  route Proxy handler (Just (PushNamespace name)) ns text = run handler (pushNamespace name)
  route Proxy handler (Just (PopNamespace  name)) ns text = run handler (popNamespace name)
  route Proxy handler Nothing ns text = return $ iterM runAlgebra handler

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  type Server (s :> r) = Server r
  route Proxy handler action ns text = do
    let prefix = T.pack $ symbolVal (Proxy :: Proxy s)
    rest <- T.stripPrefix prefix text
    route (Proxy :: Proxy r) handler action ns (T.drop 1 rest)

instance (KnownSymbol s, KnownSymbol desc, HasServer r)
  => HasServer (NS (s :: Symbol) (desc :: Symbol) :> r) where
  type Server (NS s desc :> r) = Server r
  route Proxy handler action ns text = do
    let prefix = T.pack $ symbolVal (Proxy :: Proxy s)
    case T.stripPrefix prefix text of
      Just rest@(T.uncons -> Just ('/', "exit")) ->
        route (Proxy :: Proxy r) handler (Just $ PopNamespace prefix) ns rest
      Just rest@(T.uncons -> Just ('/', _)) ->
        return $ maybe (return ()) id $ route (Proxy :: Proxy r) handler action ns rest
      Just _ ->
        route (Proxy :: Proxy r) handler (Just $ PushNamespace prefix) ns "/start"
      Nothing | prefix == ns -> return $ return ()
      Nothing -> Nothing

instance {-# OVERLAPPING #-} (KnownSymbol s, KnownSymbol desc, Eval UserMonad f)
  => HasServer (Match (s :: Symbol) :> (Run f (desc :: Symbol))) where
  type Server (Match s :> Run f desc) = T.Match T.Text -> Free f ()
  route Proxy handler action ns text = do
    regex <- T.compileRegex $ symbolVal (Proxy :: Proxy s)
    let match = text T.?=~ regex
    if T.matched match
      then return $ iterM runAlgebra (handler match)
      else Nothing

instance (Read a, KnownSymbol tag, HasServer r) => HasServer (Capture a (tag :: Symbol) :> r) where
  type Server (Capture a tag :> r) = a -> Server r
  route Proxy handler action ns text = do
    a <- readMaybe $ T.unpack text
    route (Proxy :: Proxy r) (handler a) action ns text

instance {-# OVERLAPPING #-} (HasServer r, KnownSymbol tag) => HasServer (Capture T.Text (tag :: Symbol) :> r) where
  type Server (Capture T.Text tag :> r) = T.Text -> Server r
  route Proxy handler action ns text | T.length text > 0 =
    route (Proxy :: Proxy r) (handler text) action ns (fst $ T.breakOn " " text)
  route _ _       _      _ _ = Nothing

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

instance (KnownSymbol s, KnownSymbol d, HasHelp r)
  => HasHelp ((NS (s :: Symbol) (d :: Symbol)) :> r) where
  help Proxy acc = acc
    <> (T.pack $ symbolVal (Proxy :: Proxy s)) <> " "
    <> (T.pack $ symbolVal (Proxy :: Proxy d)) <> "\n"

instance (KnownSymbol s, HasHelp r) => HasHelp ((s :: Symbol) :> r) where
  help Proxy acc =
    help (Proxy :: Proxy r) (acc <> (T.pack $ symbolVal (Proxy :: Proxy s)))

instance (Read a, KnownSymbol tag, HasHelp r) => HasHelp (Capture a (tag :: Symbol) :> r) where
  help Proxy acc =
    help (Proxy :: Proxy r) (acc <> " <" <> (T.pack $ symbolVal (Proxy :: Proxy tag)) <> ">")

serve :: (HasServer layout) => Proxy layout -> Server layout -> T.Text -> T.Text -> UserMonad ()
serve p h ns text@(T.uncons -> (Just ('/', _))) =
  fromMaybe (return ()) $ route p h Nothing ns $ ns <> text
serve p h ns text =
  fromMaybe (return ()) $ route p h Nothing ns text
