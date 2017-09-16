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
