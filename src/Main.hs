{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor, RankNTypes #-}
module Main where

import Control.Monad.Free
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent

-- * A la carte
data (f :+: g) e = Inl (f e) | Inr (g e)
infixr 9 :+:

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

inject :: (g :<: f) => g (Free f a) -> Free f a
inject = Free . inj

-- * DSL
data Bot next  = Send next
               | Prompt String (String -> next)
  deriving (Functor)

data Feeder next = Parse next
  deriving (Functor, Show)

data Youtube next = Download String next
  deriving (Functor, Show)

send :: (Functor f, MonadFree f m, Bot :<: f) => m ()
send = liftF . inj $ Send ()

prompt :: (Functor f, MonadFree f m, Bot :<: f) => String -> m String
prompt name = liftF . inj $ Prompt name id

parse :: (Functor f, MonadFree f m, Feeder :<: f) => m ()
parse = liftF . inj $ Parse ()

download :: String -> (Functor f, MonadFree f m, Youtube :<: f) => m ()
download url = liftF . inj $ Download url ()

-- * Interpreters

data User = User {userChan :: TChan String
                 }

newtype UserMonad a = UserMonad { runUser :: ReaderT User IO a }
  deriving (Monad, Applicative, Functor, MonadReader User, MonadIO)

class (Functor f, Monad m, MonadReader User m) => Eval m f where
  runAlgebra :: f (m a) -> m a

instance Eval UserMonad Bot where
  runAlgebra (Send next) = do
    liftIO $ print "send"
    next

  runAlgebra (Prompt name next) = do
    liftIO $ print "prompt"
    chan <- asks userChan
    resp <- liftIO $ atomically $ readTChan chan
    next resp

instance Eval UserMonad Feeder where
  runAlgebra (Parse next) = do
    liftIO $ print "parse"
    next

instance Eval UserMonad Youtube where
  runAlgebra (Download url next) = do
    liftIO $ print $ "download: " ++ url
    next

instance (Monad m, Eval m f, Eval m g) => Eval m (f :+: g) where
  runAlgebra (Inl r) = runAlgebra r
  runAlgebra (Inr r) = runAlgebra r

-- * Plugins
plugin :: Free (Bot :+: Feeder :+: Youtube) ()
plugin = do
  send
  parse
  name <- prompt "url to download: "
  download name

-- * Bot
main = do
  chan <- newTChanIO
  forkIO $ do
    threadDelay 5000000
    atomically $ writeTChan chan "gabriele"

  flip runReaderT (User chan) $ runUser $ run plugin
 where
  run = iterM runAlgebra
