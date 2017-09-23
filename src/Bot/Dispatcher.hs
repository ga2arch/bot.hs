{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bot.Dispatcher where

import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad.Trans.Class
import Data.Typeable
import Data.Maybe
import Data.Proxy
import qualified STMContainers.Map as M

type Listeners = M.Map TypeRep Listener

data Listener where
  Listener :: (Typeable eventType) => TypeRep -> TChan eventType -> Listener

newtype Dispatcher a = D { unD :: ReaderT Listeners IO a }
  deriving (Monad, Functor, Applicative, MonadReader Listeners, MonadIO)

addListener :: forall event. (Typeable event) => TChan event -> Dispatcher ()
addListener chan = do
  listeners <- ask
  let key = typeRep (Proxy :: Proxy event)
  let listener = Listener key chan
  liftIO $ atomically $ M.insert listener key listeners
  return ()

dispatch :: forall event. (Typeable event) => event -> Dispatcher ()
dispatch event = do
  listeners <- ask
  listener <- liftIO $ atomically $ M.lookup (typeOf event) listeners
  case listener of
    Nothing -> return()
    Just (Listener _ chan) -> do
      liftIO $ atomically $ writeTChan (fromJust $ cast chan) event
      return ()

dispatch' :: forall event. (Typeable event) => Listeners -> event -> IO ()
dispatch' listeners event = runD (dispatch event) listeners

addListener' :: forall event. (Typeable event) => Listeners -> TChan event -> IO ()
addListener' listener chan = runD (addListener chan) listener

runD :: Dispatcher a -> Listeners -> IO a
runD f m = runReaderT (unD f) m
