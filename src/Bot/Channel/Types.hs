{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module Bot.Channel.Types where

import Bot.Command.Types

data Expr f = In (f (Expr f ))
inject :: (g :<: f ) => g (Expr f ) -> Expr f
inject = In . inj

class Channel f repr where
  sendChannel :: f a -> repr ()

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f ) t)
