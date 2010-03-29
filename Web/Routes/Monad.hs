{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies, PackageImports, FlexibleContexts, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  RouteT.Monad
-- Copyright   :  (c) 2010 Jeremy Shaw
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  partners@seereason.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Declaration of the 'RouteT' monad transformer
-----------------------------------------------------------------------------
module Web.Routes.Monad where

import Control.Applicative (Applicative((<*>), pure), Alternative((<|>), empty))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.Fix (MonadFix(mfix))

-- * RouteT Monad Transformer

type Link = String

-- |monad transformer for generating URLs
newtype RouteT url m a = RouteT { unRouteT :: (url -> Link) -> m a }
--     deriving (Functor, Monad, MonadFix, MonadPlus) -- , MonadIO, MonadTrans, MonadReader (url -> Link))

runRouteT :: RouteT url m a -> (url -> Link) -> m a
runRouteT = unRouteT

-- | Transform the computation inside a @RouteT@.
mapRouteT :: (m a -> n b) -> RouteT url m a -> RouteT url n b
mapRouteT f (RouteT m) = RouteT $ f . m

-- | Execute a computation in a modified environment
withRouteT :: ((url' -> Link) -> (url -> Link)) -> RouteT url m a -> RouteT url' m a
withRouteT f (RouteT m) = RouteT $ m . f

liftRouteT :: m a -> RouteT url m a
liftRouteT m = RouteT (const m)

askRouteT :: (Monad m) => RouteT url m (url -> String)
askRouteT = RouteT return

instance (Functor m) => Functor (RouteT url m) where
  fmap f = mapRouteT (fmap f)
  
instance (Applicative m) => Applicative (RouteT url m) where  
  pure = liftRouteT . pure
  f <*> v = RouteT $ \ url -> unRouteT f url <*> unRouteT v url

instance (Alternative m) => Alternative (RouteT url m) where
    empty   = liftRouteT empty
    m <|> n = RouteT $ \ url -> unRouteT m url <|> unRouteT n url

instance (Monad m) => Monad (RouteT url m) where
    return   = liftRouteT . return
    m >>= k  = RouteT $ \ url -> do
        a <- unRouteT m url
        unRouteT (k a) url
    fail msg = liftRouteT (fail msg)

instance (MonadPlus m, Monad (RouteT url m)) => MonadPlus (RouteT url m) where
    mzero       = liftRouteT mzero
    m `mplus` n = RouteT $ \ url -> unRouteT m url `mplus` unRouteT n url

instance (MonadFix m) => MonadFix (RouteT url m) where
    mfix f = RouteT $ \ url -> mfix $ \ a -> unRouteT (f a) url

class ShowURL m where
    type URL m
    showURL :: (URL m) -> m Link -- ^ convert a URL value into a Link (aka, a String)

instance (Monad m) => ShowURL (RouteT url m) where
    type URL (RouteT url m) = url
    showURL url =
        do showF <- askRouteT
           return (showF url)

-- |used to embed a RouteT into a larger parent url
nestURL :: (Monad m) => (url2 -> url1) -> RouteT url2 m a -> RouteT url1 m a
nestURL b = withRouteT (. b)

crossURL :: (Monad m) => (url2 -> url1) -> RouteT url1 m (url2 -> Link)
crossURL f = 
    do showF <- askRouteT
       return $ \url2 -> showF (f url2)
