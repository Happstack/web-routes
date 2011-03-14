{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, TypeFamilies, PackageImports, FlexibleContexts, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Web.Route.RouteT
-- Copyright   :  (c) 2010 Jeremy Shaw
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  partners@seereason.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Declaration of the 'RouteT' monad transformer
-----------------------------------------------------------------------------
module Web.Routes.RouteT where

import Control.Applicative (Applicative((<*>), pure), Alternative((<|>), empty))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.Cont(MonadCont(callCC))
import Control.Monad.Error (MonadError(throwError, catchError))
import Control.Monad.Fix (MonadFix(mfix))
import Control.Monad.Reader(MonadReader(ask,local))
import Control.Monad.RWS (MonadRWS)
import Control.Monad.State(MonadState(get,put))
import Control.Monad.Trans (MonadTrans(lift), MonadIO(liftIO))
import Control.Monad.Writer(MonadWriter(listen, tell, pass))


-- * RouteT Monad Transformer

type Link = String

-- |monad transformer for generating URLs
newtype RouteT url m a = RouteT { unRouteT :: (url -> [(String, String)] -> Link) -> m a }
--     deriving (Functor, Monad, MonadFix, MonadPlus) -- , MonadIO, MonadTrans, MonadReader (url -> Link))

runRouteT :: RouteT url m a -> (url -> [(String, String)] -> Link) -> m a
runRouteT = unRouteT

-- | Transform the computation inside a @RouteT@.
mapRouteT :: (m a -> n b) -> RouteT url m a -> RouteT url n b
mapRouteT f (RouteT m) = RouteT $ f . m

-- | Execute a computation in a modified environment
withRouteT :: ((url' -> [(String, String)] -> Link) -> (url -> [(String, String)] -> Link)) -> RouteT url m a -> RouteT url' m a
withRouteT f (RouteT m) = RouteT $ m . f

liftRouteT :: m a -> RouteT url m a
liftRouteT m = RouteT (const m)

askRouteT :: (Monad m) => RouteT url m (url -> [(String, String)] -> String)
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

instance (MonadCont m) => MonadCont (RouteT url m) where
    callCC f = RouteT $ \url ->
        callCC $ \c ->
        unRouteT (f (\a -> RouteT $ \_ -> c a)) url

instance (MonadError e m) => MonadError e (RouteT url m) where
  throwError = liftRouteT . throwError
  catchError action handler = RouteT $ \f -> catchError (unRouteT action f) (\e -> unRouteT (handler e) f)

instance (MonadFix m) => MonadFix (RouteT url m) where
    mfix f = RouteT $ \ url -> mfix $ \ a -> unRouteT (f a) url

instance (MonadIO m) => MonadIO (RouteT url m) where  
  liftIO = lift . liftIO

instance (MonadReader r m) => MonadReader r (RouteT url m) where
  ask   = liftRouteT ask
  local f = mapRouteT (local f)

instance (MonadRWS r w s m) => MonadRWS r w s (RouteT url m)  

instance (MonadState s m) => MonadState s (RouteT url m) where  
  get = liftRouteT get
  put s = liftRouteT $ put s

instance MonadTrans (RouteT url) where
  lift = liftRouteT

instance (MonadWriter w m) => MonadWriter w (RouteT url m) where
  tell   w = liftRouteT $ tell w
  listen m = mapRouteT listen m
  pass   m = mapRouteT pass   m


class ShowURL m where
    type URL m
    showURLParams :: (URL m) -> [(String, String)] -> m Link -- ^ convert a URL value and a parameter list into a Link (aka, a String)

instance (Monad m) => ShowURL (RouteT url m) where
    type URL (RouteT url m) = url
    showURLParams url params =
        do showF <- askRouteT
           return (showF url params)

-- | convert a URL value into a Link (aka, a String) using a null parameter list.
showURL :: ShowURL m => URL m -> m Link  
showURL url = showURLParams url []

-- |used to embed a RouteT into a larger parent url
nestURL :: (Monad m) => (url2 -> url1) -> RouteT url2 m a -> RouteT url1 m a
nestURL b = withRouteT (. b)

crossURL :: (Monad m) => (url2 -> url1) -> [(String, String)] -> RouteT url1 m (url2 -> Link)
crossURL f params = 
    do showF <- askRouteT
       return $ \url2 -> showF (f url2) params
