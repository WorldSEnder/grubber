{-# LANGUAGE FunctionalDependencies #-}
-- GHC can't see through Reifies determining ctx from s, hence ctx being determined
-- in the instance for MonadContext ctx (CtxReflT s m)
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
module Grubber.MonadContext
( MonadContext(..)
, CtxReflT(..)
, runCtxReflT
, mapCtxReflT
, withCtxReflT
, MonadIOOps(..)
, MonadIOFromCtx(..)
) where

import Control.Monad.IO.Class

import Data.Reflection
import Data.Proxy

-- | Like MonadReader, but without the inherent `local` operation
class Monad m => MonadContext ctx m | m -> ctx where 
  capture :: m ctx
  captures :: (ctx -> a) -> m a
  captures f = f <$> capture
  fromContext :: (ctx -> m a) -> m a
  fromContext = (capture >>=)

-- | Reflect
newtype CtxReflT s m a = CtxReflT { unCtxReflT :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Monad m, Reifies s ctx) => MonadContext ctx (CtxReflT s m) where
  capture = fromContext pure
  captures f = fromContext (pure . f)
  fromContext f = f $ reflect (Proxy :: Proxy s)
  {-# INLINE fromContext #-}

runCtxReflT :: (forall s. Reifies s ctx => CtxReflT s m a) -> ctx -> m a
runCtxReflT ms ctx = reify ctx $ \(_ :: Proxy s) -> unCtxReflT $ ms @s

-- | Transform the computation inside a CtxReflT.
mapCtxReflT :: (m a -> n b) -> CtxReflT r m a -> CtxReflT r n b 
mapCtxReflT h c = CtxReflT $ h $ unCtxReflT c

-- | Execute a computation in a modified environment.
withCtxReflT :: (Monad m, Reifies t r') => (r' -> r) -> (forall s. Reifies s r => CtxReflT s m a) -> CtxReflT t m a
withCtxReflT rm cont = fromContext $ \r' -> CtxReflT $ runCtxReflT cont $ rm r'

-- | A reified version of the MonadIO class dictionary.
newtype MonadIOOps m = MonadIOOps
  { opsLiftIO :: forall a. IO a -> m a
  }

-- | Class of contexts that include a MonadIOOps dict
class HasMonadIOOps ctx m where
  monadIOOps :: ctx -> MonadIOOps m

-- | Newtype for deriving a MonadIO instance where the io effect is derived
-- from the MonadContext instance.
newtype MonadIOFromCtx m a = MonadIOFromCtx { runMonadIOFromCtx :: m a }
  deriving (Functor, Applicative, Monad)

instance (MonadContext ctx m, HasMonadIOOps ctx m) => MonadIO (MonadIOFromCtx m) where
  liftIO io = MonadIOFromCtx $ fromContext $ \ctx -> opsLiftIO (monadIOOps ctx) io
