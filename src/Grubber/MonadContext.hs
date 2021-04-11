{-# LANGUAGE FunctionalDependencies #-}
-- GHC can't see through Reifies determining ctx from s, hence ctx being determined
-- in the instance for MonadContext ctx (CtxReflT s m)
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
module Grubber.MonadContext
( MonadContext(..)
, ContextFromReader(..)
, CtxReflT(..)
, runCtxReflT
, mapCtxReflT
, withCtxReflT
, MonadIOOps(..)
, MonadIOFromCtx(..)
) where

import Control.Monad
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Zip
import Control.Applicative
import Control.Monad.Trans.Control

import Data.Coerce
import Data.Reflection
import Data.Proxy

import Grubber.Types

-- | Like MonadReader, but without the inherent `local` operation
class Monad m => MonadContext ctx m | m -> ctx where
  -- | There are some additional laws one should follow:
  -- ```
  -- capture >> capture = capture
  -- captures = <$> capture
  -- fromContext = (capture >>=)
  -- ```
  -- Further, capture should commute with *every* other monadic operation.
  capture :: m ctx
  captures :: (ctx -> a) -> m a
  captures = (<$> capture)
  fromContext :: (ctx -> m a) -> m a
  fromContext = (capture >>=)

-- | Reflect
newtype CtxReflT s m a = CtxReflT { unCtxReflT :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadFix, MonadZip, Alternative, MonadPlus)

deriving instance MonadBase b m => MonadBase b (CtxReflT s m)
deriving instance MonadBaseControl b m => MonadBaseControl b (CtxReflT s m)

instance MonadTrans (CtxReflT s) where
  lift = coerce
  {-# INLINE lift #-}

instance MonadTransControl (CtxReflT s) where
  type StT (CtxReflT s) a = a
  liftWith f = coerce $ f coerce
  {-# INLINE liftWith #-}
  restoreT = coerce
  {-# INLINE restoreT #-}

deriving via (MonadRestrictedIOViaTrans (CtxReflT s) m)
  instance MonadRestrictedIO m => MonadRestrictedIO (CtxReflT r m)

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

newtype ContextFromReader m a = ContextFromReader { runR2Ctx :: m a }
  deriving (Functor, Applicative, Monad)

instance MonadReader r m => MonadContext r (ContextFromReader m) where
  capture = ContextFromReader ask
  {-# INLINE capture #-}
  captures = ContextFromReader . reader
  {-# INLINE captures #-}

deriving via (ContextFromReader (ReaderT r m))
  instance Monad m => MonadContext r (ReaderT r m)
