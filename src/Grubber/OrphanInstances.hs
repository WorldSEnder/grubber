{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Grubber.OrphanInstances
() where

import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Control

instance MonadTransControl ResourceT where
  type StT ResourceT a = a
  liftWith f = do
    rmap <- getInternalState
    lift $ f $ flip runInternalState rmap
  restoreT = lift

instance (MonadBase b m) => MonadBase b (ResourceT m) where
  liftBase = lift . liftBase
  
instance (MonadBaseControl b m) => MonadBaseControl b (ResourceT m) where
  type StM (ResourceT m) a = StM m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM
