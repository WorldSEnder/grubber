{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
module Grubber.Types
( Recipe
, BuildEnv
, recipe
, runRecipe
, scheduleResolver
, DependencyResolver(..)
, WithResolverT(..)
, runResolver
, RecipeBook
, Scheduler
, runScheduler
, runSchedulerX
, Build
, BuildX
, LocallyIO(..)
, MonadRestrictedIO(..)
) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Kind
import Grubber.Blocking

-- | Like MonadIO but less powerful. Only allows lifting of actions which result in unit.
class Monad m => MonadRestrictedIO m where
  -- | Suggest an io action to run as a side effect. Since one is not allowed to
  -- recover any result from the io action, the side effect can safely be ignored,
  -- but lifecycle events, logging and emitting debugging information can still be
  -- performed.
  liftOptionalIO :: IO () -> m ()
  -- Lift a repeatable io action. No guarantees if the io action can not be repeated.
  -- liftIdempotentIO :: IO a -> m a

instance MonadRestrictedIO IO where
  liftOptionalIO = id
  -- liftIdempotentIO = id

newtype MonadRestrictedIOViaIO m a = MonadRestrictedIOViaIO (m a)
  deriving (Functor, Applicative, Monad)
newtype MonadRestrictedIOViaTrans t m a = MonadRestrictedIOViaTrans (t m a)
  deriving (Functor, Applicative, Monad)
instance MonadIO m => MonadRestrictedIO (MonadRestrictedIOViaIO m) where
  liftOptionalIO = MonadRestrictedIOViaIO . liftIO
  -- liftIdempotentIO = MonadRestrictedIOViaIO . liftIO

instance (MonadRestrictedIO m, MonadTrans t, Monad (t m)) => MonadRestrictedIO (MonadRestrictedIOViaTrans t m) where
  liftOptionalIO = MonadRestrictedIOViaTrans . lift . liftOptionalIO
  -- liftIdempotentIO = MonadRestrictedIOViaTrans . lift . liftIdempotentIO

deriving via (MonadRestrictedIOViaTrans (ReaderT r) m)
  instance MonadRestrictedIO m => MonadRestrictedIO (ReaderT r m)
deriving via (MonadRestrictedIOViaTrans (StateT r) m)
  instance MonadRestrictedIO m => MonadRestrictedIO (StateT r m)
deriving via (MonadRestrictedIOViaTrans (ExceptT r) m)
  instance MonadRestrictedIO m => MonadRestrictedIO (ExceptT r m)
deriving via (MonadRestrictedIOViaTrans (BlockingT w) m)
  instance (Semigroupal w, MonadRestrictedIO m) => MonadRestrictedIO (BlockingT w m)

class MonadIO m => LocallyIO m where
  locallyIO :: m a -> (IO a -> IO b) -> m b

instance LocallyIO IO where
  locallyIO ma f = f ma

instance LocallyIO m => LocallyIO (ReaderT r m) where
  locallyIO rdr f = ReaderT $ \r -> locallyIO (runReaderT rdr r) f

class DependencyResolver k v f where
  resolve :: k a -> f (v a)

type family BuildEnv (e :: k) (f :: Type -> Type) :: Constraint
type instance BuildEnv c f = c f
type instance BuildEnv '[] f = ()
type instance BuildEnv (e ': es) f = (BuildEnv e f, BuildEnv es f)

-- | A recipe for producing a value of type 'b' in any environment fulfilling 'c',
-- having access to a dependency resolution mechanics for resolving keys 'k a' to values 'v a'
newtype Recipe e k v x = Recipe { runRecipe :: forall f. (BuildEnv e f) => WithResolverT k v f (v x) }

recipe :: (forall f. (BuildEnv e f) => WithResolverT k v f (v x)) -> Recipe e k v x
recipe = Recipe

newtype Resolver k v m = Resolver (forall x. k x -> m (v x))

newtype WithResolverT k v m a = WithResolver { runResolver_ :: ReaderT (Resolver k v m) m a }
  deriving (Functor, Applicative, Monad, MonadIO, LocallyIO, MonadRestrictedIO)

instance MonadTrans (WithResolverT k v) where
  lift ma = WithResolver $ lift ma

instance DependencyResolver k v (WithResolverT k v m) where
  resolve key = WithResolver $ ReaderT $ \(Resolver r) -> r key

runResolver :: (forall x. k x -> m (v x)) -> WithResolverT k v m a -> m a
runResolver r task = runReaderT (runResolver_ task) (Resolver r)

type Scheduler m e k v x = (forall y. k y -> m (v y))
                         -> k x -> Recipe e k v x -> m (v x)

scheduleResolver :: (BuildEnv e m)
                 => Scheduler m e k v x
scheduleResolver deps _ reci = runResolver deps $ runRecipe reci 

type RecipeBook c k v = forall x. k x -> Maybe (Recipe c k v x)

-- | A build system working in a monad 'm', most likely supporting some kind of state,
-- implements refreshing a key 'k x' given a book of rules to run.
type Build m e k v = forall x. RecipeBook e k v -> k x -> m (v x)
type BuildX m e k v = forall x. RecipeBook e k v -> k x -> m x (v x)

runScheduler :: forall m e k v. ()
             => (forall x. k x -> m (v x))
             -> (forall x. Scheduler m e k v x)
             -> Build m e k v
runScheduler onNoRecipe schedule recipes = go
  where
    go :: forall y. k y -> m (v y)
    go k  = case recipes k of
              Nothing -> onNoRecipe k
              Just reci -> schedule go k reci

runSchedulerX :: forall m e k v. ()
              => (forall x. k x -> m x (v x))
              -> (forall x. Scheduler (m x) e k v x)
              -> (forall x y r. k y -> m y r -> m x r)
              -> BuildX m e k v
runSchedulerX onNoRecipe schedule inDep recipes = go
  where
    go :: forall y. k y -> m y (v y)
    go k  = case recipes k of
              Nothing -> onNoRecipe k
              Just reci -> schedule (\l -> inDep l $ go l) k reci
