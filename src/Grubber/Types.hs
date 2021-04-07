{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
module Grubber.Types
( Recipe
, BuildEnv
, RecipeOutput
, recipe
, recipe'
, runRecipe
, scheduleResolver
, DependencyResolver(..)
, WithResolverT(..)
, Resolver(..)
, runResolver
, RecipeBook
, Scheduler
, runScheduler
, runSchedulerX
, Build
, BuildX
, MonadRestrictedIO(..)
, AuxInput
, AccessAuxInput(..)
) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad.Base
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control

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

type family BuildEnv (e :: k) (f :: Type -> Type) (x :: k1) :: Constraint
type instance BuildEnv c f _ = c f
type instance BuildEnv c f x = c f x
type instance BuildEnv '[] f _ = ()
type instance BuildEnv (e ': es) f x = (BuildEnv e f x, BuildEnv es f x)
type instance BuildEnv ('(,) a b) f x = (BuildEnv a f x, BuildEnv b f x)
type instance BuildEnv ('(,,) a b c) f x = (BuildEnv a f x, BuildEnv b f x, BuildEnv c f x)
type instance BuildEnv ('(,,,) a b c d) f x = (BuildEnv a f x, BuildEnv b f x, BuildEnv c f x, BuildEnv d f x)

type family RecipeOutput (e :: k) :: Type
type instance RecipeOutput e = e

class DependencyResolver k v f where
  resolve :: k a -> f (v a)

-- | A recipe for producing a value of type 'b' in any environment fulfilling 'c',
-- having access to a dependency resolution mechanics for resolving keys 'k a' to values 'v a'
newtype Recipe e k v x = Recipe { runRecipe :: forall f. (BuildEnv e f x) => WithResolverT k v f (RecipeOutput x) }

recipe :: (forall f. BuildEnv e f x => WithResolverT k v f (RecipeOutput x)) -> Recipe e k v x
recipe = Recipe

recipe' :: (forall f. BuildEnv e f x => (forall a. k a -> f (v a)) -> f (RecipeOutput x)) -> Recipe e k v x
recipe' reci = Recipe $ WithResolver $ ReaderT $ \(Resolver resolv) -> reci resolv

newtype Resolver k v m = Resolver (forall x. k x -> m (v x))

newtype WithResolverT k v m a = WithResolver { runResolver_ :: ReaderT (Resolver k v m) m a }
  deriving (Functor, Applicative, Monad, MonadRestrictedIO)

instance MonadTrans (WithResolverT k v) where
  lift ma = WithResolver $ lift ma

deriving instance MonadBase b m => MonadBase b (WithResolverT k v m)
deriving instance MonadBaseControl b m => MonadBaseControl b (WithResolverT k v m)

instance DependencyResolver k v (WithResolverT k v m) where
  resolve key = WithResolver $ ReaderT $ \(Resolver r) -> r key

instance AccessAuxInput m x => AccessAuxInput (WithResolverT k v m) x where
  getAuxInput = WithResolver $ ReaderT $ const getAuxInput

runResolver :: (forall x. k x -> m (v x)) -> WithResolverT k v m a -> m a
runResolver r task = runReaderT (runResolver_ task) (Resolver r)

type Scheduler m e k v x = (forall y. k y -> m (v y))
                         -> k x -> Recipe e k v x -> m (RecipeOutput x)

scheduleResolver :: (BuildEnv e m x)
                 => Scheduler m e k v x
scheduleResolver deps _ reci = runResolver deps $ runRecipe reci

type RecipeBook e k v = forall x. k x -> Maybe (Recipe e k v x)

-- | A build system working in a monad 'm', most likely supporting some kind of state,
-- implements refreshing a key 'k x' given a book of rules to run.
type Build m e k v = forall x. RecipeBook e k v -> k x -> m (RecipeOutput x)
type BuildX m e k v = forall x. RecipeBook e k v -> k x -> m x (RecipeOutput x)

runScheduler :: forall m e k v. ()
             => (forall x. k x -> m (RecipeOutput x))
             -> (forall y. k y -> m (RecipeOutput y) -> m (v y))
             -> (forall x. Scheduler m e k v x)
             -> Build m e k v
runScheduler onNoRecipe augmentResult schedule recipes = go
  where
    resolv :: forall y. k y -> m (v y)
    resolv k = augmentResult k $ go k
    go :: forall y. k y -> m (RecipeOutput y)
    go k  = case recipes k of
              Nothing -> onNoRecipe k
              Just reci -> schedule resolv k reci

runSchedulerX :: forall m e k v. ()
              => (forall x. k x -> m x (RecipeOutput x))
              -> (forall x y. k y -> m y (RecipeOutput y) -> m x (v y))
              -> (forall x. Scheduler (m x) e k v x)
              -> BuildX m e k v
runSchedulerX onNoRecipe augmentDepResult schedule recipes = go
  where
    go :: forall y. k y -> m y (RecipeOutput y)
    go k  = case recipes k of
              Nothing -> onNoRecipe k
              Just reci -> schedule (\l -> augmentDepResult l $ go l) k reci

type family AuxInput (e :: k) :: *

class AccessAuxInput m x | m -> x where
  getAuxInput :: m (AuxInput x)
