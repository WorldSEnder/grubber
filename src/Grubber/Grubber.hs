{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
module Grubber.Grubber
( GrubberConfig(grubberRunLifecycle)
, defaultGrubberCfg
, FailureReason(..)
, MonadRecipeGrub
, GrubberM
, runGrubber
, runGrubberDef
, build
) where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Catch

import Data.Functor.Compose
import Data.Functor.Const
import Data.GADT.Compare
import Data.Dependent.Map as DM

import Grubber.Types
import Grubber.Blocking

data GrubberConfig = GrubberConfig
  { grubberRunLifecycle :: !Bool
  , _grubberExtend :: ()
  }

defaultGrubberCfg :: GrubberConfig
defaultGrubberCfg = GrubberConfig
  { grubberRunLifecycle = False
  , _grubberExtend = error "reserved for future extensions and not exported"
  }

data GrubberEnv k v = GrubberEnv
  { gsBuildCache :: BuildCache k v
  , gsCfg :: GrubberConfig
  }

instance HasBuildCache k v (GrubberEnv k v) where
  buildCache inner s = (\nc -> s {gsBuildCache = nc}) <$> inner (gsBuildCache s)

newtype GrubberM k v a = GrubberM { runGrubberM :: ReaderT (GrubberEnv k v) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadThrow, MonadCatch, MonadMask, LocallyIO)

deriving via (ReaderT (GrubberEnv k v) IO)
  instance MonadReader (GrubberEnv k v) (GrubberM k v)

instance MonadRestrictedIO (GrubberM k v) where
  liftOptionalIO act = do
    shouldRun <- GrubberM $ asks (grubberRunLifecycle . gsCfg)
    if shouldRun then GrubberM (liftIO act) else pure ()
  liftIdempotentIO = GrubberM . liftIO

runGrubber :: GrubberConfig -> GrubberM k v r -> IO r
runGrubber cfg m = do
  cache <- emptyBuildCacheIO
  runReaderT (runGrubberM m) $ GrubberEnv cache cfg

runGrubberDef :: GrubberM k v r -> IO r
runGrubberDef = runGrubber defaultGrubberCfg

type MonadRecipeGrub = MonadRestrictedIO

data FailureReason k
  = forall x. NoRecipeFound (k x)
  | forall x. DepFailed (k x) (FailureReason k)
  | RuntimeError SomeException

type BuildResult f a x = Either f (a x)

data TransactionValue k v x
  = InFlight
  { _tvCompletion :: TMVar (BuildResult (FailureReason k) v x)
  }
  | Built
  { _tvResult :: BuildResult (FailureReason k) v x
  }

data CachedResult k v x = CachedResult
newtype BuildCache k v = BuildCache
  { bcInnerMap :: TVar (DMap k (CachedResult k v))
  }

emptyBuildCacheIO :: IO (BuildCache k v)
emptyBuildCacheIO = do
  inner <- newTVarIO DM.empty
  return $ BuildCache inner

type Lens' a b = forall f. Functor f => (b -> f b) -> a -> f a

class HasBuildCache k v e where
  buildCache :: Lens' e (BuildCache k v)

data TransactionState k v
  = TransactionState
  { tsBuiltTargets :: TVar (DMap k (TransactionValue k v))
  , tsBuildCache :: BuildCache k v
  }

data AsyncResult f k v r = forall x. r ~ v x => AsyncResult (k x) (Async (BuildResult f v x))

type RecipeM k v m = ReaderT (TransactionState k v) m
type SchedulerM k v m = ExceptT (FailureReason k) (RecipeM k v m)
type ResolveM k f v m = BlockingListT (AsyncResult f k v) (ExceptT f m)

scheduleAsync :: forall e f k v m x. (MonadRestrictedIO m, LocallyIO m, MonadCatch m)
              => (forall y. k y -> f -> f)
              -> Scheduler (ResolveM k f v m) e k v x
              -> Scheduler (ExceptT f m) e k v x
scheduleAsync depFailed scheduleInner resolveDep target reci =
  loopBlockingT unblockAsyncList (scheduleInner liftedResolve target reci)
  where
    liftedResolve :: forall y. k y -> ResolveM k f v m (v y)
    liftedResolve key = do
      as <- lift .  lift $ locallyIO (runExceptT $ resolveDep key) async
      block $ AsyncResult key as
    waitResultSTM :: forall y. AsyncResult f k v y -> ExceptT f STM y
    waitResultSTM (AsyncResult key br) = do
      res <- lift $ waitSTM br
      case res of
        Left failed -> throwE $ depFailed key failed
        Right ok -> return ok
    -- TODO: shortcurcuit on failed dependency and cancel remaining active tasks.
    -- NOTE: canceling might be wrong, when other tasks are waiting on the same dependency.
    unblockAsyncList :: MonadIO m => TaskList (AsyncResult f k v) b -> ExceptT f m b
    unblockAsyncList = ExceptT . liftIO . atomically . runExceptT . elimTaskListA waitResultSTM

newtype Nat m n = NatMorph { (|~>) :: forall x. m x -> n x }

getReaderUnlift :: Monad m => ReaderT r m (Nat (ReaderT r m) m)
getReaderUnlift = ReaderT $ \env -> return $ NatMorph $ \rdr -> runReaderT rdr env

schedulerExceptReader :: Monad m
                      => Scheduler (ExceptT f m) e k v x
                      -> Scheduler (ExceptT f (ReaderT r m)) e k v x
schedulerExceptReader scheduleInner resolver target reci = do
  rdrNat <- lift getReaderUnlift
  mapExceptT lift $ scheduleInner (mapExceptT (rdrNat |~>) . resolver) target reci

scheduleCoop :: (MonadIO m, GCompare k)
             => Scheduler (SchedulerM k v m) e k v x
             -> Scheduler (SchedulerM k v m) e k v x
scheduleCoop scheduleInner resolveDep target reci = do
  targetInfo <- asks tsBuiltTargets
  strat <- liftIO . atomically $ do
    buildInfo <- readTVar targetInfo
    (promise, newBuildInfo) <- getCompose $ flip (DM.alterF target) buildInfo $ \x ->
      Compose $ case x of
        Just existing -> return (Left existing, x)
        Nothing -> do
          newPromise <- newEmptyTMVar
          return (Right newPromise, Just $ InFlight newPromise)
    writeTVar targetInfo newBuildInfo
    return promise
  case strat of
    Left (Built res)        -> ExceptT $ return res
    Left (InFlight promise) -> ExceptT $ liftIO . atomically $ readTMVar promise
    Right promise           -> ExceptT $ do
      val <- runExceptT $ scheduleInner resolveDep target reci
      liftIO . atomically $ putTMVar promise val
      return val

build :: (MonadRestrictedIO m, LocallyIO m, MonadCatch m, MonadReader e m, HasBuildCache k v e, GCompare k)
      => (forall x. FailureReason k -> m (v x))
      -> Build m MonadRecipeGrub k v
build onFailure recipes globalGoal = do
  m <- liftIO $ newTVarIO empty
  c <- asks (getConst . buildCache Const)
  res <- runReaderT (runExceptT $ mkTarget globalGoal) $ TransactionState m c
  finalize res
  where
    grubberScheduler = scheduleCoop $ schedulerExceptReader $ scheduleAsync DepFailed scheduleResolver

    mkTarget = runScheduler (throwE . NoRecipeFound) grubberScheduler recipes

    finalize (Left e) = onFailure e
    finalize (Right ok) = pure ok
