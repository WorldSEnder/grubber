{-# LANGUAGE ConstraintKinds #-}
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
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Catch

import Data.Functor.Compose
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

newtype GrubberM a = GrubberM { runGrubberM :: ReaderT GrubberConfig IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadThrow, MonadCatch, MonadMask, LocallyIO)

instance MonadRestrictedIO GrubberM where
  liftOptionalIO act = do
    shouldRun <- GrubberM $ asks grubberRunLifecycle
    if shouldRun then GrubberM (liftIO act) else pure ()
  liftIdempotentIO = GrubberM . liftIO

runGrubber :: GrubberConfig -> GrubberM r -> IO r
runGrubber cfg m = runReaderT (runGrubberM m) cfg

runGrubberDef :: GrubberM r -> IO r
runGrubberDef = runGrubber defaultGrubberCfg

type MonadRecipeGrub = MonadRestrictedIO

data FailureReason k
  = forall x. NoRecipeFound (k x)
  | forall x. DepFailed (k x) (FailureReason k)
  | RuntimeError SomeException

type BuildResult k a = Either (FailureReason k) a

data TransactionValue k v x
  = InFlight
  { _tvCompletion :: TMVar (BuildResult k (v x))
  }
  | Built
  { _tvResult :: BuildResult k (v x)
  }

data TransactionState k v
  = TransactionState
  { tsBuildTargets :: TVar (DMap k (TransactionValue k v))
  , _tsTiming :: Int
  }

data AsyncResult k r = forall x. AsyncResult (k x) (Async (BuildResult k r))

unblockAsyncList :: MonadIO m => TaskList (AsyncResult k) b -> SchedulerM k v m b
unblockAsyncList = ExceptT . liftIO . atomically . runExceptT . elimTaskListA waitResultSTM
  -- TODO: shortcurcuit on failed dependency and cancel remaining active tasks.
  -- NOTE: canceling might be wrong, when other tasks are waiting on the same dependency.
 where
  waitResultSTM :: AsyncResult k r -> ExceptT (FailureReason k) STM r
  waitResultSTM (AsyncResult key br) = do
    res <- lift $ waitSTM br
    case res of
      Left failed -> throwE $ DepFailed key failed
      Right ok -> return ok

type RecipeM k v m = ReaderT (TransactionState k v) m
type SchedulerM k v m = ExceptT (FailureReason k) (RecipeM k v m)
type ResolveM k v m = BlockingListT (AsyncResult k) (SchedulerM k v m)

build :: forall k v m. (MonadRestrictedIO m, LocallyIO m, MonadMask m, GCompare k)
      => (FailureReason k -> m ())
      -> Build m MonadRecipeGrub k v
build onFailure recipes globalGoal = do
  state <- liftIO newTransaction
  res <- runReaderT (runExceptT $ mkTarget globalGoal) state
  finalize res
  where
    newTransaction = do
      m <- newTVarIO empty
      return $ TransactionState m 0

    resolveDep :: k x -> WithResolverT k v (ResolveM k v m) (v x)
    resolveDep key = do
      as <- lift . lift . lift $ locallyIO (runExceptT $ mkTarget key) async
      lift . block $ AsyncResult key as

    runReci :: forall x. Recipe MonadRecipeGrub k v x -> SchedulerM k v m (v x)
    runReci reci = loopBlockingT unblockAsyncList (runResolver resolveDep unpackedReci)
                   `catchAll` (throwE . RuntimeError)
      where unpackedReci :: WithResolverT k v (ResolveM k v m) (v x)
            unpackedReci = runRecipe reci

    mkTarget :: k x -> SchedulerM k v m (v x)
    mkTarget target = ExceptT $ do
      targetInfo <- asks tsBuildTargets
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
        Left (Built res)        -> return res
        Left (InFlight promise) -> liftIO . atomically $ readTMVar promise
        Right promise           -> case recipes target of
          Nothing   -> return $ Left $ NoRecipeFound target
          Just reci -> do
            val <- runExceptT $ runReci reci
            liftIO . atomically $ putTMVar promise val
            return val

    finalize (Left e) = onFailure e
    finalize _ = pure ()
