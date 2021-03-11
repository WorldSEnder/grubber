{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Grubber.Grubber
( GrubberConfig(grubberRunLifecycle)
, defaultGrubberCfg
, FailureReason(..)
, MonadRecipeGrub
, MonadRecipeGrubAux
, GrubberM
, runGrubber
, runGrubberDef
, build
, supplyFileTarget
, supplyFileTargets
) where

import Control.Monad.Base
import Control.Monad.Trans.Control
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
import Data.Constraint
import Data.Reflection
import Data.Proxy

import Grubber.Types
import Grubber.Blocking
import Grubber.Filesystem
import Grubber.Internal

import System.IO

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
  deriving (Functor, Applicative, Monad, MonadFail, MonadThrow, MonadCatch, MonadMask)

deriving instance MonadBase IO (GrubberM k v)
deriving instance MonadBaseControl IO (GrubberM k v)

deriving via (ReaderT (GrubberEnv k v) IO)
  instance MonadReader (GrubberEnv k v) (GrubberM k v)

instance MonadRestrictedIO (GrubberM k v) where
  liftOptionalIO act = do
    shouldRun <- GrubberM $ asks (grubberRunLifecycle . gsCfg)
    if shouldRun then GrubberM (liftBase act) else pure ()
  -- liftIdempotentIO = GrubberM . liftIO

runGrubber :: GrubberConfig -> GrubberM k v r -> IO r
runGrubber cfg m = do
  cache <- emptyBuildCacheIO
  runReaderT (runGrubberM m) $ GrubberEnv cache cfg

runGrubberDef :: GrubberM k v r -> IO r
runGrubberDef = runGrubber defaultGrubberCfg

data FailureReason k x
  = NoRecipeFound (k x)
  | forall y. DepFailed (k y) (FailureReason k y)
  | RuntimeError SomeException

type BuildResult f a x = Either f (a x)

data TransactionValue k v x
  = InFlight
  { _tvCompletion :: TMVar (BuildResult (FailureReason k x) v x)
  }
  | Built
  { _tvResult :: BuildResult (FailureReason k x) v x
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

data AsyncResult f v m r = forall x. r ~ v x => AsyncResult (Async (StM (ExceptT f m) (v x)))

-- the monad to use to actually *run* a recipe in.
newtype RecipeEnvT x f k v m r = RecipeEnvT { runRecipeT :: BlockingListT (AsyncResult (f x) v m) (ExceptT (f x) m) r }
  deriving (Functor, Applicative, Monad, MonadRestrictedIO)

deriving instance MonadBase b m => MonadBase b (RecipeEnvT x f k v m)
deriving instance MonadBaseControl b m => MonadBaseControl b (RecipeEnvT x f k v m)

instance MonadBaseControl IO m => FileReading (RecipeEnvT x f k v m) where
  readFile (FileReadToken fp) hdl = withDict (internalIO (Proxy :: Proxy (RecipeEnvT x f k v m))) $
    liftBaseWith (\runInBase -> withBinaryFile fp ReadMode $ runInBase . hdl) >>= restoreM

instance MonadBaseControl IO m => FileWriting (RecipeEnvT x f k v m) where
  type FileWriteToken (RecipeEnvT x f k v m) = FilePath
  writeFile fp hdl = withDict (internalIO (Proxy :: Proxy (RecipeEnvT x f k v m))) $
    liftBaseWith (\runInBase -> withBinaryFile fp WriteMode $ runInBase . hdl) >>= restoreM
  toReadToken = return . FileReadToken

instance MonadBaseControl IO m => InternalOperations (RecipeEnvT x f k v m)
instance MonadBaseControl IO m => HasInternalOperations (RecipeEnvT x f k v m) where
  internalDict _ = Dict

scheduleAsync :: forall e f k v m x.
                 (MonadRestrictedIO m, MonadBaseControl IO m, MonadCatch m)
              => Scheduler (RecipeEnvT x f k v m) e k v x
              -> Scheduler (ExceptT (f x) m) e k v x
scheduleAsync scheduleInner resolveDep target reci =
  loopBlockingT unblockAsyncList (runRecipeT $ scheduleInner liftedResolve target reci)
  where
    liftedResolve :: forall y. k y -> RecipeEnvT x f k v m (v y)
    liftedResolve key = RecipeEnvT $ do
      as <- lift .  lift $ liftBaseWith (\unlift -> async (unlift $ runExceptT $ resolveDep key))
      block (AsyncResult as :: AsyncResult (f x) v m (v y))
    waitResultSTM :: forall y. AsyncResult (f x) v m y -> ExceptT (f x) m y
    waitResultSTM (AsyncResult br) = do
      stmRes <- liftBase $ atomically $ waitSTM br
      res2 <- lift $ restoreM stmRes
      case res2 of
        Left failed -> throwE failed
        Right ok -> return ok
    -- TODO: shortcurcuit on failed dependency and cancel remaining active tasks.
    -- NOTE: canceling might be wrong, when other tasks are waiting on the same dependency.
    unblockAsyncList :: MonadBase IO m => TaskList (AsyncResult (f x) v m) b -> ExceptT (f x) m b
    unblockAsyncList = elimTaskListA waitResultSTM

schedulerExceptReader :: Monad m
                      => Scheduler (ExceptT f m) e k v x
                      -> Scheduler (ExceptT f (ReaderT r m)) e k v x
schedulerExceptReader scheduleInner resolver target reci = ExceptT $ liftWith $ \runner ->
  runExceptT $ scheduleInner (mapExceptT runner . resolver) target reci

type SchedulerM k v m x = ExceptT (FailureReason k x) (ReaderT (TransactionState k v) m)

scheduleCoop :: (MonadBase IO m, GCompare k)
             => Scheduler (SchedulerM k v m x) e k v x
             -> Scheduler (SchedulerM k v m x) e k v x
scheduleCoop scheduleInner resolveDep target reci = do
  targetInfo <- asks tsBuiltTargets
  strat <- liftBase . atomically $ do
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
    Left (InFlight promise) -> ExceptT $ liftBase . atomically $ readTMVar promise
    Right promise           -> ExceptT $ do
      val <- runExceptT $ scheduleInner resolveDep target reci
      liftBase . atomically $ putTMVar promise val
      return val

newtype BuildT k v m x r = BuildT { runBuildT :: SchedulerM k v m x r }

scheduleBuild :: Scheduler (SchedulerM k v m x) e k v y -> Scheduler (BuildT k v m x) e k v y
scheduleBuild scheduleInner resolveDeps target reci = BuildT $ scheduleInner (runBuildT . resolveDeps) target reci

type MonadRecipeGrub = '[MonadRestrictedIO, HasInternalOperations, FileReading]

build :: forall e k v m.
         (MonadRestrictedIO m, MonadBaseControl IO m, MonadCatch m, MonadReader e m, HasBuildCache k v e, GCompare k)
      => (forall x. FailureReason k x -> m (v x))
      -> Build m MonadRecipeGrub k v
build onFailure recipes globalGoal = do
  m <- liftBase $ newTVarIO empty
  c <- asks (getConst . buildCache Const)
  res <- runReaderT (runExceptT $ runBuildT $ mkTarget globalGoal) $ TransactionState m c
  finalize res
  where
    grubberScheduler :: forall y. Scheduler (BuildT k v m y) MonadRecipeGrub k v y
    grubberScheduler = scheduleBuild $ scheduleCoop $ schedulerExceptReader $ scheduleAsync scheduleResolver

    catchErrorInDep k err = BuildT $ catchE (runBuildT err) (throwE . DepFailed k)
    mkTarget = runSchedulerX (BuildT . throwE . NoRecipeFound) grubberScheduler catchErrorInDep recipes

    finalize (Left e) = onFailure e
    finalize (Right ok) = pure ok

type MonadRecipeGrubAux = '[MonadRestrictedIO, HasInternalOperations, FileReading, FileWriting, FileWritingAux]

-- | Supply the filepath the recipe is allowed to write to
supplyFileTarget :: FilePath -> Recipe MonadRecipeGrubAux k v x -> Recipe MonadRecipeGrub k v x
supplyFileTarget fp reci = recipe' $ \(resolv :: forall x. k x -> m (v x)) ->
  withInternalOps (Proxy :: Proxy m) $ reify fp $ \ps ->
    runRRT ps $ runResolver (ReflectingReaderT . resolv) (runRecipe reci)

supplyFileTargets :: (forall x. k x -> FilePath) -> RecipeBook MonadRecipeGrubAux k v -> RecipeBook MonadRecipeGrub k v
supplyFileTargets fps book arg = supplyFileTarget (fps arg) <$> book arg
