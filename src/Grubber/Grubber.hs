{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
module Grubber.Grubber
( GrubberConfig(grubberRunLifecycle)
, defaultGrubberCfg
, FailureReason(..)
, DependencyOuput(..)
, SupplyAuxInput(..)
, MonadRecipeGrub
, RecipeGrub
, RecipeBookGrub
, GrubberM
, runGrubber
, runGrubberDef
, build
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
import Data.Dependent.Map as DM hiding ((\\))
import Data.Constraint
import Data.Reflection
import Data.Proxy

import Grubber.Types
import Grubber.Blocking
import Grubber.Filesystem
import Grubber.Internal
import Grubber.MonadContext

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

type BuildResult k x = Either (FailureReason k x) (RecipeOutput x)

data TransactionValue k x
  = InFlight
  { _tvCompletion :: TMVar (BuildResult k x)
  }
  | Built
  { _tvResult :: BuildResult k x
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
  { tsBuiltTargets :: TVar (DMap k (TransactionValue k))
  , tsBuildCache :: BuildCache k v
  }

data AsyncResult f v m r = forall x. r ~ v x => AsyncResult (Async (StM (ExceptT f m) (v x)))

-- the monad to use to actually *run* a recipe in.
newtype RecipeEnvT x f k v m r = RecipeEnvT
  { runRecipeT ::
      ReaderT (k x) (
        BlockingListT (AsyncResult (f x) v m) (
          ExceptT (f x) m
        )
      ) r
  }
  deriving (Functor, Applicative, Monad, MonadRestrictedIO)

deriving instance MonadBase b m => MonadBase b (RecipeEnvT x f k v m)
deriving instance MonadBaseControl b m => MonadBaseControl b (RecipeEnvT x f k v m)

instance MonadBaseControl IO m => FileReading (RecipeEnvT x f k v m) where
  type FileReadToken (RecipeEnvT x f k v m) = GrubberReadToken
  withReadFile (GrubberReadToken fp) = withReadFileB fp \\ internalIO (Proxy :: Proxy (RecipeEnvT x f k v m)) 

instance MonadBaseControl IO m => FileWriting (RecipeEnvT x f k v m) where
  type FileWriteToken (RecipeEnvT x f k v m) = FilePath
  withWriteFile = withWriteFileB \\ internalIO (Proxy :: Proxy (RecipeEnvT x f k v m))
  toReadToken = return . GrubberReadToken

instance MonadBaseControl IO m => InternalOperations (RecipeEnvT x f k v m)
instance MonadBaseControl IO m => HasInternalOperations (RecipeEnvT x f k v m) where
  internalDict _ = Dict

instance (Monad m, SupplyAuxInput k m) => AccessAuxInput (RecipeEnvT x f k v m) x where
  getAuxInput = RecipeEnvT $ ReaderT $ \trgt -> lift $ lift $ supplyAuxInput trgt

instance ( Monad m, MonadBaseControl IO m, MonadRestrictedIO m, SupplyAuxInput k m )
  => GrubberPublicInterface (RecipeEnvT x f k v m) x

type UnwrappedRecipeT x f k v m = BlockingListT (AsyncResult (f x) v m) (ExceptT (f x) m)

scheduleCatchErrors :: MonadCatch m
                    => Scheduler (RecipeEnvT x (FailureReason k) k v m) e k v x
                    -> Scheduler (RecipeEnvT x (FailureReason k) k v m) e k v x
scheduleCatchErrors scheduleInner resolv target reci = RecipeEnvT $
  catch (runRecipeT $ scheduleInner resolv target reci) $ \e ->
    -- reevaluate if catching *all* exceptions is really the way to go
    lift $ lift $ throwE $ RuntimeError e

scheduleReadInputs :: Scheduler (RecipeEnvT x f k v m) e k v x
                   -> Scheduler (UnwrappedRecipeT x f k v m) e k v x
scheduleReadInputs scheduleInner resolveDep target reci =
  runReaderT (runRecipeT $ scheduleInner (\k -> RecipeEnvT $ ReaderT $ \_ {- == target -} -> resolveDep k) target reci) target

scheduleAsync :: forall e f k v m x.
                 (MonadRestrictedIO m, MonadBaseControl IO m, MonadCatch m)
              => Scheduler (UnwrappedRecipeT x f k v m) e k v x
              -> Scheduler (ExceptT (f x) m) e k v x
scheduleAsync scheduleInner resolveDep target reci =
  loopBlockingT unblockAsyncList (scheduleInner liftedResolve target reci)
  where
    liftedResolve :: forall y. k y -> UnwrappedRecipeT x f k v m (v y)
    liftedResolve key = do
      as <- lift .  lift $ liftBaseWith $ \unlift -> async (unlift $ runExceptT $ resolveDep key)
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

type SchedulerM k v m x = ReaderT (TransactionState k v) (ExceptT (FailureReason k x) m)

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
    Left (Built res)        -> lift $ ExceptT $ return res
    Left (InFlight promise) -> lift $ ExceptT $ liftBase . atomically $ readTMVar promise
    Right promise           -> controlT $ \runTs -> ExceptT $ do
      val <- runExceptT $ runTs (scheduleInner resolveDep target reci)
      liftBase . atomically $ putTMVar promise val
      return val

newtype BuildT k v m x r = BuildT { runBuildT :: SchedulerM k v m x r }

scheduleBuild :: Scheduler (SchedulerM k v m x) e k v y -> Scheduler (BuildT k v m x) e k v y
scheduleBuild = coerceScheduler

type GrubberPublicInterface :: forall k. (* -> *) -> k -> Constraint
class ( MonadRestrictedIO m
      , FileReading m
      , FileReadToken m ~ GrubberReadToken
      , AccessAuxInput m x
      ) => GrubberPublicInterface m x

type MonadRecipeGrub :: forall k. ((* -> *) -> Constraint, (* -> *) -> k -> Constraint)
type MonadRecipeGrub = ('(,) HasInternalOperations GrubberPublicInterface)

-- a trick I learned from https://www.reddit.com/r/haskell/comments/mkh6iz/
-- explicitly annotating the right side implicitly quantifies over the kind variables mentioned there
-- and thus allows to use kk in the explicit annotation of MonadRecipeGrub
type RecipeGrub :: forall kk k2. (k2 -> *) -> (k2 -> *) -> kk -> *
type RecipeGrub = Recipe (MonadRecipeGrub @kk) :: (k2 -> *) -> (k2 -> *) -> kk -> *
type RecipeBookGrub :: forall k. (k -> *) -> (k -> *) -> *
type RecipeBookGrub (k :: kk -> *) v = RecipeBook (MonadRecipeGrub @kk) k v

class SupplyAuxInput k m where
  supplyAuxInput :: k x -> m (AuxInput x)

-- | Class of values that can be produced from recipe outputs.
-- For example, even though recipes for files don't return the written file content,
-- depending on them will return a FileReadToken.
class DependencyOuput k v m where
  fromRecipeOutput :: k x -> RecipeOutput x -> m (v x)

build :: forall e kk (k :: kk -> *) v m.
      ( MonadRestrictedIO m
      , MonadBaseControl IO m
      , MonadCatch m
      , MonadReader e m
      , HasBuildCache k v e
      , GCompare k
      , SupplyAuxInput k m
      , DependencyOuput k v m )
      => (forall x. FailureReason k x -> m (RecipeOutput x))
      -> Build m (MonadRecipeGrub @kk) k v
build onFailure recipes globalGoal = do
  m <- liftBase $ newTVarIO empty
  c <- asks (getConst . buildCache Const)
  res <- runExceptT $ runReaderT (runBuildT $ mkTarget globalGoal) $ TransactionState m c
  finalize res
  where
    grubberScheduler :: forall y. Scheduler (BuildT k v m y) (MonadRecipeGrub @kk) k v y
    grubberScheduler = scheduleBuild $ scheduleCoop $ scheduleReader $ scheduleAsync $ scheduleReadInputs $ scheduleCatchErrors scheduleResolver

    catchErrorInDep :: forall x y. k y -> BuildT k v m y (RecipeOutput y) -> BuildT k v m x (v y)
    catchErrorInDep k (BuildT inner) = BuildT $ controlT $ \runTs ->
      catchE (runTs inner >>= lift . fromRecipeOutput k) (throwE . DepFailed k)

    mkTarget = runSchedulerX (BuildT . lift . throwE . NoRecipeFound) catchErrorInDep grubberScheduler recipes

    finalize (Left e) = onFailure e
    finalize (Right ok) = pure ok
