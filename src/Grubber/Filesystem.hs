{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module Grubber.Filesystem
( ReflectingReaderT(..)
, runRRT
, MonadReadAux(..)
, FileReading(..)
, withReadFileB
, withWriteFileB
, FileWriting(..)
, FileWritingAux
) where

import System.IO
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Trans.Control
import Data.Reflection
import Data.Proxy

import Grubber.Types ( MonadRestrictedIO, WithResolverT(..), runResolver, Resolver(..) )

-- newtype FileReadToken = FileReadToken FilePath

newtype ReflectingReaderT aux s m a = ReflectingReaderT { _runRRT :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadRestrictedIO, FileReading, FileWriting)

deriving instance MonadBase b m => MonadBase b (ReflectingReaderT uax s m)
deriving instance MonadBaseControl b m => MonadBaseControl b (ReflectingReaderT uax s m)

runRRT :: proxy s -> ReflectingReaderT aux s m a -> m a
runRRT _ = _runRRT

-- These instances need UndecidableInstances,
-- because they do not satisfy the coverage condition.
deriving instance MonadReader r m => MonadReader r (ReflectingReaderT aux s m)
deriving instance MonadState r m => MonadState r (ReflectingReaderT aux s m)

class MonadReadAux r m | m -> r where
  askAux :: m r

instance (Monad m, Reifies s aux) => MonadReadAux aux (ReflectingReaderT aux s m) where
  askAux = ReflectingReaderT $ pure $ reflect (Proxy :: Proxy s)

class FileReading m where
  type FileReadToken m
  withReadFile :: FileReadToken m -> (Handle -> m r) -> m r

withFileB :: MonadBaseControl IO m => FilePath -> IOMode -> (Handle -> m b) -> m b
withFileB fp mode hdl = control $ \runInBase -> withBinaryFile fp mode $ runInBase . hdl

withReadFileB :: MonadBaseControl IO m => FilePath -> (Handle -> m b) -> m b
withReadFileB fp = withFileB fp ReadMode

withWriteFileB :: MonadBaseControl IO m => FilePath -> (Handle -> m b) -> m b
withWriteFileB fp = withFileB fp WriteMode

instance (Monad m, FileReading m) => FileReading (WithResolverT k v m) where
  type FileReadToken (WithResolverT k v m) = FileReadToken m
  withReadFile fp hdl = WithResolver $ ReaderT $ \(Resolver r) -> withReadFile fp (runResolver r . hdl)

class FileWriting m where
  -- By quantifying the token, it can not be returned from recipes (without additional constraints circumventing this)
  -- since those are universally quantified.
  -- A simple constraint circumventing is an equality constraint, fixing @m@.
  -- Anyway, the only way to access a token should thus be to get one externally.
  type FileWriteToken m
  withWriteFile :: FileWriteToken m -> (Handle -> m r) -> m r
  toReadToken :: FileWriteToken m -> m (FileReadToken m)

class (FileWriting m, MonadReadAux (FileWriteToken m) m) => FileWritingAux m
instance (FileWriting m, MonadReadAux (FileWriteToken m) m) => FileWritingAux m
