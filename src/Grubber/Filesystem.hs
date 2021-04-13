{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module Grubber.Filesystem
( FileReading(..)
, withReadFileB
, withWriteFileB
, FileWriting(..)
) where

import System.IO
import Control.Monad.Reader
import Control.Monad.Trans.Control

import Grubber.Types

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
  fwPutStr :: Handle -> String -> m ()
  toReadToken :: FileWriteToken m -> m (FileReadToken m)
