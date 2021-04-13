{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module Grubber.Filesystem
( FileReading(..)
, defWithReadBinaryFile
, defWithWriteBinaryFile
, FileWriting(..)
) where

import System.IO
import Control.Monad.Reader
import Control.Monad.Trans.Control

import Grubber.Types

class FileReading m where
  type FileReadToken m
  withReadBinaryFile :: FileReadToken m -> (Handle -> m r) -> m r

withFileB :: MonadBaseControl IO m => FilePath -> IOMode -> (Handle -> m b) -> m b
withFileB fp mode hdl = control $ \runInBase -> withBinaryFile fp mode $ runInBase . hdl

defWithReadBinaryFile :: MonadBaseControl IO m => FilePath -> (Handle -> m b) -> m b
defWithReadBinaryFile fp = withFileB fp ReadMode

defWithWriteBinaryFile :: MonadBaseControl IO m => FilePath -> (Handle -> m b) -> m b
defWithWriteBinaryFile fp = withFileB fp WriteMode

instance (Monad m, FileReading m) => FileReading (WithResolverT k v m) where
  type FileReadToken (WithResolverT k v m) = FileReadToken m
  withReadBinaryFile fp hdl = WithResolver $ ReaderT $ \(Resolver r) -> withReadBinaryFile fp (runResolver r . hdl)

class FileWriting m where
  -- By quantifying the token, it can not be returned from recipes (without additional constraints circumventing this)
  -- since those are universally quantified.
  -- A simple constraint circumventing is an equality constraint, fixing @m@.
  -- Anyway, the only way to access a token should thus be to get one externally.
  type FileWriteToken m
  withWriteBinaryFile :: FileWriteToken m -> (Handle -> m r) -> m r
  fwPutStr :: Handle -> String -> m ()
  toReadToken :: FileWriteToken m -> m (FileReadToken m)
