{-# LANGUAGE UndecidableInstances #-}
module Grubber.Internal
( HasInternalOperations(..)
, GrubberReadToken(..)
, InternalOperations
, withInternalOps
, internalIO
, internalFilewriting
, filesystemType
) where

import Control.Monad.Trans.Control
import Data.Constraint

import Grubber.Filesystem

newtype GrubberReadToken = GrubberReadToken FilePath
class
  ( MonadBaseControl IO m
  , FileWriting m
  , FileWriteToken m ~ FilePath
  , FileReadToken m ~ GrubberReadToken
  ) => InternalOperations m

class HasInternalOperations m where
  internalDict :: proxy m -> Dict (InternalOperations m)

withInternalOps :: HasInternalOperations m => proxy m -> (InternalOperations m => r) -> r
withInternalOps (p :: proxy m) = withDict (internalDict p)

internalIO :: HasInternalOperations m => proxy m -> Dict (MonadBaseControl IO m)
internalIO p = withInternalOps p Dict

internalFilewriting :: HasInternalOperations m => proxy m -> Dict (FileWriting m)
internalFilewriting p = withInternalOps p Dict

filesystemType :: HasInternalOperations m => proxy m -> Dict (FileWriteToken m ~ FilePath)
filesystemType p = withInternalOps p Dict
