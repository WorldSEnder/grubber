module Grubber.ResourceT
( module R
, runResourceT
) where

import Control.Monad.Catch
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource as R hiding (runResourceT)
import Control.Monad.Trans.Resource.Internal

runResourceT :: MonadBaseControl IO m => ResourceT m a -> m a
runResourceT (ResourceT r) = control $ \run -> do
    istate <- createInternalState
    mask $ \restore -> do
        res <- restore (run (r istate)) `catch` \e -> do
            stateCleanupChecked (Just e) istate
            throwM e
        stateCleanupChecked Nothing istate
        return res
