{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
module Grubber.Blocking
( BlockingT(..)
, BlockingListT
, MonadBlockingT(..)
, Blocker(..)
, runBlockingT
, loopBlockingT
, Semigroupal(..)
, TaskList
, singletonF
, hoistTaskList
, elimTaskList
, elimTaskListA
, ConstSemigroupoid(..)
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Monad.Catch as MC

class Functor f => Semigroupal f where
  (<<>>) :: f a -> f b -> f (a, b)
  default (<<>>) :: Applicative f => f a -> f b -> f (a, b)
  (<<>>) = liftA2 (,)
  dayMap :: f a -> f b -> (a -> b -> x) -> f x
  dayMap fa fb c = uncurry c <$> (fa <<>> fb)

-- | BlockingT allows to be explicit in the type of problems 'w', that might
-- require monadic computations in 'm' to run, before a result can be computed,
-- and collecting them into a list
data BlockingT w m a
  = Pure a
  | forall b. BlockedOn !(w b) (m b -> BlockingT w m a)
  | Effect (forall x. (BlockingT w m a -> m x) -> m x)

type BlockingListT w = BlockingT (TaskList w)

class Applicative m => MonadBlockingT w m where
  block :: w a -> m a
  blockAll :: TaskList w a -> m a
  blockAll = elimTaskListA block

instance (Semigroupal w, Monad m) => MonadBlockingT w (BlockingT w m) where
  block wa = BlockedOn wa $ \ma -> Effect $ \c -> ma >>= c . Pure

instance Monad m => MonadBlockingT w (BlockingT (TaskList w) m) where
  block wa = block $ singletonF wa

data Blocker w m a = forall b. Blocker !(w b) (m b -> BlockingT w m a)

runBlockingT :: Monad m => BlockingT w m a -> m (Either a (Blocker w m a))
runBlockingT (Pure a) = return $ Left a
runBlockingT (BlockedOn w c) = return $ Right $ Blocker w c
runBlockingT (Effect c) = c runBlockingT

loopBlockingT :: Monad m => (forall b. w b -> m b) -> BlockingT w m a -> m a
loopBlockingT unblock action = runBlockingT action >>= loop where
  loop (Left res) = return res
  loop (Right (Blocker blocker cont)) = runBlockingT (cont $ unblock blocker) >>= loop

instance Functor m => Functor (BlockingT w m) where
  fmap f (Pure a) = Pure (f a)
  fmap f (BlockedOn w cont) = BlockedOn w $ fmap f . cont
  fmap f (Effect ma) = Effect $ \x -> ma (x . fmap f)

instance (Applicative m, Semigroupal w) => Applicative (BlockingT w m) where
  pure = Pure

  Pure f             <*> Pure a             = Pure (f a)
  BlockedOn w1 c1    <*> BlockedOn w2 c2    =
    BlockedOn (w1 <<>> w2) $ \mww -> c1 (fst <$> mww) <*> c2 (snd <$> mww)
  Effect mf          <*> Effect ma          =
    Effect $ \x -> mf $ \bf -> ma $ \ba -> x (bf <*> ba)

  BlockedOn w c      <*> ba                 = BlockedOn w $ \b -> c b <*> ba
  bf                 <*> BlockedOn w c      = BlockedOn w $ \b -> bf <*> c b

  bf                 <*> Effect ma = Effect $ \x -> ma $ \ba -> x $ bf <*> ba
  Effect mf          <*> ba        = Effect $ \x -> mf $ \bf -> x $ bf <*> ba

instance (Applicative m, Semigroupal w) => Monad (BlockingT w m) where
  Pure a >>= f = f a
  BlockedOn w c >>= f = BlockedOn w $ c >=> f
  Effect ma >>= f = Effect $ \x -> ma $ \ba -> x (ba >>= f)

instance (MonadFail m, Semigroupal w) => MonadFail (BlockingT w m) where
  fail = lift . fail

instance (Semigroupal w) => MonadTrans (BlockingT w) where
  lift ma = Effect $ \x -> ma >>= x . Pure

instance (MonadBase b m, Semigroupal w) => MonadBase b (BlockingT w m) where
  liftBase = lift . liftBase

type RunBlocking w = forall n b. Monad n => BlockingT w n b -> n (Either b (Blocker w n b))

liftBlockingWith :: (Semigroupal w, Monad m)
                 => (RunBlocking w -> m a) -> BlockingT w m a
liftBlockingWith f = lift $ f runBlockingT

-- compare to 'Control.Monad.Trans.Control.restoreT'
restoreBlockingT :: (Semigroupal w, Monad m) => Either a (Blocker w m a) -> BlockingT w m a
restoreBlockingT (Left res) = Pure res
restoreBlockingT (Right (Blocker blocker cont)) = BlockedOn blocker cont

controlBlockingT :: (Semigroupal w, Monad m)
                 => (RunBlocking w -> m (Either a (Blocker w m a))) -> BlockingT w m a
controlBlockingT f = liftBlockingWith f >>= restoreBlockingT

instance (MonadBaseControl b m, Semigroupal w) => MonadBaseControl b (BlockingT w m) where
  type StM (BlockingT w m) a = StM m (Either a (Blocker w m a))
  liftBaseWith f = lift $ liftBaseWith $ \runner -> f (runner . runBlockingT)
  restoreM s = lift (restoreM s) >>= restoreBlockingT

instance (MonadThrow m, Semigroupal w) => MonadThrow (BlockingT w m) where
  throwM = lift . throwM

instance (MonadCatch m, Semigroupal w) => MonadCatch (BlockingT w m) where
  catch act hdlr = controlBlockingT $ \run -> MC.catch (run act) (run . hdlr)

data TaskList f x where
  Single :: f a -> (a -> x) -> TaskList f x
  Multiple :: TaskList f a -> TaskList f b -> (a -> b -> x) -> TaskList f x

instance Functor (TaskList f) where
  fmap f (Single fa c) = Single fa (f . c)
  fmap f (Multiple fa fb c) = Multiple fa fb (\a b -> f $ c a b)

instance Semigroupal (TaskList f) where
  tl <<>> tr = Multiple tl tr (,)

singletonF :: f a -> TaskList f a
singletonF faf = Single faf id

hoistTaskList :: (forall x. f x -> g x) -> TaskList f a -> TaskList g a
hoistTaskList h (Single f c) = Single (h f) c
hoistTaskList h (Multiple l r c) = Multiple (hoistTaskList h l) (hoistTaskList h r) c

elimTaskList :: Semigroupal g => (forall x. f x -> g x) -> TaskList f a -> g a
elimTaskList e (Single f x) = x <$> e f
elimTaskList e (Multiple fa fb c) = dayMap (elimTaskList e fa) (elimTaskList e fb) c

newtype SemigroupalApplicative g a = SemigroupalApplicative { runSA :: g a }
  deriving Functor

instance Applicative g => Semigroupal (SemigroupalApplicative g) where
  fa <<>> fb = SemigroupalApplicative $ liftA2 (,) (runSA fa) (runSA fb)

elimTaskListA :: Applicative g => (forall x. f x -> g x) -> TaskList f a -> g a
elimTaskListA e tl = runSA $ elimTaskList (SemigroupalApplicative . e) tl

newtype ConstSemigroupoid w a = ConstSemigroupoid { runCS :: w }
  deriving Functor

instance Semigroup w => Semigroupal (ConstSemigroupoid w) where
  a <<>> b = ConstSemigroupoid $ runCS a <> runCS b

instance Monoid w => Applicative (ConstSemigroupoid w) where
  pure _ = ConstSemigroupoid mempty
  a <*> b = ConstSemigroupoid $ runCS a <> runCS b
