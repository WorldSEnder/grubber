{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

import Test.Tasty
import Test.Tasty.HUnit (testCase)
import Test.HUnit

import Control.Monad.Base
import Control.Concurrent (threadDelay)
import Data.IORef
import Data.GADT.Compare.TH
import Data.Dependent.Map as DM
import Data.Dependent.Sum (DSum((:=>)))

import Grubber.Blocking
import Grubber.Types
import Grubber.Grubber
--import Grubber.Filesystem

import Control.Concurrent.MVar (newMVar, withMVar, MVar)
--import System.IO (hShow)
import System.IO.Unsafe (unsafePerformIO)

data Dependency a where
  SomeDep :: Integer -> Dependency Integer

data Result a where
  SomeResult :: a -> Result a

testRecipe :: IORef Bool -> Recipe (MonadBase IO) Dependency Result Integer
testRecipe signalOk = recipe do
  ~(SomeResult x) <- resolve $ SomeDep 4
  -- both dependencies should be blocked on *before* effectfully writing to the IORef
  -- depends on ApplicativeDo doing the right thing.
  liftBase $ writeIORef signalOk False
  ~(SomeResult y) <- resolve $ SomeDep 5
  return $ SomeResult $ x + y

mockResolve :: Dependency a -> BlockingListT Dependency IO (Result a)
mockResolve dep@(SomeDep _) = block (SomeResult <$> singletonF dep)

decodeList :: TaskList Dependency x -> [Integer]
decodeList = runCS . elimTaskList alg where
  alg :: Dependency a -> ConstSemigroupoid [Integer] a
  alg (SomeDep x) = ConstSemigroupoid [x]

getBlockers :: Either z (Blocker (TaskList Dependency) m x) -> IO [Integer]
getBlockers (Left _) = assertFailure "expected computation to be blocked"
getBlockers (Right (Blocker l _)) = pure $ decodeList l

blockingTests :: TestTree
blockingTests = testGroup "'Blocking' tests"
  [ testCase "recipe blocks on two deps" $ do
      signalOk <- newIORef True
      blocks <- runBlockingT (runResolver mockResolve $ runRecipe $ testRecipe signalOk) >>= getBlockers
      blocks @?= [4, 5]
      -- We want to, as early as possible, start co-runnable dependencies.
      -- Hence, block asap before running other side effects.
      readIORef signalOk @? "should not write to the IORef before blocking"
  ]

data DiamondTag a where
  DBot, DLeft, DRight, DTop :: DiamondTag ()

$(deriveGEq ''DiamondTag)
$(deriveGCompare ''DiamondTag)

data DiamondTestEnv
  = DiamondTestEnv
  { counterTop :: IORef Int
  , counterLeft :: IORef Int
  , counterRight :: IORef Int
  , counterBot :: IORef Int
  }

buildExample :: DiamondTestEnv -> DiamondTag x -> GrubberM DiamondTag Result (Result x)
buildExample env = build onFailure (`DM.lookup` rules) where
  rules :: DMap DiamondTag (Recipe MonadRecipeGrub DiamondTag Result)
  rules = DM.fromList
    [ DBot   :=> recipe do
        liftOptionalIO $ modifyIORef' (counterBot env) (+ 1)
        return $ SomeResult ()
    , DLeft  :=> recipe do
        liftOptionalIO $ modifyIORef' (counterLeft env) (+ 1)
        ~(SomeResult _) <- resolve DBot
        return $ SomeResult ()
    , DRight :=> recipe do
        liftOptionalIO $ modifyIORef' (counterRight env) (+ 1)
        ~(SomeResult _) <- resolve DLeft
        ~(SomeResult _) <- resolve DBot
        return $ SomeResult ()
    , DTop   :=> recipe do
        liftOptionalIO $ modifyIORef' (counterTop env) (+ 1)
        ~(SomeResult _) <- resolve DRight
        ~(SomeResult _) <- resolve DLeft
        return $ SomeResult ()
    ]

onFailure :: FailureReason k x -> GrubberM DiamondTag Result (Result x)
onFailure _ = liftBase $ assertFailure "shouldn't fail to run"

globalPutStrLock :: MVar ()
globalPutStrLock = unsafePerformIO $ newMVar ()
{-# NOINLINE globalPutStrLock #-}

atomicPutStrLn :: String -> IO ()
atomicPutStrLn str = withMVar globalPutStrLock $ \_ -> putStrLn str

_delayedExample :: DiamondTag x -> GrubberM DiamondTag Result (Result x)
_delayedExample = build onFailure (`DM.lookup` delayedRules) where
  delayedRules :: DMap DiamondTag (Recipe MonadRecipeGrub DiamondTag Result)
  delayedRules = DM.fromList
    [ DBot    :=> recipe do
        liftOptionalIO $ atomicPutStrLn "start bot" >> threadDelay 2000000 >> atomicPutStrLn "end bot"
        return $ SomeResult ()
    , DLeft   :=> recipe do
        liftOptionalIO $ atomicPutStrLn "start left" >> threadDelay 3000000 >> atomicPutStrLn "end left"
        return $ SomeResult ()
    , DRight  :=> recipe do
        liftOptionalIO $ atomicPutStrLn "start right"
        ~(SomeResult b) <- resolve DBot
        ~(SomeResult l) <- resolve DLeft
        b `seq` l `seq` liftOptionalIO $ threadDelay 2000000 >> atomicPutStrLn "end right"
        return $ SomeResult ()
    , DTop   :=> recipe do
        liftOptionalIO $ atomicPutStrLn "start top"
        ~(SomeResult b) <- resolve DBot
        ~(SomeResult l) <- b `seq` resolve DLeft
        ~(SomeResult r) <- l `seq` resolve DRight
        --withReadFile _ $ \hdl -> liftOptionalIO $ hShow hdl >>= putStrLn
        r `seq` liftOptionalIO $ threadDelay 1000000 >> atomicPutStrLn "end top"
        return $ SomeResult ()
    ]

grubberSpecCfg :: GrubberConfig
grubberSpecCfg = defaultGrubberCfg
  { grubberRunLifecycle = True
  }

grubberTests :: TestTree
grubberTests = testGroup "Grubber tests"
  [ testCase "diamond test case" $ do
      rTop <- newIORef 0 ; rLeft <- newIORef 0 ; rRight <- newIORef 0 ; rBot <- newIORef 0
      let cnts = DiamondTestEnv rTop rLeft rRight rBot
      runGrubber grubberSpecCfg $ buildExample cnts DTop
      (== 1) <$> readIORef rTop @? "top should have run once"
      (== 1) <$> readIORef rLeft @? "left should have run once"
      (== 1) <$> readIORef rRight @? "right should have run once"
      (== 1) <$> readIORef rBot @? "bot should have run once"
  ]

main :: IO ()
main = defaultMain $ testGroup "functionality tests"
  [ blockingTests
  , grubberTests
  ]
