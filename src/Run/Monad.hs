module Run.Monad (
    TestRun(..),
    TestEnv(..),
    TestState(..),
    TestOptions(..), defaultTestOptions,
    Failed(..),

    finally,
    forkTest,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.Map (Map)
import Data.Scientific
import Data.Set (Set)
import Data.Text qualified as T

import {-# SOURCE #-} GDB
import Network.Ip
import Output
import {-# SOURCE #-} Process
import Test

newtype TestRun a = TestRun { fromTestRun :: ReaderT (TestEnv, TestState) (ExceptT Failed IO) a }
    deriving (Functor, Applicative, Monad, MonadReader (TestEnv, TestState), MonadIO)

data TestEnv = TestEnv
    { teOutput :: Output
    , teFailed :: TVar (Maybe Failed)
    , teOptions :: TestOptions
    , teProcesses :: MVar [Process]
    , teGDB :: Maybe (MVar GDB)
    }

data TestState = TestState
    { tsGlobals :: GlobalDefs
    , tsLocals :: [ ( VarName, SomeVarValue ) ]
    , tsDisconnectedUp :: Set NetworkNamespace
    , tsDisconnectedBridge :: Set NetworkNamespace
    , tsNodePacketLoss :: Map NetworkNamespace Scientific
    }

data TestOptions = TestOptions
    { optDefaultTool :: String
    , optProcTools :: [(ProcName, String)]
    , optTestDir :: FilePath
    , optTimeout :: Scientific
    , optGDB :: Bool
    , optForce :: Bool
    , optKeep :: Bool
    , optWait :: Bool
    }

defaultTestOptions :: TestOptions
defaultTestOptions = TestOptions
    { optDefaultTool = ""
    , optProcTools = []
    , optTestDir = ".test"
    , optTimeout = 1
    , optGDB = False
    , optForce = False
    , optKeep = False
    , optWait = False
    }

data Failed = Failed
            | ProcessCrashed Process

instance MonadFail TestRun where
    fail str = do
        outLine OutputError Nothing $ T.pack str
        throwError Failed

instance MonadError Failed TestRun where
    throwError failed = do
        failedVar <- asks $ teFailed . fst
        liftIO $ atomically $ modifyTVar failedVar (`mplus` Just failed)

        te <- asks fst
        case failed of
            ProcessCrashed _ | Just mgdb <- teGDB te -> do
                maybe (return ()) gdbSession =<< liftIO (tryTakeMVar mgdb)
            _ -> return ()

        TestRun $ throwError failed

    catchError (TestRun act) handler = TestRun $ catchError act $ fromTestRun . handler

instance MonadEval TestRun where
    askGlobalDefs = asks (tsGlobals . snd)
    askDictionary = asks (tsLocals . snd)
    withDictionary f = local (fmap $ \s -> s { tsLocals = f (tsLocals s) })

instance MonadOutput TestRun where
    getOutput = asks $ teOutput . fst

instance MonadPIO TestRun where
    postpone = liftIO


finally :: MonadError e m => m a -> m b -> m a
finally act handler = do
    x <- act `catchError` \e -> handler >> throwError e
    void handler
    return x

forkTest :: TestRun () -> TestRun ()
forkTest act = do
    tenv <- ask
    void $ liftIO $ forkIO $ do
        runExceptT (flip runReaderT tenv $ fromTestRun act) >>= \case
            Left e -> atomically $ writeTVar (teFailed $ fst tenv) (Just e)
            Right () -> return ()
