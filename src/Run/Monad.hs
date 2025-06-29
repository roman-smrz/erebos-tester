module Run.Monad (
    TestRun(..),
    TestEnv(..),
    TestState(..),
    TestOptions(..), defaultTestOptions,
    Failed(..),

    finally,
    forkTest,
    forkTestUsing,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Map (Map)
import Data.Scientific
import Data.Set (Set)
import Data.Text qualified as T

import {-# SOURCE #-} GDB
import Network.Ip
import Output
import {-# SOURCE #-} Process
import Script.Expr
import Script.Object

newtype TestRun a = TestRun { fromTestRun :: ReaderT (TestEnv, TestState) (ExceptT Failed (WriterT [ SomeObject TestRun ] IO)) a }
  deriving
    ( Functor, Applicative, Monad
    , MonadReader ( TestEnv, TestState )
    , MonadWriter [ SomeObject TestRun ]
    , MonadIO
    )

data TestEnv = TestEnv
    { teOutput :: Output
    , teFailed :: TVar (Maybe Failed)
    , teOptions :: TestOptions
    , teNextObjId :: MVar Int
    , teProcesses :: MVar [ Process ]
    , teTimeout :: MVar Scientific
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

forkTest :: TestRun () -> TestRun ThreadId
forkTest = forkTestUsing forkIO

forkTestUsing :: (IO () -> IO ThreadId) -> TestRun () -> TestRun ThreadId
forkTestUsing fork act = do
    tenv <- ask
    liftIO $ fork $ do
        ( res, [] ) <- runWriterT (runExceptT $ flip runReaderT tenv $ fromTestRun act)
        case res of
            Left e -> atomically $ writeTVar (teFailed $ fst tenv) (Just e)
            Right () -> return ()
