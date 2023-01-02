module Run.Monad (
    TestRun(..),
    TestEnv(..),
    TestState(..),
    TestOptions(..), defaultTestOptions,
    Failed(..),
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.Map (Map)
import Data.Scientific
import qualified Data.Text as T

import {-# SOURCE #-} GDB
import {-# SOURCE #-} Network
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
    { tsNetwork :: Network
    , tsVars :: [(VarName, SomeVarValue)]
    , tsNodePacketLoss :: Map NodeName Scientific
    }

data TestOptions = TestOptions
    { optDefaultTool :: String
    , optProcTools :: [(ProcName, String)]
    , optTestDir :: FilePath
    , optTimeout :: Scientific
    , optGDB :: Bool
    , optForce :: Bool
    }

defaultTestOptions :: TestOptions
defaultTestOptions = TestOptions
    { optDefaultTool = ""
    , optProcTools = []
    , optTestDir = ".test"
    , optTimeout = 1
    , optGDB = False
    , optForce = False
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
    lookupVar name = maybe (fail $ "variable not in scope: '" ++ unpackVarName name ++ "'") return =<< asks (lookup name . tsVars . snd)
    rootNetwork = asks $ tsNetwork . snd

instance MonadOutput TestRun where
    getOutput = asks $ teOutput . fst
