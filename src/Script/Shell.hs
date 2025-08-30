module Script.Shell (
    ShellScript(..),
    ShellStatement(..),
    ShellPipeline(..),
    ShellCommand(..),
    withShellProcess,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable

import System.Exit
import System.IO
import System.Posix.IO qualified as P
import System.Posix.Types
import System.Process hiding (ShellCommand)

import Network
import Network.Ip
import Output
import Process
import Run.Monad
import Script.Var


newtype ShellScript = ShellScript [ ShellStatement ]

data ShellStatement = ShellStatement
    { shellPipeline :: ShellPipeline
    , shellSourceLine :: SourceLine
    }

data ShellPipeline = ShellPipeline
    { pipeCommand :: ShellCommand
    , pipeUpstream :: Maybe ShellPipeline
    }

data ShellCommand = ShellCommand
    { cmdCommand :: Text
    , cmdArguments :: [ Text ]
    , cmdSourceLine :: SourceLine
    }


data ShellExecInfo = ShellExecInfo
    { seiNode :: Node
    , seiProcName :: ProcName
    , seiStatusVar :: MVar ExitCode
    }


data HandleHandling
    = CloseHandle Handle
    | KeepHandle Handle

closeIfRequested :: MonadIO m => HandleHandling -> m ()
closeIfRequested (CloseHandle h) = liftIO $ hClose h
closeIfRequested (KeepHandle _) = return ()

handledHandle :: HandleHandling -> Handle
handledHandle (CloseHandle h) = h
handledHandle (KeepHandle h) = h


executeCommand :: ShellExecInfo -> HandleHandling -> HandleHandling -> HandleHandling -> ShellCommand -> TestRun ()
executeCommand ShellExecInfo {..} pstdin pstdout pstderr ShellCommand {..} = do
    case cmdCommand of
        "echo" -> liftIO $ do
            T.hPutStrLn (handledHandle pstdout) $ T.intercalate " " cmdArguments
            hFlush (handledHandle pstdout)
            mapM_ closeIfRequested [ pstdin, pstdout, pstderr ]
        cmd -> do
            (_, _, _, phandle) <- liftIO $ createProcess_ "shell"
                (proc (T.unpack cmd) (map T.unpack cmdArguments))
                    { std_in = UseHandle $ handledHandle pstdin
                    , std_out = UseHandle $ handledHandle pstdout
                    , std_err = UseHandle $ handledHandle pstderr
                    , cwd = Just (nodeDir seiNode)
                    , env = Just []
                    }
            mapM_ closeIfRequested [ pstdin, pstdout, pstderr ]
            liftIO (waitForProcess phandle) >>= \case
                ExitSuccess -> return ()
                status -> do
                    outLine OutputChildFail (Just $ textProcName seiProcName) $ "failed at: " <> textSourceLine cmdSourceLine
                    liftIO $ putMVar seiStatusVar status
                    throwError Failed

executePipeline :: ShellExecInfo -> HandleHandling -> HandleHandling -> HandleHandling -> ShellPipeline -> TestRun ()
executePipeline sei pstdin pstdout pstderr ShellPipeline {..} = do
    case pipeUpstream of
        Nothing -> do
            executeCommand sei pstdin pstdout pstderr pipeCommand

        Just upstream -> do
            ( pipeRead, pipeWrite ) <- createPipeCloexec
            void $ forkTestUsing forkOS $ do
                executePipeline sei pstdin (CloseHandle pipeWrite) (KeepHandle $ handledHandle pstderr) upstream

            executeCommand sei (CloseHandle pipeRead) pstdout (KeepHandle $ handledHandle pstderr) pipeCommand
            closeIfRequested pstderr

executeScript :: ShellExecInfo -> Handle -> Handle -> Handle -> ShellScript -> TestRun ()
executeScript sei@ShellExecInfo {..} pstdin pstdout pstderr (ShellScript statements) = do
    setNetworkNamespace $ getNetns seiNode
    forM_ statements $ \ShellStatement {..} -> do
        executePipeline sei (KeepHandle pstdin) (KeepHandle pstdout) (KeepHandle pstderr) shellPipeline
    liftIO $ putMVar seiStatusVar ExitSuccess

spawnShell :: Node -> ProcName -> ShellScript -> TestRun Process
spawnShell procNode procName script = do
    procOutput <- liftIO $ newTVarIO []
    procIgnore <- liftIO $ newTVarIO ( 0, [] )
    seiStatusVar <- liftIO $ newEmptyMVar
    ( pstdin, procStdin ) <- createPipeCloexec
    ( hout, pstdout ) <- createPipeCloexec
    ( herr, pstderr ) <- createPipeCloexec
    procHandle <- fmap (Right . (, seiStatusVar)) $ forkTestUsing forkOS $ do
        let seiNode = procNode
            seiProcName = procName
        executeScript ShellExecInfo {..} pstdin pstdout pstderr script
        liftIO $ do
            hClose pstdin
            hClose pstdout
            hClose pstderr

    let procKillWith = Nothing
    let process = Process {..}

    startProcessIOLoops process hout herr
    return process

withShellProcess :: Node -> ProcName -> ShellScript -> (Process -> TestRun a) -> TestRun a
withShellProcess node pname script inner = do
    procVar <- asks $ teProcesses . fst

    process <- spawnShell node pname script
    liftIO $ modifyMVar_ procVar $ return . (process:)

    inner process `finally` do
        ps <- liftIO $ takeMVar procVar
        closeTestProcess process `finally` do
            liftIO $ putMVar procVar $ filter (/=process) ps


foreign import ccall "shell_pipe_cloexec" c_pipe_cloexec :: Ptr Fd -> IO CInt

createPipeCloexec :: (MonadIO m, MonadFail m) => m ( Handle, Handle )
createPipeCloexec = liftIO $ do
    allocaArray 2 $ \ptr -> do
        c_pipe_cloexec ptr >>= \case
            0 -> do
                rh <- P.fdToHandle =<< peekElemOff ptr 0
                wh <- P.fdToHandle =<< peekElemOff ptr 1
                return ( rh, wh )
            _ -> do
                fail $ "failed to create pipe"
