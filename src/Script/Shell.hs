module Script.Shell (
    ShellStatement(..),
    ShellScript(..),
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

import System.Exit
import System.IO
import System.Process hiding (ShellCommand)

import Network
import Output
import Process
import Run.Monad


data ShellStatement = ShellStatement
    { shellCommand :: Text
    , shellArguments :: [ Text ]
    }

newtype ShellScript = ShellScript [ ShellStatement ]


executeScript :: Node -> ProcName -> Handle -> Handle -> Handle -> ShellScript -> TestRun ()
executeScript node pname pstdin pstdout pstderr (ShellScript statements) = do
    forM_ statements $ \ShellStatement {..} -> case shellCommand of
        "echo" -> liftIO $ do
            T.hPutStrLn pstdout $ T.intercalate " " shellArguments
            hFlush pstdout
        cmd -> do
            (_, _, _, phandle) <- liftIO $ createProcess_ "shell"
                (proc (T.unpack cmd) (map T.unpack shellArguments))
                    { std_in = UseHandle pstdin
                    , std_out = UseHandle pstdout
                    , std_err = UseHandle pstderr
                    , cwd = Just (nodeDir node)
                    , env = Just []
                    }
            liftIO (waitForProcess phandle) >>= \case
                ExitSuccess -> return ()
                ExitFailure code -> do
                    outLine OutputChildFail (Just $ textProcName pname) $ T.pack $ "exit code: " ++ show code
                    throwError Failed

spawnShell :: Node -> ProcName -> ShellScript -> TestRun Process
spawnShell procNode procName script = do
    procOutput <- liftIO $ newTVarIO []
    statusVar <- liftIO $ newEmptyMVar
    ( pstdin, procStdin ) <- liftIO $ createPipe
    ( hout, pstdout ) <- liftIO $ createPipe
    ( herr, pstderr ) <- liftIO $ createPipe
    procHandle <- fmap (Right . (, statusVar)) $ forkTest $ do
        executeScript procNode procName pstdin pstdout pstderr script
        liftIO $ putMVar statusVar ExitSuccess

    let procKillWith = Nothing
    let process = Process {..}

    void $ forkTest $ lineReadingLoop process hout $ \line -> do
        outProc OutputChildStdout process line
        liftIO $ atomically $ modifyTVar procOutput (++ [ line ])
    void $ forkTest $ lineReadingLoop process herr $ \line -> do
        outProc OutputChildStderr process line

    return process

withShellProcess :: Node -> ProcName -> ShellScript -> (Process -> TestRun a) -> TestRun a
withShellProcess node pname script inner = do
    procVar <- asks $ teProcesses . fst

    process <- spawnShell node pname script
    liftIO $ modifyMVar_ procVar $ return . (process:)

    inner process `finally` do
        ps <- liftIO $ takeMVar procVar
        closeProcess process `finally` do
            liftIO $ putMVar procVar $ filter (/=process) ps
