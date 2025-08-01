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
import Network.Ip
import Output
import Process
import Run.Monad
import Script.Var


data ShellStatement = ShellStatement
    { shellCommand :: Text
    , shellArguments :: [ Text ]
    , shellSourceLine :: SourceLine
    }

newtype ShellScript = ShellScript [ ShellStatement ]


executeScript :: Node -> ProcName -> MVar ExitCode -> Handle -> Handle -> Handle -> ShellScript -> TestRun ()
executeScript node pname statusVar pstdin pstdout pstderr (ShellScript statements) = do
    setNetworkNamespace $ getNetns node
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
                status -> do
                    outLine OutputChildFail (Just $ textProcName pname) $ "failed at: " <> textSourceLine shellSourceLine
                    liftIO $ putMVar statusVar status
                    throwError Failed
    liftIO $ putMVar statusVar ExitSuccess

spawnShell :: Node -> ProcName -> ShellScript -> TestRun Process
spawnShell procNode procName script = do
    procOutput <- liftIO $ newTVarIO []
    statusVar <- liftIO $ newEmptyMVar
    ( pstdin, procStdin ) <- liftIO $ createPipe
    ( hout, pstdout ) <- liftIO $ createPipe
    ( herr, pstderr ) <- liftIO $ createPipe
    procHandle <- fmap (Right . (, statusVar)) $ forkTestUsing forkOS $ do
        executeScript procNode procName statusVar pstdin pstdout pstderr script

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
        closeTestProcess process `finally` do
            liftIO $ putMVar procVar $ filter (/=process) ps
