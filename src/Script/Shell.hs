module Script.Shell (
    ShellScript(..),
    ShellStatement(ShellStatement),
    ShellPipeline(ShellPipeline),
    ShellCommand(ShellCommand),
    ShellArgument(..),
    withShellProcess,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable

import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix.IO qualified as P
import System.Posix.Process
import System.Posix.Types
import System.Process hiding (ShellCommand)

import Network
import Network.Ip
import Output
import Process
import Run.Monad
import Script.Expr.Class
import Script.Var


newtype ShellScript = ShellScript [ ShellStatement ]

data ShellState = ShellState
    { shellWorkingDirectory :: FilePath
    , shellOldWorkingDirectory :: FilePath
    , shellExitOnError :: Bool
    , shellLastExitCode :: ExitCode
    }

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
    , cmdExtArguments :: [ ShellArgument ]
    , cmdSourceLine :: SourceLine
    }

data ShellArgument
    = ShellArgument Text
    | ShellRedirectStdin Text
    | ShellRedirectStdout Bool Text
    | ShellRedirectStderr Bool Text

cmdArguments :: ShellCommand -> [ Text ]
cmdArguments = catMaybes . map (\case ShellArgument x -> Just x; _ -> Nothing) . cmdExtArguments

instance ExprType ShellScript where
    textExprType _ = T.pack "ShellScript"
    textExprValue _ = "<shell-script>"

instance ExprType ShellStatement where
    textExprType _ = T.pack "ShellStatement"
    textExprValue _ = "<shell-statement>"

instance ExprType ShellPipeline where
    textExprType _ = T.pack "ShellPipeline"
    textExprValue _ = "<shell-pipeline>"

instance ExprType ShellCommand where
    textExprType _ = T.pack "ShellCommand"
    textExprValue _ = "<shell-command>"

instance ExprType ShellArgument where
    textExprType _ = T.pack "ShellArgument"
    textExprValue _ = "<shell-argument>"


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


executeCommand :: ShellExecInfo -> ShellState -> HandleHandling -> HandleHandling -> HandleHandling -> ShellCommand -> TestRun ShellState
executeCommand sei@ShellExecInfo {..} st pstdin pstdout pstderr scmd@ShellCommand {..} = do
    let args = cmdArguments scmd
    ( pstdin', pstdout', pstderr' ) <- (\f -> foldM f ( pstdin, pstdout, pstderr ) cmdExtArguments) $ \cur@( cin, cout, cerr ) -> \case
        ShellRedirectStdin path -> do
            closeIfRequested cin
            h <- liftIO $ openBinaryFile (nodeDir seiNode </> T.unpack path) ReadMode
            return ( CloseHandle h, cout, cerr )
        ShellRedirectStdout append path -> do
            closeIfRequested cout
            h <- liftIO $ openBinaryFile (nodeDir seiNode </> T.unpack path) $ if append then AppendMode else WriteMode
            return ( cin, CloseHandle h, cerr )
        ShellRedirectStderr append path -> do
            closeIfRequested cerr
            h <- liftIO $ openBinaryFile (nodeDir seiNode </> T.unpack path) $ if append then AppendMode else WriteMode
            return ( cin, cout, CloseHandle h )
        _ -> do
            return cur

    ( getExitStatus, st' ) <- executeCommandProcess sei st (handledHandle pstdin') (handledHandle pstdout') (handledHandle pstderr') args cmdCommand
    let failedWithStatus status = do
            when (shellExitOnError st) $ do
                liftIO $ putMVar seiStatusVar status
                throwError Failed
            return st' { shellLastExitCode = status }

    mapM_ closeIfRequested [ pstdin', pstdout', pstderr' ]
    getExitStatus >>= \case
        Exited ExitSuccess -> do
            return st' { shellLastExitCode = ExitSuccess }
        Exited status -> do
            outLine OutputChildFail (Just $ textProcName seiProcName) $ "failed at: " <> textSourceLine cmdSourceLine
            failedWithStatus status
        Terminated sig _ -> do
            outLine OutputChildFail (Just $ textProcName seiProcName) $ "killed with " <> T.pack (show sig) <> " at: " <> textSourceLine cmdSourceLine
            failedWithStatus (ExitFailure (- fromIntegral sig))
        Stopped sig -> do
            outLine OutputChildFail (Just $ textProcName seiProcName) $ "stopped with " <> T.pack (show sig) <> " at: " <> textSourceLine cmdSourceLine
            failedWithStatus (ExitFailure (- fromIntegral sig))


executeCommandProcess :: ShellExecInfo -> ShellState -> Handle -> Handle -> Handle -> [ Text ] -> Text -> TestRun ( TestRun ProcessStatus, ShellState )
executeCommandProcess ShellExecInfo {..} st@ShellState {..} pstdin pstdout pstderr args = \case
    "cd"
        | [] <- args -> liftIO $ do
            hPutStrLn pstdout (nodeDir seiNode)
            return ( return (Exited ExitSuccess), st
                { shellWorkingDirectory = nodeDir seiNode
                , shellOldWorkingDirectory = shellWorkingDirectory
                } )
        | [ "-" ] <- args -> liftIO $ do
            hPutStrLn pstdout shellOldWorkingDirectory
            return ( return (Exited ExitSuccess), st
                { shellWorkingDirectory = shellOldWorkingDirectory
                , shellOldWorkingDirectory = shellWorkingDirectory
                } )
        | [ dir ] <- args -> liftIO $ do
            cd <- canonicalizePath $ shellWorkingDirectory </> T.unpack dir
            doesDirectoryExist cd >>= \case
                True -> return ( return (Exited ExitSuccess), st
                    { shellWorkingDirectory = cd
                    , shellOldWorkingDirectory = shellWorkingDirectory
                    } )
                False -> do
                    hPutStrLn pstderr $ "cd: no such directory: " <> T.unpack dir
                    return ( return (Exited (ExitFailure (-1))), st )
        | otherwise -> do
            liftIO $ hPutStrLn pstderr $ "cd: too many arguments"
            return ( return (Exited (ExitFailure (-1))), st )

    "pwd"
        | [] <- args -> do
            liftIO $ hPutStrLn pstdout shellWorkingDirectory
            return ( return (Exited ExitSuccess), st )
        | otherwise -> do
            liftIO $ hPutStrLn pstderr $ "pwd: too many arguments"
            return ( return (Exited (ExitFailure (-1))), st )

    "set"
        | [ "+e" ] <- args -> do
            return ( return (Exited ExitSuccess), st { shellExitOnError = False } )
        | [ "-e" ] <- args -> do
            return ( return (Exited ExitSuccess), st { shellExitOnError = True } )
        | otherwise -> do
            liftIO $ hPutStrLn pstderr $ "set: " <> T.unpack (T.unwords args) <> ": not implemented"
            return ( return (Exited (ExitFailure (-1))), st )

    cmd -> liftIO $ do
        (_, _, _, phandle) <- createProcess_ "shell"
            (proc (T.unpack cmd) (map T.unpack args))
                { std_in = UseHandle pstdin
                , std_out = UseHandle pstdout
                , std_err = UseHandle pstderr
                , cwd = Just shellWorkingDirectory
                , env = Just []
                }
        Just pid <- getPid phandle
        let getProcessStatus' =
                liftIO (getProcessStatus True False pid) >>= \case
                    Just status -> return status
                    Nothing -> do
                        outLine OutputChildFail (Just $ textProcName seiProcName) $ "no exit status"
                        return (Exited (ExitFailure (-1)))
        return ( getProcessStatus', st )


executePipeline :: ShellExecInfo -> ShellState -> HandleHandling -> HandleHandling -> HandleHandling -> ShellPipeline -> TestRun ShellState
executePipeline sei st pstdin pstdout pstderr ShellPipeline {..} = do
    case pipeUpstream of
        Nothing -> do
            executeCommand sei st pstdin pstdout pstderr pipeCommand

        Just upstream -> do
            ( pipeRead, pipeWrite ) <- createPipeCloexec
            void $ forkTestUsing forkOS $ do
                void $ executePipeline sei st pstdin (CloseHandle pipeWrite) (KeepHandle $ handledHandle pstderr) upstream

            state' <- executeCommand sei st (CloseHandle pipeRead) pstdout (KeepHandle $ handledHandle pstderr) pipeCommand
            closeIfRequested pstderr
            return state'

executeScript :: ShellExecInfo -> Handle -> Handle -> Handle -> ShellScript -> TestRun ()
executeScript sei@ShellExecInfo {..} pstdin pstdout pstderr (ShellScript statements) = do
    setNetworkNamespace $ getNetns seiNode
    let initialState = ShellState
            { shellWorkingDirectory = nodeDir seiNode
            , shellOldWorkingDirectory = nodeDir seiNode
            , shellExitOnError = True
            , shellLastExitCode = ExitSuccess
            }
    finalState <- (\f -> foldM f initialState statements) $ \st ShellStatement {..} -> do
        executePipeline sei st (KeepHandle pstdin) (KeepHandle pstdout) (KeepHandle pstderr) shellPipeline

    liftIO $ putMVar seiStatusVar (shellLastExitCode finalState)

spawnShell :: Node -> ProcName -> ShellScript -> TestRun Process
spawnShell procNode procName script = do
    idVar <- asks $ teNextProcId . fst
    procId <- liftIO $ modifyMVar idVar (\x -> return ( x + 1, ProcessId x ))

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
    let procPid = Nothing
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
