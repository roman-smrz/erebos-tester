module Process (
    Process(..),
    ProcName(..),
    textProcName, unpackProcName,
    send,
    outProc,
    lineReadingLoop,
    spawnOn,
    closeProcess,
    withProcess,
) where

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.Function
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Exit
import System.IO
import System.IO.Error
import System.Posix.Signals
import System.Process

import {-# SOURCE #-} GDB
import Network
import Network.Ip
import Output
import Run.Monad
import Test

data Process = Process
    { procName :: ProcName
    , procHandle :: ProcessHandle
    , procStdin :: Handle
    , procOutput :: TVar [Text]
    , procKillWith :: Maybe Signal
    , procNode :: Node
    }

instance Eq Process where
    (==) = (==) `on` procStdin

instance ExprType Process where
    textExprType _ = T.pack "proc"
    textExprValue n = T.pack "p:" <> textProcName (procName n)
    emptyVarValue = Process (ProcName T.empty) undefined undefined undefined undefined emptyVarValue

    recordMembers = map (first T.pack)
        [ ("node", RecordSelector $ procNode)
        ]


data ProcName = ProcName Text
              | ProcNameTcpdump
              | ProcNameGDB
    deriving (Eq, Ord)

textProcName :: ProcName -> Text
textProcName (ProcName name) = name
textProcName ProcNameTcpdump = T.pack "tcpdump"
textProcName ProcNameGDB = T.pack "gdb"

unpackProcName :: ProcName -> String
unpackProcName = T.unpack . textProcName

send :: MonadIO m => Process -> Text -> m ()
send p line = liftIO $ do
    T.hPutStrLn (procStdin p) line
    hFlush (procStdin p)

outProc :: MonadOutput m => OutputType -> Process -> Text -> m ()
outProc otype p line = outLine otype (Just $ textProcName $ procName p) line

lineReadingLoop :: MonadOutput m => Process -> Handle -> (Text -> m ()) -> m ()
lineReadingLoop process h act =
    liftIO (tryIOError (T.hGetLine h)) >>= \case
        Left err
            | isEOFError err -> return ()
            | otherwise -> outProc OutputChildFail process $ T.pack $ "IO error: " ++ show err
        Right line -> do
            act line
            lineReadingLoop process h act

spawnOn :: Either Network Node -> ProcName -> Maybe Signal -> String -> TestRun Process
spawnOn target pname killWith cmd = do
    let netns = either getNetns getNetns target
    let prefix = T.unpack $ "ip netns exec \"" <> textNetnsName netns <> "\" "
    (Just hin, Just hout, Just herr, handle) <- liftIO $ createProcess (shell $ prefix ++ cmd)
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe
        , env = Just [("EREBOS_DIR", either netDir nodeDir target)]
        }
    pout <- liftIO $ newTVarIO []

    let process = Process
            { procName = pname
            , procHandle = handle
            , procStdin = hin
            , procOutput = pout
            , procKillWith = killWith
            , procNode = either (const undefined) id target
            }

    forkTest $ lineReadingLoop process hout $ \line -> do
        outProc OutputChildStdout process line
        liftIO $ atomically $ modifyTVar pout (++[line])
    forkTest $ lineReadingLoop process herr $ \line -> do
        case pname of
             ProcNameTcpdump -> return ()
             _ -> outProc OutputChildStderr process line

    asks (teGDB . fst) >>= maybe (return Nothing) (liftIO . tryReadMVar) >>= \case
        Just gdb | ProcName _ <- pname -> addInferior gdb process
        _ -> return ()

    return process

closeProcess :: (MonadIO m, MonadOutput m, MonadError Failed m) => Process -> m ()
closeProcess p = do
    liftIO $ hClose $ procStdin p
    case procKillWith p of
        Nothing -> return ()
        Just sig -> liftIO $ getPid (procHandle p) >>= \case
            Nothing -> return ()
            Just pid -> signalProcess sig pid

    liftIO $ void $ forkIO $ do
        threadDelay 1000000
        terminateProcess $ procHandle p
    liftIO (waitForProcess (procHandle p)) >>= \case
        ExitSuccess -> return ()
        ExitFailure code -> do
            outProc OutputChildFail p $ T.pack $ "exit code: " ++ show code
            throwError Failed

withProcess :: Either Network Node -> ProcName -> Maybe Signal -> String -> (Process -> TestRun a) -> TestRun a
withProcess target pname killWith cmd inner = do
    procVar <- asks $ teProcesses . fst

    process <- spawnOn target pname killWith cmd
    liftIO $ modifyMVar_ procVar $ return . (process:)

    inner process `finally` do
        ps <- liftIO $ takeMVar procVar
        closeProcess process `finally` do
            liftIO $ putMVar procVar $ filter (/=process) ps
