module Process (
    Process(..),
    ProcName(..),
    textProcName, unpackProcName,
    send,
    outProc,
    lineReadingLoop,
    startProcessIOLoops,
    spawnOn,
    closeProcess,
    closeTestProcess,
    withProcess,

    IgnoreProcessOutput(..),
    flushProcessOutput,
) where

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.Function
import Data.Maybe
import Data.Scientific
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.Posix.Signals
import System.Process

import {-# SOURCE #-} GDB
import Network
import Network.Ip
import Output
import Run.Monad
import Script.Expr
import Script.Expr.Class
import Script.Object

data Process = Process
    { procName :: ProcName
    , procHandle :: Either ProcessHandle ( ThreadId, MVar ExitCode )
    , procStdin :: Handle
    , procOutput :: TVar [ Text ]
    , procIgnore :: TVar ( Int, [ ( Int, Maybe Regex ) ] )
    , procKillWith :: Maybe Signal
    , procNode :: Node
    }

instance Eq Process where
    (==) = (==) `on` procStdin

instance ExprType Process where
    textExprType _ = T.pack "proc"
    textExprValue n = T.pack "p:" <> textProcName (procName n)

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
        Left err -> do
            when (not (isEOFError err)) $ do
                outProc OutputChildFail process $ T.pack $ "IO error: " ++ show err
            liftIO $ hClose h
        Right line -> do
            act line
            lineReadingLoop process h act

startProcessIOLoops :: Process -> Handle -> Handle -> TestRun ()
startProcessIOLoops process@Process {..} hout herr = do

    void $ forkTest $ lineReadingLoop process hout $ \line -> do
        outProc OutputChildStdout process line
        liftIO $ atomically $ do
            ignores <- map snd . snd <$> readTVar procIgnore
            when (not $ any (matches line) ignores) $ do
                modifyTVar procOutput (++ [ line ])

    void $ forkTest $ lineReadingLoop process herr $ \line -> do
        case procName of
            ProcNameTcpdump -> return ()
            _ -> outProc OutputChildStderr process line

  where
    matches _    Nothing
        = True
    matches line (Just re)
        | Right (Just _) <- regexMatch re line = True
        | otherwise = False

spawnOn :: Either Network Node -> ProcName -> Maybe Signal -> String -> TestRun Process
spawnOn target procName procKillWith cmd = do
    -- When executing command given with relative path, turn it to absolute one,
    -- because working directory will be changed for the shell wrapper.
    cmd' <- liftIO $ do
        case span (/= ' ') cmd of
            ( path, rest )
                | any isPathSeparator path && isRelative path
                -> do
                    path' <- makeAbsolute path
                    return (path' ++ rest)
            _ -> return cmd

    let netns = either getNetns getNetns target
    currentEnv <- liftIO $ getEnvironment
    (Just procStdin, Just hout, Just herr, handle) <- liftIO $ do
        runInNetworkNamespace netns $ createProcess (shell cmd')
            { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe
            , cwd = Just (either netDir nodeDir target)
            , env = Just $ ( "EREBOS_DIR", "." ) : currentEnv
            }
    let procHandle = Left handle
    procOutput <- liftIO $ newTVarIO []
    procIgnore <- liftIO $ newTVarIO ( 0, [] )
    let procNode = either (const undefined) id target
    let process = Process {..}

    startProcessIOLoops process hout herr

    asks (teGDB . fst) >>= maybe (return Nothing) (liftIO . tryReadMVar) >>= \case
        Just gdb | ProcName _ <- procName -> addInferior gdb process
        _ -> return ()

    return process

closeProcess :: (MonadIO m, MonadOutput m, MonadError Failed m) => Scientific -> Process -> m ()
closeProcess timeout p = do
    liftIO $ hClose $ procStdin p
    case procKillWith p of
        Nothing -> return ()
        Just sig -> liftIO $ either getPid (\_ -> return Nothing) (procHandle p) >>= \case
            Nothing -> return ()
            Just pid -> signalProcess sig pid

    liftIO $ void $ forkIO $ do
        threadDelay $ floor $ 1000000 * timeout
        either terminateProcess (killThread . fst) $ procHandle p
    liftIO (either waitForProcess (takeMVar . snd) (procHandle p)) >>= \case
        ExitSuccess -> return ()
        ExitFailure code -> do
            outProc OutputChildFail p $ T.pack $ "exit code: " ++ show code
            throwError Failed

closeTestProcess :: Process -> TestRun ()
closeTestProcess process = do
    timeout <- getCurrentTimeout
    closeProcess timeout process

withProcess :: Either Network Node -> ProcName -> Maybe Signal -> String -> (Process -> TestRun a) -> TestRun a
withProcess target pname killWith cmd inner = do
    procVar <- asks $ teProcesses . fst

    process <- spawnOn target pname killWith cmd
    liftIO $ modifyMVar_ procVar $ return . (process:)

    inner process `finally` do
        ps <- liftIO $ takeMVar procVar
        closeTestProcess process `finally` do
            liftIO $ putMVar procVar $ filter (/=process) ps


data IgnoreProcessOutput = IgnoreProcessOutput Process Int

instance ObjectType TestRun IgnoreProcessOutput where
    type ConstructorArgs IgnoreProcessOutput = ( Process, Maybe Regex )

    createObject oid ( process@Process {..}, regex ) = do
        liftIO $ atomically $ do
            flushProcessOutput process regex
            ( iid, list ) <- readTVar procIgnore
            writeTVar procIgnore ( iid + 1, ( iid, regex ) : list )
            return $ Object oid $ IgnoreProcessOutput process iid

    destroyObject Object { objImpl = IgnoreProcessOutput Process {..} iid } = do
        liftIO $ atomically $ do
            writeTVar procIgnore . fmap (filter ((iid /=) . fst)) =<< readTVar procIgnore

flushProcessOutput :: Process -> Maybe Regex -> STM ()
flushProcessOutput p mbre = do
    writeTVar (procOutput p) =<< case mbre of
        Nothing -> return []
        Just re -> filter (either error isNothing . regexMatch re) <$> readTVar (procOutput p)
