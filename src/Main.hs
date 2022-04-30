module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

import Data.List
import Data.Maybe
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Read
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.Posix.Process
import System.Posix.Signals
import System.Process

import GDB
import Output
import Parser
import Process
import Test

data Network = Network
    { netNodes :: MVar [Node]
    , netProcesses :: MVar [Process]
    , netDir :: FilePath
    }

data Node = Node
    { nodeName :: NodeName
    , nodeNetwork :: Network
    , nodeDir :: FilePath
    }

data Options = Options
    { optDefaultTool :: String
    , optProcTools :: [(ProcName, String)]
    , optTimeout :: Scientific
    , optGDB :: Bool
    }

defaultOptions :: Options
defaultOptions = Options
    { optDefaultTool = ""
    , optProcTools = []
    , optTimeout = 1
    , optGDB = False
    }

testDir :: FilePath
testDir = "./.test"

initNetwork :: Output -> Bool -> IO Network
initNetwork out useGDB = do
    exists <- doesPathExist testDir
    when exists $ ioError $ userError $ testDir ++ " exists"
    createDirectoryIfMissing True testDir

    callCommand "ip link add name br0 type bridge"
    callCommand "ip addr add 192.168.0.1/24 broadcast 192.168.0.255 dev br0"
    callCommand "ip link set dev br0 up"
    callCommand "ip link set dev lo up"
    net <- Network <$> newMVar [] <*> newMVar [] <*> pure testDir

    void $ spawnOn out (Left net) (ProcNameTcpdump) (Just softwareTermination) $
        "tcpdump -i br0 -w '" ++ testDir ++ "/br0.pcap' -U -Z root"

    when useGDB $ do
        gdbInit =<< spawnOn out (Left net) ProcNameGDB Nothing gdbCmd

    return net

exitNetwork :: Output -> Network -> Bool -> IO ()
exitNetwork out net okTest = do
    processes <- readMVar (netProcesses net)
    forM_ processes $ \p -> do
        when (procName p /= ProcNameGDB) $ do
            hClose (procStdin p)
        case procKillWith p of
             Nothing -> return ()
             Just sig -> getPid (procHandle p) >>= \case
                Nothing -> return ()
                Just pid -> signalProcess sig pid

    forM_ processes $ \p -> do
        when (procName p == ProcNameGDB) $ do
            outPrompt out $ T.pack "gdb> "
            gdbSession p
            outClearPrompt out
            hClose (procStdin p)

    okProc <- fmap and $ forM processes $ \p -> do
        waitForProcess (procHandle p) >>= \case
            ExitSuccess -> return True
            ExitFailure code -> do
                outLine out OutputChildFail (Just $ procName p) $ T.pack $ "exit code: " ++ show code
                return False

    if okTest && okProc
       then do removeDirectoryRecursive $ netDir net
               exitSuccess
       else exitFailure

getNode :: Network -> NodeName -> IO Node
getNode net nname@(NodeName tnname) = (find ((nname==).nodeName) <$> readMVar (netNodes net)) >>= \case
    Just node -> return node
    _ -> do
        let name = T.unpack tnname
            dir = netDir net </> ("erebos_" ++ name)
            node = Node { nodeName = nname
                        , nodeNetwork = net
                        , nodeDir = dir
                        }

        exists <- doesPathExist dir
        when exists $ ioError $ userError $ dir ++ " exists"
        createDirectoryIfMissing True dir

        modifyMVar_ (netNodes net) $ \nodes -> do
            callCommand $ "ip netns add \""++ name ++ "\""
            callCommand $ "ip link add \"veth_" ++ name ++ ".0\" type veth peer name \"veth_" ++ name ++ ".1\" netns \"" ++ name ++ "\""
            callCommand $ "ip link set dev \"veth_" ++ name ++ ".0\" master br0 up"
            callOn node $ "ip addr add 192.168.0." ++ show (11 + length nodes) ++ "/24 broadcast 192.168.0.255 dev \"veth_" ++ name ++ ".1\""
            callOn node $ "ip link set dev \"veth_" ++ name++ ".1\" up"
            callOn node $ "ip link set dev lo up"
            return $ node : nodes
        return node

callOn :: Node -> String -> IO ()
callOn node cmd = callCommand $ "ip netns exec \"" ++ unpackNodeName (nodeName node) ++ "\" " ++ cmd

spawnOn :: Output -> Either Network Node -> ProcName -> Maybe Signal -> String -> IO Process
spawnOn out target pname killWith cmd = do
    let prefix = either (const "") (\node -> "ip netns exec \"" ++ unpackNodeName (nodeName node) ++ "\" ") target
    (Just hin, Just hout, Just herr, handle) <- createProcess (shell $ prefix ++ cmd)
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe
        , env = Just [("EREBOS_DIR", either netDir nodeDir target)]
        }
    pout <- newTVarIO []

    let readingLoop :: Handle -> (Text -> IO ()) -> IO ()
        readingLoop h act =
            tryIOError (T.hGetLine h) >>= \case
                Left err
                    | isEOFError err -> return ()
                    | otherwise -> outLine out OutputChildFail (Just pname) $ T.pack $ "IO error: " ++ show err
                Right line -> do
                    act line
                    readingLoop h act

    void $ forkIO $ readingLoop hout $ \line -> do
        outLine out OutputChildStdout (Just pname) line
        atomically $ modifyTVar pout (++[line])
    void $ forkIO $ readingLoop herr $ \line -> do
        case pname of
             ProcNameTcpdump -> return ()
             _ -> outLine out OutputChildStderr (Just pname) line

    let process = Process
            { procName = pname
            , procHandle = handle
            , procStdin = hin
            , procOutput = pout
            , procKillWith = killWith
            }

    let net = either id nodeNetwork target
    when (pname /= ProcNameGDB) $ do
        getPid handle >>= \case
            Just pid -> void $ do
                ps <- readMVar (netProcesses net)
                forM_ ps $ \gdb -> do
                    when (procName gdb == ProcNameGDB) $ do
                        addInferior gdb (length ps) pid
            Nothing -> return ()

    modifyMVar_ (netProcesses net) $ return . (process:)
    return process

getProcess :: Network -> ProcName -> IO Process
getProcess net pname = do
    Just p <- find ((pname==).procName) <$> readMVar (netProcesses net)
    return p

tryMatch :: Regex -> [Text] -> Maybe (Text, [Text])
tryMatch re (x:xs) | Right (Just _) <- regexec re x = Just (x, xs)
                   | otherwise = fmap (x:) <$> tryMatch re xs
tryMatch _ [] = Nothing

expect :: Output -> Options -> Process -> Regex -> Text -> IO Bool
expect out opts p re pat = do
    delay <- registerDelay $ ceiling $ 1000000 * optTimeout opts
    mbmatch <- atomically $ (Nothing <$ (check =<< readTVar delay)) <|> do
        line <- readTVar (procOutput p)
        case tryMatch re line of
             Nothing -> retry
             Just (m, out') -> do
                 writeTVar (procOutput p) out'
                 return $ Just m
    case mbmatch of
         Just line -> do
             outLine out OutputMatch (Just $ procName p) line
             return True
         Nothing -> do
             outLine out OutputMatchFail (Just $ procName p) $ T.pack "expect failed /" `T.append` pat `T.append` T.pack "/"
             return False

allM :: Monad m => [a] -> (a -> m Bool) -> m Bool
allM (x:xs) p = p x >>= \case True -> allM xs p; False -> return False
allM [] _ = return True

runTest :: Output -> Options -> Test -> IO ()
runTest out opts test = do
    net <- initNetwork out $ optGDB opts

    let sigHandler SignalInfo { siginfoSpecific = chld } = do
            processes <- readMVar (netProcesses net)
            forM_ processes $ \p -> do
                mbpid <- getPid (procHandle p)
                when (mbpid == Just (siginfoPid chld)) $ do
                    let err detail = outLine out OutputChildFail (Just $ procName p) detail
                    case siginfoStatus chld of
                         Exited ExitSuccess -> outLine out OutputChildInfo (Just $ procName p) $ T.pack $ "child exited successfully"
                         Exited (ExitFailure code) -> err $ T.pack $ "child process exited with status " ++ show code
                         Terminated sig _ -> err $ T.pack $ "child terminated with signal " ++ show sig
                         Stopped sig -> err $ T.pack $ "child stopped with signal " ++ show sig
    oldHandler <- installHandler processStatusChanged (CatchInfo sigHandler) Nothing

    ok <- allM (testSteps test) $ \case
        Spawn pname nname -> do
            node <- getNode net nname
            void $ spawnOn out (Right node) pname Nothing $
                fromMaybe (optDefaultTool opts) (lookup pname $ optProcTools opts)
            return True

        Send pname line -> do
            p <- getProcess net pname
            send p line
            return True

        Expect pname regex pat -> do
            p <- getProcess net pname
            expect out opts p regex pat

        Wait -> do
            outPrompt out $ T.pack "Waiting..."
            void $ getLine
            outClearPrompt out
            return True

    _ <- installHandler processStatusChanged oldHandler Nothing
    exitNetwork out net ok


options :: [OptDescr (Options -> Options)]
options =
    [ Option ['T'] ["tool"]
        (ReqArg (\str opts -> case break (==':') str of
                                   (path, []) -> opts { optDefaultTool = path }
                                   (pname, (_:path)) -> opts { optProcTools = (ProcName (T.pack pname), path) : optProcTools opts }
                ) "PATH")
        "test tool to be used"
    , Option ['t'] ["timeout"]
        (ReqArg (\str opts -> case readMaybe str of
                                   Just timeout -> opts { optTimeout = timeout }
                                   Nothing -> error "timeout must be a number") "SECONDS")
        "default timeout in seconds with microsecond precision"
    , Option ['g'] ["gdb"]
        (NoArg (\opts -> opts { optGDB = True }))
        "run GDB and attach spawned processes"
    ]

main :: IO ()
main = do
    envtool <- fromMaybe (error "No test tool defined") <$> lookupEnv "EREBOS_TEST_TOOL"
    args <- getArgs
    (opts, files) <- case getOpt Permute options args of
        (o, files, []) -> return (foldl (flip id) defaultOptions { optDefaultTool = envtool } o, files)
        (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
            where header = "Usage: erebos-tester [OPTION...]"

    optDefaultTool opts `seq` return ()

    out <- startOutput
    forM_ files $ mapM_ (runTest out opts) <=< parseTestFile
