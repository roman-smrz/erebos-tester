module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Regex.TDFA
import Text.Regex.TDFA.String

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.Posix.Process
import System.Posix.Signals
import System.Process

import Parser
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

data Process = Process
    { procName :: ProcName
    , procHandle :: ProcessHandle
    , procNode :: Node
    , procStdin :: Handle
    , procOutput :: TVar [String]
    }

testDir :: FilePath
testDir = "./.test"

initNetwork :: IO Network
initNetwork = do
    exists <- doesPathExist testDir
    when exists $ ioError $ userError $ testDir ++ " exists"
    createDirectoryIfMissing True testDir

    callCommand "ip link add name br0 type bridge"
    callCommand "ip addr add 192.168.0.1/24 broadcast 192.168.0.255 dev br0"
    callCommand "ip link set dev br0 up"
    callCommand "ip link set dev lo up"
    Network <$> newMVar [] <*> newMVar [] <*> pure testDir

exitNetwork :: Network -> IO ()
exitNetwork net = do
    processes <- readMVar (netProcesses net)
    ok <- fmap and $ forM processes $ \p -> do
        hClose (procStdin p)
        waitForProcess (procHandle p) >>= \case
            ExitSuccess -> return True
            ExitFailure code -> do
                putStrLn $ "\ESC[31m" ++ unpackNodeName (nodeName (procNode p)) ++ "!!> exit code: " ++ show code ++ "\ESC[0m"
                return False

    if ok
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

spawnOn :: Node -> ProcName -> String -> IO Process
spawnOn node pname cmd = do
    (Just hin, Just hout, Just herr, handle) <- createProcess (shell $ "ip netns exec \"" ++ unpackNodeName (nodeName node) ++ "\" " ++ cmd)
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe
        , env = Just [("EREBOS_DIR", nodeDir node)]
        }
    out <- newTVarIO []

    let readingLoop :: Handle -> (String -> IO ()) -> IO ()
        readingLoop h act =
            tryIOError (hGetLine h) >>= \case
                Left err
                    | isEOFError err -> return ()
                    | otherwise -> putStrLn $ "\ESC[31m" ++ unpackNodeName (nodeName node) ++ "!!> IO error: " ++ show err ++ "\ESC[0m"
                Right line -> do
                    act line
                    readingLoop h act

    void $ forkIO $ readingLoop hout $ \line -> do
        putStrLn $ unpackNodeName (nodeName node) ++ "> " ++ line
        atomically $ modifyTVar out (++[line])
    void $ forkIO $ readingLoop herr $ \line -> do
        putStrLn $ "\ESC[31m" ++ unpackNodeName (nodeName node) ++ "!> " ++ line ++ "\ESC[0m"

    let process = Process
            { procName = pname
            , procHandle = handle
            , procNode = node
            , procStdin = hin
            , procOutput = out
            }

    modifyMVar_ (netProcesses (nodeNetwork node)) $ return . (process:)
    return process

getProcess :: Network -> ProcName -> IO Process
getProcess net pname = do
    Just p <- find ((pname==).procName) <$> readMVar (netProcesses net)
    return p

tryMatch :: Regex -> [String] -> Maybe (String, [String])
tryMatch re (x:xs) | Right (Just _) <- regexec re x = Just (x, xs)
                   | otherwise = fmap (x:) <$> tryMatch re xs
tryMatch _ [] = Nothing

expect :: Process -> Regex -> IO ()
expect p re = do
    mbmatch <- atomically $ do
        out <- readTVar (procOutput p)
        case tryMatch re out of
             Nothing -> retry
             Just (m, out') -> do
                 writeTVar (procOutput p) out'
                 return $ Just m
    case mbmatch of
         Just line -> putStrLn $ "\ESC[32m" ++ unpackNodeName (nodeName (procNode p)) ++ "+> " ++ line ++ "\ESC[0m"
         Nothing -> putStrLn $ "\ESC[31m" ++ unpackNodeName (nodeName (procNode p)) ++ "/> expect failed" ++ "\ESC[0m"

send :: Process -> Text -> IO ()
send p line = do
    T.hPutStrLn (procStdin p) line
    hFlush (procStdin p)

runTest :: String -> Test -> IO ()
runTest tool test = do
    net <- initNetwork

    let sigHandler SignalInfo { siginfoSpecific = chld } = do
            processes <- readMVar (netProcesses net)
            forM_ processes $ \p -> do
                mbpid <- getPid (procHandle p)
                when (mbpid == Just (siginfoPid chld)) $ do
                    let err detail = putStrLn $ "\ESC[31m" ++ unpackNodeName (nodeName (procNode p)) ++ "!!> child " ++ detail ++ "\ESC[0m"
                    case siginfoStatus chld of
                         Exited ExitSuccess -> putStrLn $ unpackNodeName (nodeName (procNode p)) ++ ".> child exited successfully"
                         Exited (ExitFailure code) -> err $ "process exited with status " ++ show code
                         Terminated sig _ -> err $ "terminated with signal " ++ show sig
                         Stopped sig -> err $ "stopped with signal " ++ show sig
    oldHandler <- installHandler processStatusChanged (CatchInfo sigHandler) Nothing

    forM_ (testSteps test) $ \case
        Spawn pname nname -> do
            node <- getNode net nname
            void $ spawnOn node pname tool

        Send pname line -> do
            p <- getProcess net pname
            send p line

        Expect pname regex -> do
            p <- getProcess net pname
            expect p regex

        Wait -> do
            putStr "Waiting..."
            hFlush stdout
            void $ getLine

    _ <- installHandler processStatusChanged oldHandler Nothing
    exitNetwork net

main :: IO ()
main = do
    tool <- getEnv "EREBOS_TEST_TOOL"
    files <- getArgs

    forM_ files $ mapM_ (runTest tool) <=< parseTestFile
