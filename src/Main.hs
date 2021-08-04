module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

import Text.Regex.TDFA
import Text.Regex.TDFA.String

import System.Environment
import System.IO
import System.IO.Error
import System.Process

data Network = Network
    { netNodes :: MVar [(Int, Node)]
    }

data Node = Node
    { nodeNetwork :: Network
    , nodeName :: String
    }

data Process = Process
    { procHandle :: ProcessHandle
    , procNode :: Node
    , procStdin :: Handle
    , procOutput :: TVar [String]
    }

initNetwork :: IO Network
initNetwork = do
    callCommand "ip link add name br0 type bridge"
    callCommand "ip addr add 192.168.0.1/24 broadcast 192.168.0.255 dev br0"
    callCommand "ip link set dev br0 up"
    callCommand "ip link set dev lo up"
    Network <$> newMVar []

getNode :: Network -> Int -> IO Node
getNode net idx = (lookup idx <$> readMVar (netNodes net)) >>= \case
    Just node -> return node
    _ -> do
        let name = "node" ++ show idx
            node = Node { nodeNetwork = net
                        , nodeName = name
                        }
        callCommand $ "ip netns add \""++ name ++ "\""
        callCommand $ "ip link add \"veth" ++ show idx ++ ".0\" type veth peer name \"veth" ++ show idx ++ ".1\" netns \"" ++ name ++ "\""
        callCommand $ "ip link set dev \"veth" ++ show idx ++ ".0\" master br0 up"
        callOn node $ "ip addr add 192.168.0." ++ show (10 + idx) ++ "/24 broadcast 192.168.0.255 dev \"veth" ++ show idx ++ ".1\""
        callOn node $ "ip link set dev \"veth" ++ show idx ++ ".1\" up"
        callOn node $ "ip link set dev lo up"
        modifyMVar_ (netNodes net) $ return . ((idx, node):)
        return node

callOn :: Node -> String -> IO ()
callOn node cmd = callCommand $ "ip netns exec \"" ++ nodeName node ++ "\" " ++ cmd

spawnOn :: Node -> String -> IO Process
spawnOn node cmd = do
    (Just hin, Just hout, Just herr, handle) <- createProcess (shell $ "ip netns exec \"" ++ nodeName node ++ "\" " ++ cmd)
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
    out <- newTVarIO []

    let readingLoop :: Handle -> (String -> IO ()) -> IO ()
        readingLoop h act =
            tryIOError (hGetLine h) >>= \case
                Left err
                    | isEOFError err -> return ()
                    | otherwise -> putStrLn $ "\ESC[31m" ++ nodeName node ++ "!!> IO error: " ++ show err ++ "\ESC[0m"
                Right line -> do
                    act line
                    readingLoop h act

    void $ forkIO $ readingLoop hout $ \line -> do
        putStrLn $ nodeName node ++ "> " ++ line
        atomically $ modifyTVar out (++[line])
    void $ forkIO $ readingLoop herr $ \line -> do
        putStrLn $ "\ESC[31m" ++ nodeName node ++ "!> " ++ line ++ "\ESC[0m"

    return Process
        { procHandle = handle
        , procNode = node
        , procStdin = hin
        , procOutput = out
        }

tryMatch :: Regex -> [String] -> Maybe (String, [String])
tryMatch re (x:xs) | Right (Just _) <- regexec re x = Just (x, xs)
                   | otherwise = fmap (x:) <$> tryMatch re xs
tryMatch _ [] = Nothing

expect :: Process -> String -> IO ()
expect p pat = case compile defaultCompOpt defaultExecOpt ("^" ++ pat ++ "$") of
                    Right re -> do
                        mbmatch <- atomically $ do
                            out <- readTVar (procOutput p)
                            case tryMatch re out of
                                 Nothing -> retry
                                 Just (m, out') -> do
                                     writeTVar (procOutput p) out'
                                     return $ Just m
                        case mbmatch of
                             Just line -> putStrLn $ "\ESC[32m" ++ nodeName (procNode p) ++ "+> " ++ line ++ "\ESC[0m"
                             Nothing -> putStrLn $ "\ESC[31m" ++ nodeName (procNode p) ++ "/> expect failed" ++ "\ESC[0m"
                    Left err -> putStrLn $ "failed to parse re: " ++ err

send :: Process -> String -> IO ()
send p str = do
    hPutStrLn (procStdin p) str
    hFlush (procStdin p)

main :: IO ()
main = do
    [tool] <- getArgs

    net <- initNetwork
    node1 <- getNode net 1
    node2 <- getNode net 2

    p1 <- spawnOn node1 tool
    p2 <- spawnOn node2 tool

    send p1 "create-identity Device1"
    send p2 "create-identity Device2"
    send p1 "start-server"
    send p2 "start-server"
    expect p1 "peer [0-9]+ 192.168.0.11:29665"
    expect p1 "peer [0-9]+ 192.168.0.12:29665"
    expect p2 "peer [0-9]+ 192.168.0.12:29665"
    expect p2 "peer [0-9]+ 192.168.0.11:29665"
