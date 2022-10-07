module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.Maybe
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Read (readMaybe)
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
import Network
import Output
import Parser
import Process
import Test

data Options = Options
    { optDefaultTool :: String
    , optProcTools :: [(ProcName, String)]
    , optVerbose :: Bool
    , optTimeout :: Scientific
    , optGDB :: Bool
    }

defaultOptions :: Options
defaultOptions = Options
    { optDefaultTool = ""
    , optProcTools = []
    , optVerbose = False
    , optTimeout = 1
    , optGDB = False
    }

testDir :: FilePath
testDir = "./.test"


data TestEnv = TestEnv
    { teOutput :: Output
    , teFailed :: TVar Bool
    , teOptions :: Options
    }

data TestState = TestState
    { tsNetwork :: Network
    , tsVars :: [(VarName, SomeVarValue)]
    }

newtype TestRun a = TestRun { fromTestRun :: ReaderT (TestEnv, TestState) (ExceptT () IO) a }
    deriving (Functor, Applicative, Monad, MonadReader (TestEnv, TestState), MonadIO)

instance MonadFail TestRun where
    fail str = do
        outLine OutputError T.empty $ T.pack str
        throwError ()

instance MonadError () TestRun where
    throwError () = do
        failedVar <- asks $ teFailed . fst
        liftIO $ atomically $ writeTVar failedVar True
        TestRun $ throwError ()

    catchError (TestRun act) handler = TestRun $ catchError act $ fromTestRun . handler

instance MonadEval TestRun where
    lookupVar name = maybe (fail $ "variable not in scope: '" ++ unpackVarName name ++ "'") return =<< asks (lookup name . tsVars . snd)

instance MonadOutput TestRun where
    getOutput = asks $ teOutput . fst

withVar :: ExprType e => VarName -> e -> TestRun a -> TestRun a
withVar name value = local (fmap $ \s -> s { tsVars = (name, SomeVarValue value) : tsVars s })

forkTest :: TestRun () -> TestRun ()
forkTest act = do
    tenv <- ask
    void $ liftIO $ forkIO $ do
        runExceptT (flip runReaderT tenv $ fromTestRun act) >>= \case
            Left () -> atomically $ writeTVar (teFailed $ fst tenv) True
            Right () -> return ()

atomicallyTest :: STM a -> TestRun a
atomicallyTest act = do
    failedVar <- asks $ teFailed . fst
    res <- liftIO $ atomically $ do
        failed <- readTVar failedVar
        if failed then return $ Left ()
                  else Right <$> act
    case res of
        Left e -> throwError e
        Right x -> return x

initNetwork :: (Network -> TestRun a) -> TestRun a
initNetwork inner = do
    net <- liftIO $ do
        exists <- doesPathExist testDir
        when exists $ ioError $ userError $ testDir ++ " exists"
        createDirectoryIfMissing True testDir

        callCommand "ip link add name br0 group 1 type bridge"
        callCommand "ip addr add 192.168.0.1/24 broadcast 192.168.0.255 dev br0"
        callCommand "ip link set dev br0 up"
        callCommand "ip link set dev lo up"
        Network <$> newMVar [] <*> newMVar [] <*> pure testDir

    void $ spawnOn (Left net) (ProcNameTcpdump) (Just softwareTermination) $
        "tcpdump -i br0 -w '" ++ testDir ++ "/br0.pcap' -U -Z root"

    useGDB <- asks $ optGDB . teOptions . fst
    when useGDB $ do
        gdbInit =<< spawnOn (Left net) ProcNameGDB Nothing gdbCmd

    local (fmap $ \s -> s { tsNetwork = net }) $ inner net

exitNetwork :: Network -> TestRun ()
exitNetwork net = do
    processes <- liftIO $ readMVar (netProcesses net)

    forM_ processes $ \p -> do
        when (procName p == ProcNameGDB) $ do
            outPrompt $ T.pack "gdb> "
            gdbSession p
            outClearPrompt

    forM_ processes $ \p -> do
        closeProcess p `catchError` \_ -> return ()

    liftIO $ do
        callCommand $ "ip -all netns del"
        callCommand $ "ip link del group 1"

    failed <- liftIO . atomically . readTVar =<< asks (teFailed . fst)
    liftIO $ if failed then exitFailure
                       else removeDirectoryRecursive $ netDir net

createNode :: TypedVarName Node -> (Node -> TestRun a) -> TestRun a
createNode (TypedVarName vname) inner = do
    net <- asks $ tsNetwork . snd
    node <- liftIO $ do
        node <- modifyMVar (netNodes net) $ \nodes -> do
            let nname = nextNodeName vname $ map nodeName nodes
                ip = "192.168.0." ++ show (11 + length nodes)
                node = Node { nodeName = nname
                            , nodeIp = T.pack ip
                            , nodeNetwork = net
                            , nodeDir = netDir net </> ("erebos_" ++ unpackNodeName nname)
                            }
            return $ (node : nodes, node)

        let name = unpackNodeName $ nodeName node
            dir = nodeDir node

        exists <- doesPathExist dir
        when exists $ ioError $ userError $ dir ++ " exists"
        createDirectoryIfMissing True dir

        callCommand $ "ip netns add \""++ name ++ "\""
        callCommand $ "ip link add \"veth_" ++ name ++ ".0\" group 1 type veth peer name \"veth_" ++ name ++ ".1\" netns \"" ++ name ++ "\""
        callCommand $ "ip link set dev \"veth_" ++ name ++ ".0\" master br0 up"
        callOn node $ "ip addr add " ++ T.unpack (nodeIp node) ++ "/24 broadcast 192.168.0.255 dev \"veth_" ++ name ++ ".1\""
        callOn node $ "ip link set dev \"veth_" ++ name++ ".1\" up"
        callOn node $ "ip link set dev lo up"
        return node

    withVar vname node $ inner node

callOn :: Node -> String -> IO ()
callOn node cmd = callCommand $ "ip netns exec \"" ++ unpackNodeName (nodeName node) ++ "\" " ++ cmd

spawnOn :: Either Network Node -> ProcName -> Maybe Signal -> String -> TestRun Process
spawnOn target pname killWith cmd = do
    let prefix = either (const "") (\node -> "ip netns exec \"" ++ unpackNodeName (nodeName node) ++ "\" ") target
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
            }

    let readingLoop :: Handle -> (Text -> TestRun ()) -> TestRun ()
        readingLoop h act =
            liftIO (tryIOError (T.hGetLine h)) >>= \case
                Left err
                    | isEOFError err -> return ()
                    | otherwise -> outProc OutputChildFail process $ T.pack $ "IO error: " ++ show err
                Right line -> do
                    act line
                    readingLoop h act

    forkTest $ readingLoop hout $ \line -> do
        outProc OutputChildStdout process line
        liftIO $ atomically $ modifyTVar pout (++[line])
    forkTest $ readingLoop herr $ \line -> do
        case pname of
             ProcNameTcpdump -> return ()
             _ -> outProc OutputChildStderr process line

    let net = either id nodeNetwork target
    when (pname /= ProcNameGDB) $ liftIO $ do
        getPid handle >>= \case
            Just pid -> void $ do
                ps <- readMVar (netProcesses net)
                forM_ ps $ \gdb -> do
                    when (procName gdb == ProcNameGDB) $ do
                        addInferior gdb (length ps) pid
            Nothing -> return ()

    liftIO $ modifyMVar_ (netProcesses net) $ return . (process:)
    return process

tryMatch :: Regex -> [Text] -> Maybe ((Text, [Text]), [Text])
tryMatch re (x:xs) | Right (Just (_, _, _, capture)) <- regexec re x = Just ((x, capture), xs)
                   | otherwise = fmap (x:) <$> tryMatch re xs
tryMatch _ [] = Nothing

exprFailed :: Text -> SourceLine -> Maybe ProcName -> Expr a -> TestRun ()
exprFailed desc (SourceLine sline) pname expr = do
    let prompt = maybe T.empty textProcName pname
    exprVars <- gatherVars expr
    outLine OutputMatchFail prompt $ T.concat [desc, T.pack " failed on ", sline]
    forM_ exprVars $ \(name, value) ->
        outLine OutputMatchFail prompt $ T.concat [T.pack "  ", textVarName name, T.pack " = ", textSomeVarValue value]
    throwError ()

expect :: SourceLine -> Process -> Expr Regex -> [TypedVarName Text] -> TestRun () -> TestRun ()
expect (SourceLine sline) p expr tvars inner = do
    re <- eval expr
    timeout <- asks $ optTimeout . teOptions . fst
    delay <- liftIO $ registerDelay $ ceiling $ 1000000 * timeout
    mbmatch <- atomicallyTest $ (Nothing <$ (check =<< readTVar delay)) <|> do
        line <- readTVar (procOutput p)
        case tryMatch re line of
             Nothing -> retry
             Just (m, out') -> do
                 writeTVar (procOutput p) out'
                 return $ Just m
    case mbmatch of
         Just (line, capture) -> do
             let vars = map (\(TypedVarName n) -> n) tvars

             when (length vars /= length capture) $ do
                 outProc OutputMatchFail p $ T.pack "mismatched number of capture variables on " `T.append` sline
                 throwError ()

             forM_ vars $ \name -> do
                 cur <- asks (lookup name . tsVars . snd)
                 when (isJust cur) $ do
                     outProc OutputError p $ T.pack "variable '" `T.append` textVarName name `T.append` T.pack "' already exists on " `T.append` sline
                     throwError ()

             outProc OutputMatch p line
             local (fmap $ \s -> s { tsVars = zip vars (map SomeVarValue capture) ++ tsVars s }) inner

         Nothing -> exprFailed (T.pack "expect") (SourceLine sline) (Just $ procName p) expr

testStepGuard :: SourceLine -> Expr Bool -> TestRun ()
testStepGuard sline expr = do
    x <- eval expr
    when (not x) $ exprFailed (T.pack "guard") sline Nothing expr

allM :: Monad m => [a] -> (a -> m Bool) -> m Bool
allM (x:xs) p = p x >>= \case True -> allM xs p; False -> return False
allM [] _ = return True

finally :: MonadError e m => m a -> m b -> m a
finally act handler = do
    x <- act `catchError` \e -> handler >> throwError e
    void handler
    return x

evalSteps :: [TestStep] -> TestRun ()
evalSteps = mapM_ $ \case
    Let (SourceLine sline) name expr inner -> do
        cur <- asks (lookup name . tsVars . snd)
        when (isJust cur) $ do
            outLine OutputError T.empty $ T.pack "variable '" `T.append` textVarName name `T.append` T.pack "' already exists on " `T.append` sline
            throwError ()
        value <- eval expr
        withVar name value $ evalSteps inner

    Spawn (TypedVarName vname@(VarName tname)) nname inner -> do
        either createNode ((>>=) . eval) nname $ \node -> do
            let pname = ProcName tname
            opts <- asks $ teOptions . fst
            p <- spawnOn (Right node) pname Nothing $
                fromMaybe (optDefaultTool opts) (lookup pname $ optProcTools opts)
            withVar vname p (evalSteps inner) `finally` do
                net <- asks $ tsNetwork . snd
                ps <- liftIO $ takeMVar (netProcesses net)
                closeProcess p `finally` do
                    liftIO $ putMVar (netProcesses net) $ filter (/=p) ps

    Send pname expr -> do
        p <- eval pname
        line <- eval expr
        send p line

    Expect line pname expr captures inner -> do
        p <- eval pname
        expect line p expr captures $ evalSteps inner

    Guard line expr -> do
        testStepGuard line expr

    Wait -> do
        outPrompt $ T.pack "Waiting..."
        void $ liftIO $ getLine
        outClearPrompt

runTest :: Output -> Options -> Test -> IO Bool
runTest out opts test = do
    tenv <- TestEnv
        <$> pure out
        <*> newTVarIO False
        <*> pure opts
    tstate <- TestState
        <$> pure (error "network not initialized")
        <*> pure []
    (fmap $ either (const False) id) $ runExceptT $ flip runReaderT (tenv, tstate) $ fromTestRun $ initNetwork $ \net -> do
        let sigHandler SignalInfo { siginfoSpecific = chld } = do
                processes <- readMVar (netProcesses net)
                forM_ processes $ \p -> do
                    mbpid <- getPid (procHandle p)
                    when (mbpid == Just (siginfoPid chld)) $ flip runReaderT out $ do
                        let err detail = outProc OutputChildFail p detail
                        case siginfoStatus chld of
                             Exited ExitSuccess -> outProc OutputChildInfo p $ T.pack $ "child exited successfully"
                             Exited (ExitFailure code) -> err $ T.pack $ "child process exited with status " ++ show code
                             Terminated sig _ -> err $ T.pack $ "child terminated with signal " ++ show sig
                             Stopped sig -> err $ T.pack $ "child stopped with signal " ++ show sig
        oldHandler <- liftIO $ installHandler processStatusChanged (CatchInfo sigHandler) Nothing

        flip catchError (const $ return ()) $ evalSteps $ testSteps test

        _ <- liftIO $ installHandler processStatusChanged oldHandler Nothing
        exitNetwork net

        atomicallyTest $ return True


options :: [OptDescr (Options -> Options)]
options =
    [ Option ['T'] ["tool"]
        (ReqArg (\str opts -> case break (==':') str of
                                   (path, []) -> opts { optDefaultTool = path }
                                   (pname, (_:path)) -> opts { optProcTools = (ProcName (T.pack pname), path) : optProcTools opts }
                ) "PATH")
        "test tool to be used"
    , Option ['v'] ["verbose"]
        (NoArg (\opts -> opts { optVerbose = True }))
        "show output of processes and successful tests"
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

    out <- startOutput $ optVerbose opts
    forM_ files $ mapM_ (runTest out opts) <=< parseTestFile
