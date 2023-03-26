module Main (main) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.Map qualified as M
import Data.Maybe
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T

import Text.Read (readMaybe)

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.FilePath.Glob
import System.IO.Error
import System.Posix.Process
import System.Posix.Signals
import System.Process

import Config
import GDB
import Network
import Output
import Parser
import Process
import Run.Monad
import Test
import Util
import Version

withVar :: ExprType e => VarName -> e -> TestRun a -> TestRun a
withVar name value = local (fmap $ \s -> s { tsVars = (name, SomeVarValue value) : tsVars s })

withNodePacketLoss :: Node -> Scientific -> TestRun a -> TestRun a
withNodePacketLoss node loss inner = do
    x <- local (fmap $ \s -> s { tsNodePacketLoss = M.insertWith (\l l' -> 1 - (1 - l) * (1 - l')) (nodeName node) loss $ tsNodePacketLoss s }) $ do
        resetLoss
        inner
    resetLoss
    return x
  where
    resetLoss = do
        tl <- asks $ fromMaybe 0 . M.lookup (nodeName node) . tsNodePacketLoss . snd
        liftIO $ callOn node $ "tc qdisc replace dev veth0 root netem loss " <> T.pack (show (tl * 100)) <> "%"
        liftIO $ putStrLn $ "tc qdisc replace dev veth0 root netem loss " <> show (tl * 100) <> "%"

atomicallyTest :: STM a -> TestRun a
atomicallyTest act = do
    failedVar <- asks $ teFailed . fst
    res <- liftIO $ atomically $ do
        readTVar failedVar >>= \case
            Just e  -> return $ Left e
            Nothing -> Right <$> act
    case res of
        Left e -> throwError e
        Right x -> return x

withInternet :: (Network -> TestRun a) -> TestRun a
withInternet inner = do
    testDir <- asks $ optTestDir . teOptions . fst
    inet <- newInternet testDir
    res <- withNetwork (inetRoot inet) $ \net -> do
        local (fmap $ \s -> s { tsNetwork = net }) $ inner net
    delInternet inet
    return res

withSubnet :: Network -> Maybe (TypedVarName Network) -> (Network -> TestRun a) -> TestRun a
withSubnet parent tvname inner = do
    net <- newSubnet parent (fromTypedVarName <$> tvname)
    withNetwork net inner

withNetwork :: Network -> (Network -> TestRun a) -> TestRun a
withNetwork net inner = do
    tcpdump <- liftIO (findExecutable "tcpdump") >>= return . \case
        Just path -> withProcess (Left net) ProcNameTcpdump (Just softwareTermination)
            (path ++ " -i br0 -w '" ++ netDir net ++ "/br0.pcap' -U -Z root") . const
        Nothing -> id

    tcpdump $ inner net

withNode :: Expr Network -> Either (TypedVarName Node) (TypedVarName Process) -> (Node -> TestRun a) -> TestRun a
withNode netexpr tvname inner = do
    net <- eval netexpr
    node <- newNode net (either fromTypedVarName fromTypedVarName tvname)
    either (flip withVar node . fromTypedVarName) (const id) tvname $ inner node

tryMatch :: Regex -> [Text] -> Maybe ((Text, [Text]), [Text])
tryMatch re (x:xs) | Right (Just (_, _, _, capture)) <- regexMatch re x = Just ((x, capture), xs)
                   | otherwise = fmap (x:) <$> tryMatch re xs
tryMatch _ [] = Nothing

exprFailed :: Text -> SourceLine -> Maybe ProcName -> Expr a -> TestRun ()
exprFailed desc (SourceLine sline) pname expr = do
    let prompt = maybe T.empty textProcName pname
    exprVars <- gatherVars expr
    outLine OutputMatchFail (Just prompt) $ T.concat [desc, T.pack " failed on ", sline]
    forM_ exprVars $ \(name, value) ->
        outLine OutputMatchFail (Just prompt) $ T.concat [T.pack "  ", textVarName name, T.pack " = ", textSomeVarValue value]
    throwError Failed

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
                 throwError Failed

             forM_ vars $ \name -> do
                 cur <- asks (lookup name . tsVars . snd)
                 when (isJust cur) $ do
                     outProc OutputError p $ T.pack "variable '" `T.append` textVarName name `T.append` T.pack "' already exists on " `T.append` sline
                     throwError Failed

             outProc OutputMatch p line
             local (fmap $ \s -> s { tsVars = zip vars (map SomeVarValue capture) ++ tsVars s }) inner

         Nothing -> exprFailed (T.pack "expect") (SourceLine sline) (Just $ procName p) expr

testStepGuard :: SourceLine -> Expr Bool -> TestRun ()
testStepGuard sline expr = do
    x <- eval expr
    when (not x) $ exprFailed (T.pack "guard") sline Nothing expr

evalSteps :: [TestStep] -> TestRun ()
evalSteps = mapM_ $ \case
    Let (SourceLine sline) (TypedVarName name) expr inner -> do
        cur <- asks (lookup name . tsVars . snd)
        when (isJust cur) $ do
            outLine OutputError Nothing $ T.pack "variable '" `T.append` textVarName name `T.append` T.pack "' already exists on " `T.append` sline
            throwError Failed
        value <- eval expr
        withVar name value $ evalSteps inner

    For (SourceLine sline) (TypedVarName name) expr inner -> do
        cur <- asks (lookup name . tsVars . snd)
        when (isJust cur) $ do
            outLine OutputError Nothing $ T.pack "variable '" `T.append` textVarName name `T.append` T.pack "' already exists on " `T.append` sline
            throwError Failed
        value <- eval expr
        forM_ value $ \i -> do
            withVar name i $ evalSteps inner

    Subnet name@(TypedVarName vname) parentExpr inner -> do
        parent <- eval parentExpr
        withSubnet parent (Just name) $ \net -> do
            withVar vname net $ evalSteps inner

    DeclNode name@(TypedVarName vname) net inner -> do
        withNode net (Left name) $ \node -> do
            withVar vname node $ evalSteps inner

    Spawn tvname@(TypedVarName vname@(VarName tname)) target inner -> do
        case target of
            Left nname -> withNode RootNetwork (Left nname) go
            Right (Left net) -> withNode net (Right tvname) go
            Right (Right node) -> go =<< eval node
      where
        go node = do
            opts <- asks $ teOptions . fst
            let pname = ProcName tname
                tool = fromMaybe (optDefaultTool opts) (lookup pname $ optProcTools opts)
            withProcess (Right node) pname Nothing tool $ \p -> do
                withVar vname p (evalSteps inner)

    Send pname expr -> do
        p <- eval pname
        line <- eval expr
        outProc OutputChildStdin p line
        send p line

    Expect line pname expr captures inner -> do
        p <- eval pname
        expect line p expr captures $ evalSteps inner

    Guard line expr -> do
        testStepGuard line expr

    PacketLoss loss node inner -> do
        l <- eval loss
        n <- eval node
        withNodePacketLoss n l $ evalSteps inner

    Wait -> do
        void $ outPromptGetLine "Waiting..."

runTest :: Output -> TestOptions -> Test -> IO Bool
runTest out opts test = do
    let testDir = optTestDir opts
    when (optForce opts) $ removeDirectoryRecursive testDir `catchIOError` \e ->
        if isDoesNotExistError e then return () else ioError e
    exists <- doesPathExist testDir
    when exists $ ioError $ userError $ testDir ++ " exists"
    createDirectoryIfMissing True testDir

    failedVar <- newTVarIO Nothing
    procVar <- newMVar []

    mgdb <- if optGDB opts
        then flip runReaderT out $ do
            gdb <- gdbStart $ atomically . writeTVar failedVar . Just . ProcessCrashed
            Just . (, gdbProcess gdb) <$> liftIO (newMVar gdb)
        else return Nothing

    let tenv = TestEnv
            { teOutput = out
            , teFailed = failedVar
            , teOptions = opts
            , teProcesses = procVar
            , teGDB = fst <$> mgdb
            }
        tstate = TestState
            { tsNetwork = error "network not initialized"
            , tsVars = []
            , tsNodePacketLoss = M.empty
            }

    let sigHandler SignalInfo { siginfoSpecific = chld } = do
            processes <- readMVar procVar
            forM_ processes $ \p -> do
                mbpid <- getPid (procHandle p)
                when (mbpid == Just (siginfoPid chld)) $ flip runReaderT out $ do
                    let err detail = outProc OutputChildFail p detail
                    case siginfoStatus chld of
                        Exited ExitSuccess -> outProc OutputChildInfo p $ T.pack $ "child exited successfully"
                        Exited (ExitFailure code) -> do
                            err $ T.pack $ "child process exited with status " ++ show code
                            liftIO $ atomically $ writeTVar (teFailed tenv) $ Just Failed
                        Terminated sig _ -> do
                            err $ T.pack $ "child terminated with signal " ++ show sig
                            liftIO $ atomically $ writeTVar (teFailed tenv) $ Just $ ProcessCrashed p
                        Stopped sig -> err $ T.pack $ "child stopped with signal " ++ show sig
    oldHandler <- installHandler processStatusChanged (CatchInfo sigHandler) Nothing

    res <- runExceptT $ flip runReaderT (tenv, tstate) $ fromTestRun $ do
        withInternet $ \_ -> evalSteps (testSteps test)

    void $ installHandler processStatusChanged oldHandler Nothing

    Right () <- runExceptT $ flip runReaderT out $ do
        maybe (return ()) (closeProcess . snd) mgdb
    [] <- readMVar procVar

    failed <- atomically $ readTVar (teFailed tenv)
    case (res, failed) of
        (Right (), Nothing) -> do
            removeDirectoryRecursive testDir
            return True
        _ -> return False

data CmdlineOptions = CmdlineOptions
    { optTest :: TestOptions
    , optShowVersion :: Bool
    , optVerbose :: Bool
    }

defaultCmdlineOptions :: CmdlineOptions
defaultCmdlineOptions = CmdlineOptions
    { optTest = defaultTestOptions
    , optShowVersion = False
    , optVerbose = False
    }

options :: [OptDescr (CmdlineOptions -> CmdlineOptions)]
options =
    [ Option ['T'] ["tool"]
        (ReqArg (\str -> to $ \opts -> case break (==':') str of
                                            (path, []) -> opts { optDefaultTool = path }
                                            (pname, (_:path)) -> opts { optProcTools = (ProcName (T.pack pname), path) : optProcTools opts }
                ) "PATH")
        "test tool to be used"
    , Option ['v'] ["verbose"]
        (NoArg (\opts -> opts { optVerbose = True }))
        "show output of processes and successful tests"
    , Option ['t'] ["timeout"]
        (ReqArg (\str -> to $ \opts -> case readMaybe str of
                                            Just timeout -> opts { optTimeout = timeout }
                                            Nothing -> error "timeout must be a number") "SECONDS")
        "default timeout in seconds with microsecond precision"
    , Option ['g'] ["gdb"]
        (NoArg $ to $ \opts -> opts { optGDB = True })
        "run GDB and attach spawned processes"
    , Option ['f'] ["force"]
        (NoArg $ to $ \opts -> opts { optForce = True })
        "remove test directory if it exists instead of stopping"
    , Option ['V'] ["version"]
        (NoArg $ \opts -> opts { optShowVersion = True })
        "show version and exit"
    ]
  where
    to f opts = opts { optTest = f (optTest opts) }

main :: IO ()
main = do
    configPath <- findConfig
    config <- mapM parseConfig configPath
    let baseDir = maybe "." dropFileName configPath

    envtool <- lookupEnv "EREBOS_TEST_TOOL" >>= \mbtool ->
        return $ fromMaybe (error "No test tool defined") $ mbtool `mplus` (return . (baseDir </>) =<< configTool =<< config)

    let initOpts = defaultCmdlineOptions
            { optTest = defaultTestOptions
                { optDefaultTool = envtool
                , optTestDir = normalise $ baseDir </> optTestDir defaultTestOptions
                }
            }

    args <- getArgs
    (opts, ofiles) <- case getOpt Permute options args of
        (o, files, []) -> return (foldl (flip id) initOpts o, files)
        (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
            where header = "Usage: erebos-tester [OPTION...]"

    when (optShowVersion opts) $ do
        putStrLn versionLine
        exitSuccess

    getPermissions (head $ words $ optDefaultTool $ optTest opts) >>= \perms -> do
        when (not $ executable perms) $ do
            fail $ optDefaultTool (optTest opts) <> " is not executable"

    files <- if not (null ofiles)
        then return ofiles
        else concat <$> mapM (flip globDir1 baseDir) (maybe [] configTests config)
    when (null files) $ fail $ "No test files"

    out <- startOutput $ optVerbose opts
    ok <- allM (runTest out $ optTest opts) . concat =<< mapM parseTestFile files
    when (not ok) exitFailure
