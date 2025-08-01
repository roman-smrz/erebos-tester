module Run (
    module Run.Monad,
    runTest,
    loadModules,
    evalGlobalDefs,
) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Bifunctor
import Data.Map qualified as M
import Data.Maybe
import Data.Proxy
import Data.Scientific
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T

import System.Directory
import System.Exit
import System.IO.Error
import System.Posix.Process
import System.Posix.Signals
import System.Process

import Text.Megaparsec (errorBundlePretty, showErrorComponent)

import GDB
import Network
import Network.Ip
import Output
import Parser
import Process
import Run.Monad
import Sandbox
import Script.Expr
import Script.Module
import Script.Object
import Script.Shell
import Test
import Test.Builtins


runTest :: Output -> TestOptions -> GlobalDefs -> Test -> IO Bool
runTest out opts gdefs test = do
    let testDir = optTestDir opts
    when (optForce opts) $ removeDirectoryRecursive testDir `catchIOError` \e ->
        if isDoesNotExistError e then return () else ioError e
    exists <- doesPathExist testDir
    when exists $ ioError $ userError $ testDir ++ " exists"
    createDirectoryIfMissing True testDir

    failedVar <- newTVarIO Nothing
    objIdVar <- newMVar 1
    procVar <- newMVar []
    timeoutVar <- newMVar $ optTimeout opts

    mgdb <- if optGDB opts
        then flip runReaderT out $ do
            gdb <- gdbStart $ atomically . writeTVar failedVar . Just . ProcessCrashed
            Just . (, gdbProcess gdb) <$> liftIO (newMVar gdb)
        else return Nothing

    let tenv = TestEnv
            { teOutput = out
            , teFailed = failedVar
            , teOptions = opts
            , teNextObjId = objIdVar
            , teProcesses = procVar
            , teTimeout = timeoutVar
            , teGDB = fst <$> mgdb
            }
        tstate = TestState
            { tsGlobals = gdefs
            , tsLocals = []
            , tsNodePacketLoss = M.empty
            , tsDisconnectedUp = S.empty
            , tsDisconnectedBridge = S.empty
            }

    let sigHandler SignalInfo { siginfoSpecific = chld } = do
            processes <- readMVar procVar
            forM_ processes $ \p -> do
                mbpid <- either getPid (\_ -> return Nothing) (procHandle p)
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

    resetOutputTime out
    testRunResult <- newEmptyMVar

    void $ forkOS $ do
        isolateFilesystem testDir >>= \case
            True -> do
                tres <- runWriterT $ runExceptT $ flip runReaderT (tenv, tstate) $ fromTestRun $ do
                    withInternet $ \_ -> do
                        runStep =<< eval (testSteps test)
                        when (optWait opts) $ do
                            void $ outPromptGetLine $ "Test '" <> testName test <> "' completed, waiting..."
                putMVar testRunResult tres
            _ -> do
                putMVar testRunResult ( Left Failed, [] )

    ( res, [] ) <- takeMVar testRunResult

    void $ installHandler processStatusChanged oldHandler Nothing

    Right () <- runExceptT $ flip runReaderT out $ do
        maybe (return ()) (closeProcess 1 . snd) mgdb
    [] <- readMVar procVar

    failed <- atomically $ readTVar (teFailed tenv)
    case (res, failed) of
        (Right (), Nothing) -> do
            when (not $ optKeep opts) $ removeDirectoryRecursive testDir
            return True
        _ -> do
            flip runReaderT out $ do
                void $ outLine OutputError Nothing $ "Test ‘" <> testName test <> "’ failed."
            return False


loadModules :: [ FilePath ] -> IO ( [ Module ], GlobalDefs )
loadModules files = do
    ( modules, allModules ) <- parseTestFiles files >>= \case
        Right res -> do
            return res
        Left err -> do
            case err of
                ImportModuleError bundle ->
                    putStr (errorBundlePretty bundle)
                _ -> do
                    putStrLn (showErrorComponent err)
            exitFailure
    let globalDefs = evalGlobalDefs $ concatMap (\m -> map (first ( moduleName m, )) $ moduleDefinitions m) allModules
    return ( modules, globalDefs )


evalGlobalDefs :: [ (( ModuleName, VarName ), SomeExpr ) ] -> GlobalDefs
evalGlobalDefs exprs = fix $ \gdefs ->
    builtins `M.union` M.fromList (map (fmap (evalSomeWith gdefs)) exprs)

runBlock :: TestBlock () -> TestRun ()
runBlock EmptyTestBlock = return ()
runBlock (TestBlockStep prev step) = runBlock prev >> runStep step

runStep :: TestStep () -> TestRun ()
runStep = \case
    Scope block -> do
        ( x, objs ) <- censor (const []) $ listen $ catchError (Right <$> runBlock block) (return . Left)
        mapM_ destroySomeObject (reverse objs)
        either throwError return x

    CreateObject (Proxy :: Proxy o) cargs -> do
        objIdVar <- asks (teNextObjId . fst)
        oid <- liftIO $ modifyMVar objIdVar (\x -> return ( x + 1, x ))
        obj <- createObject @TestRun @o (ObjectId oid) cargs
        tell [ toSomeObject obj ]

    Subnet name parent inner -> do
        withSubnet parent (Just name) $ runStep . inner

    DeclNode name net inner -> do
        withNode net (Left name) $ runStep . inner

    Spawn tvname@(TypedVarName (VarName tname)) target args inner -> do
        case target of
            Left net -> withNode net (Right tvname) go
            Right node -> go node
      where
        go node = do
            opts <- asks $ teOptions . fst
            let pname = ProcName tname
                tool = fromMaybe (optDefaultTool opts) (lookup pname $ optProcTools opts)
                cmd = unwords $ tool : map (T.unpack . escape) args
                escape = ("'" <>) . (<> "'") . T.replace "'" "'\\''"
            withProcess (Right node) pname Nothing cmd $ runStep . inner

    SpawnShell mbname node script inner -> do
        let tname | Just (TypedVarName (VarName name)) <- mbname = name
                  | otherwise = "shell"
        let pname = ProcName tname
        withShellProcess node pname script $ runStep . inner

    Send p line -> do
        outProc OutputChildStdin p line
        send p line

    Expect line p expr captures inner -> do
        expect line p expr captures $ runStep . inner

    Flush p regex -> do
        flush p regex

    Guard line vars expr -> do
        testStepGuard line vars expr

    DisconnectNode node inner -> do
        withDisconnectedUp (nodeUpstream node) $ runStep inner

    DisconnectNodes net inner -> do
        withDisconnectedBridge (netBridge net) $ runStep inner

    DisconnectUpstream net inner -> do
        case netUpstream net of
            Just link -> withDisconnectedUp link $ runStep inner
            Nothing -> runStep inner

    PacketLoss loss node inner -> do
        withNodePacketLoss node loss $ runStep inner

    Wait -> do
        void $ outPromptGetLine "Waiting..."


withInternet :: (Network -> TestRun a) -> TestRun a
withInternet inner = do
    testDir <- asks $ optTestDir . teOptions . fst
    inet <- newInternet testDir
    res <- withNetwork (inetRoot inet) $ \net -> do
        withTypedVar rootNetworkVar net $ do
            inner net
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
            (path ++ " -i br0 -w './br0.pcap' -U -Z root") . const
        Nothing -> id

    tcpdump $ inner net

withNode :: Network -> Either (TypedVarName Node) (TypedVarName Process) -> (Node -> TestRun a) -> TestRun a
withNode net tvname inner = do
    node <- newNode net (either fromTypedVarName fromTypedVarName tvname)
    either (flip withVar node . fromTypedVarName) (const id) tvname $ inner node

withDisconnectedUp :: Link VEth -> TestRun a -> TestRun a
withDisconnectedUp link inner = do
    let netns = getNetns link
    disconnected <- asks $ S.member netns . tsDisconnectedUp . snd
    if disconnected
      then inner
      else do
        local (fmap $ \s -> s { tsDisconnectedUp = S.insert netns $ tsDisconnectedUp s }) $ do
            atomicallyWithIO $ linkDown link
            x <- inner
            atomicallyWithIO $ linkUp link
            return x

withDisconnectedBridge :: Link Bridge -> TestRun a -> TestRun a
withDisconnectedBridge bridge inner = do
    let netns = getNetns bridge
    disconnected <- asks $ S.member netns . tsDisconnectedBridge . snd
    if disconnected
      then inner
      else do
        local (fmap $ \s -> s { tsDisconnectedBridge = S.insert netns $ tsDisconnectedBridge s }) $ do
            atomicallyWithIO $ linkDown bridge
            x <- inner
            atomicallyWithIO $ linkUp bridge
            return x

withNodePacketLoss :: Node -> Scientific -> TestRun a -> TestRun a
withNodePacketLoss node loss inner = do
    x <- local (fmap $ \s -> s { tsNodePacketLoss = M.insertWith (\l l' -> 1 - (1 - l) * (1 - l')) (getNetns node) loss $ tsNodePacketLoss s }) $ do
        resetLoss
        inner
    resetLoss
    return x
  where
    resetLoss = do
        tl <- asks $ fromMaybe 0 . M.lookup (getNetns node) . tsNodePacketLoss . snd
        liftIO $ callOn node $ "tc qdisc replace dev veth0 root netem loss " <> T.pack (show (tl * 100)) <> "%"


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

tryMatch :: Regex -> [Text] -> Maybe ((Text, [Text]), [Text])
tryMatch re (x:xs) | Right (Just (_, _, _, capture)) <- regexMatch re x = Just ((x, capture), xs)
                   | otherwise = fmap (x:) <$> tryMatch re xs
tryMatch _ [] = Nothing

exprFailed :: Text -> SourceLine -> Maybe ProcName -> EvalTrace -> TestRun ()
exprFailed desc sline pname exprVars = do
    let prompt = maybe T.empty textProcName pname
    outLine OutputMatchFail (Just prompt) $ T.concat [desc, T.pack " failed on ", textSourceLine sline]
    forM_ exprVars $ \((name, sel), value) ->
        outLine OutputMatchFail (Just prompt) $ T.concat
            [ "  ", textFqVarName name, T.concat (map ("."<>) sel)
            , " = ", textSomeVarValue sline value
            ]
    throwError Failed

expect :: SourceLine -> Process -> Traced Regex -> [TypedVarName Text] -> ([ Text ] -> TestRun ()) -> TestRun ()
expect sline p (Traced trace re) tvars inner = do
    timeout <- liftIO . readMVar =<< asks (teTimeout . fst)
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
                 outProc OutputMatchFail p $ T.pack "mismatched number of capture variables on " `T.append` textSourceLine sline
                 throwError Failed

             outProc OutputMatch p line
             inner capture

         Nothing -> exprFailed (T.pack "expect") sline (Just $ procName p) trace

flush :: Process -> Maybe Regex -> TestRun ()
flush p mbre = do
    atomicallyTest $ do
        writeTVar (procOutput p) =<< case mbre of
            Nothing -> return []
            Just re -> filter (either error isNothing . regexMatch re) <$> readTVar (procOutput p)

testStepGuard :: SourceLine -> EvalTrace -> Bool -> TestRun ()
testStepGuard sline vars x = do
    when (not x) $ exprFailed (T.pack "guard") sline Nothing vars
