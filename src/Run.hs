module Run (
    module Run.Monad,
    Report(..), SingleTestReport(..),
    TestName, textTestName,
    runTests,

    LoadedModules(..),
    loadModules',
    evalGlobalDefs,

    TestFilter(..),
    testFilterFromConfig,
    filterTests,
) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Char
import Data.Either
import Data.List
import Data.Map qualified as M
import Data.Maybe
import Data.Proxy
import Data.Scientific
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable

import System.Directory
import System.FilePath
import System.Exit
import System.IO.Error
import System.Posix.Process
import System.Posix.Signals
import System.Process

import Config
import Error
import GDB
import Network
import Network.Ip
import Output
import Parser
import Process
import Process.Signal
import Run.Monad
import Sandbox
import Script.Expr
import Script.Module
import Script.Object
import Script.Shell
import Test
import Test.Builtins


data Report = Report
    { reportTotalCount :: Int
    , reportPassedCount :: Int
    , reportSkippedCount :: Int
    , reportFailedCount :: Int
    , reportFailedList :: [ TestName ]
    , reportTotalTime :: Scientific
    , reportTests :: [ SingleTestReport ]
    }

data SingleTestReport = SingleTestReport
    { reportTestName :: TestName
    , reportTestFailed :: Maybe Failed
    , reportTime :: Scientific
    , reportOutput :: Text
    , reportOutputError :: Text
    }

runTests :: Output -> TestOptions -> GlobalDefs -> [ Test ] -> IO Report
runTests teOutput teOptions gdefs tests = do
    let evalTool = \case
            ToolPath path -> return path
            ToolCabal arg
                | (name : args) <- T.words arg
                -> do
                    readCreateProcessWithExitCode (proc "cabal" $ [ "list-bin", T.unpack name ]) "" >>= \case
                        ( ExitSuccess, cabalOut, _ ) -> do
                            let path = takeWhile (/= '\n') cabalOut
                            return $ T.unpack $ T.unwords (T.pack path : args)
                        _ -> do
                            throwOtherError $ "failed to find ‘" <> name <> "’ executable path using cabal"
                | otherwise
                -> return $ T.unpack arg
    teDefaultTool <- case optDefaultTool teOptions of
        Just tool -> evalTool tool
        Nothing -> throwOtherError "no test tool defined"
    teProcTools <- mapM (sequence . fmap evalTool) $ optProcTools teOptions

    let env = GlobalTestEnv {..}
    let go (t : ts) = do
            single <- runTest env gdefs t
            let failed = isJust (reportTestFailed single)
            r <- if
                | failed && not (optKeepGoing teOptions)
                -> go []
                | otherwise
                -> go ts
            return Report
                { reportTotalCount = 1 + reportTotalCount r
                , reportPassedCount = (if failed then 0 else 1) + reportPassedCount r
                , reportSkippedCount = reportSkippedCount r
                , reportFailedCount = (if failed then 1 else 0) + reportFailedCount r
                , reportFailedList = (if failed then (reportTestName single :) else id) $ reportFailedList r
                , reportTotalTime = reportTime single + reportTotalTime r
                , reportTests = single : reportTests r
                }
        go [] = return Report
            { reportTotalCount = 0
            , reportPassedCount = 0
            , reportSkippedCount = 0
            , reportFailedCount = 0
            , reportFailedList = []
            , reportTotalTime = 0
            , reportTests = []
            }
    go $ concat $ replicate (optRepeat teOptions) tests


runTest :: GlobalTestEnv -> GlobalDefs -> Test -> IO SingleTestReport
runTest teGlobal@GlobalTestEnv {..} gdefs test = do
    let teTestDir = optTestDir teOptions </> T.unpack (textTestName $ testName test)
    when (optForce teOptions) $ removeDirectoryRecursive teTestDir `catchIOError` \e ->
        if isDoesNotExistError e then return () else ioError e
    exists <- doesPathExist teTestDir
    when exists $ throwOtherError $ "test directory ‘" <> T.pack teTestDir <> "’ already exists"
    createDirectoryIfMissing True teTestDir

    teFailed <- newTVarIO Nothing
    teNextObjId <- newMVar 1
    teNextProcId <- newMVar 1
    teProcesses <- newMVar []
    teTimeout <- newMVar ( fromMaybe 1 $ optTimeout teOptions, 0 )

    mgdb <- if optGDB teOptions
        then flip runReaderT teOutput $ do
            gdb <- gdbStart $ atomically . writeTVar teFailed . Just . ProcessCrashed
            Just . (, gdbProcess gdb) <$> liftIO (newMVar gdb)
        else return Nothing

    let tenv = SingleTestEnv
            { teGDB = fst <$> mgdb
            , ..
            }
        tstate = TestState
            { tsGlobals = gdefs
            , tsLocals = [ ( callStackVarName, SomeExpr $ Pure $ CallStack [] ) ]
            , tsNodePacketLoss = M.empty
            , tsDisconnectedUp = S.empty
            , tsDisconnectedBridge = S.empty
            }

    let sigHandler SignalInfo { siginfoSpecific = NoSignalSpecificInfo } = return ()
        sigHandler SignalInfo { siginfoSpecific = chld } = do
            processes <- readMVar teProcesses
            forM_ processes $ \p -> do
                mbpid <- either getPid (\_ -> return Nothing) (procHandle p)
                when (mbpid == Just (siginfoPid chld)) $ flip runReaderT teOutput $ do
                    let err detail = outProc OutputChildFail p detail
                    case siginfoStatus chld of
                        Exited ExitSuccess -> outProc OutputChildInfo p $ T.pack $ "child exited successfully"
                        Exited (ExitFailure code) -> do
                            err $ T.pack $ "child process exited with status " ++ show code
                            liftIO $ atomically $ writeTVar teFailed $ Just Failed
                        Terminated sig _ -> do
                            err $ T.pack $ "child terminated with signal " ++ show sig
                            liftIO $ atomically $ writeTVar teFailed $ Just $ ProcessCrashed p
                        Stopped sig -> err $ T.pack $ "child stopped with signal " ++ show sig
    oldHandler <- installHandler processStatusChanged (CatchInfo sigHandler) Nothing

    flip runReaderT teOutput $ do
        void $ outLine OutputGlobalInfo Nothing $ "Starting test ‘" <> textTestName (testName test) <> "’"

    resetOutputTime teOutput
    testRunResult <- newEmptyMVar

    void $ forkOS $ do
        isolateFilesystem teTestDir >>= \case
            True -> do
                tres <- runWriterT $ runExceptT $ flip runReaderT (tenv, tstate) $ fromTestRun $ do
                    withInternet $ \_ -> do
                        runStep =<< eval (testSteps test)
                        when (optWait teOptions) $ do
                            void $ outPromptGetLine $ "Test ‘" <> textTestName (testName test) <> "’ completed, waiting..."
                putMVar testRunResult tres
            _ -> do
                putMVar testRunResult ( Left Failed, [] )

    ( res, [] ) <- takeMVar testRunResult
    reportTime <- getElapsedTime teOutput
    reportOutput <- collectOutput teOutput
    reportOutputError <- collectErrorOutput teOutput

    void $ installHandler processStatusChanged oldHandler Nothing

    Right () <- runExceptT $ flip runReaderT teOutput $ do
        maybe (return ()) (closeProcess 1 . snd) mgdb
    [] <- readMVar teProcesses

    failed <- atomically $ readTVar teFailed
    reportTestFailed <- case ( res, failed ) of
        ( Right (), Nothing ) -> do
            when (not $ optKeep teOptions) $ removeDirectoryRecursive teTestDir
            return Nothing
        _ -> do
            flip runReaderT teOutput $ do
                void $ outLine OutputGlobalError Nothing $ "Test ‘" <> textTestName (testName test) <> "’ failed."
            return $ either Just (const Nothing) res `mplus` failed

    (optHookTestResult teOptions) (testName test) (isNothing reportTestFailed)
    let reportTestName = testName test
    return SingleTestReport {..}


data LoadedModules = LoadedModules
    { lmModules :: [ Module ]
    , lmTags :: [ ( TestName, [ Tag ] ) ]
    , lmGlobalDefs :: GlobalDefs
    }

loadModules' :: [ SomePrimType ] -> [ ( FilePath, Maybe Text ) ] -> IO (Either CustomTestError LoadedModules)
loadModules' builtinTypes files = do
    parseTestFiles builtinTypes (map fst files) >>= \case
        Right ( modules, allModules ) -> return $ do
            lmModules <- forM (zip files modules) $ \( ( path, tsel ), m ) -> do
                tests <- case tsel of
                    Nothing -> return $ moduleTests m
                    Just tname
                        | Just test <- find ((tname ==) . testNameBase . testName) (moduleTests m)
                        -> return [ test ]
                        | otherwise
                        -> throwError $ TestNotFound tname (Just path)
                return m { moduleTests = tests }

            let lmGlobalDefs = evalGlobalDefs $ concatMap (\m -> map (first ( moduleName m, )) $ moduleDefinitions m) allModules
                evalTags test = map (\e -> runSimpleEval (eval e) lmGlobalDefs []) $ testTags test
                lmTags = concatMap (\Module {..} -> map (\test -> ( testName test, evalTags test )) moduleTests) lmModules
            Right $ LoadedModules {..}
        Left err -> do
            return $ Left err


evalGlobalDefs :: [ (( ModuleName, VarName ), SomeExpr ) ] -> GlobalDefs
evalGlobalDefs exprs = builtins `M.union` M.fromList exprs


data TestFilter = TestFilter
    { tfSelect :: Maybe [ Text ]
    , tfExclude :: [ Text ]
    }

instance Semigroup TestFilter where
    a <> b
        | isJust (tfSelect b) = b
        | otherwise = a { tfExclude = tfExclude a <> tfExclude b }

instance Monoid TestFilter where
    mempty = TestFilter Nothing []

testFilterFromConfig :: Config -> TestFilter
testFilterFromConfig Config {..} = TestFilter
    { tfSelect = configSelect
    , tfExclude = configExclude
    }

filterTests :: TestFilter -> LoadedModules -> Either CustomTestError [ Test ]
filterTests TestFilter {..} LoadedModules {..} = do
    let allTests = concatMap moduleTests lmModules
    let evalTerm :: Text -> Either CustomTestError (Either TestName (Either Tag ModuleName))
        evalTerm term =
            case (init &&& last) $ T.splitOn "." term of
                ( [], name ) | maybe False (isUpper . fst) (T.uncons name) ->
                    case find ((VarName name ==) . snd . fst) $ M.toList lmGlobalDefs of
                        Just ( _, SomeExpr (expr :: Expr etype) )
                            | Just (Refl :: etype :~: Tag) <- eqT
                            -> return $ Right $ Left $ runSimpleEval (eval expr) lmGlobalDefs []
                        Nothing
                            | Just t <- find ((name ==) . testNameBase . testName) allTests
                            -> return $ Left $ testName t
                        Nothing
                            | mname <- ModuleName [ name ]
                            , Just _ <- find ((mname ==) . moduleName) lmModules
                            -> return $ Right $ Right mname
                        _ ->
                            throwError $ TestOrTagNotFound term Nothing
                ( ms, name ) | maybe False (isUpper . fst) (T.uncons name) ->
                    case find ((( ModuleName ms, VarName name ) ==) . fst) $ M.toList lmGlobalDefs of
                        Just ( _, SomeExpr (expr :: Expr etype) )
                            | Just (Refl :: etype :~: Tag) <- eqT
                            -> return $ Right $ Left $ runSimpleEval (eval expr) lmGlobalDefs []
                        Nothing
                            | Just t <- find ((TestName (ModuleName ms) name ==) . testName) allTests
                            -> return $ Left $ testName t
                        Nothing
                            | mname <- ModuleName $ ms ++ [ name ]
                            , Just _ <- find ((mname ==) . moduleName) lmModules
                            -> return $ Right $ Right mname
                        _ ->
                            throwError $ TestOrTagNotFound term Nothing
                ( ms, name ) | mname <- ModuleName $ ms ++ [ name ] ->
                    case find ((mname ==) . moduleName) lmModules of
                        Just _ -> return $ Right $ Right mname
                        _ -> throwError $ ModuleNotFound mname

    exclude <- fmap partitionEithers . partitionEithers <$> mapM evalTerm tfExclude
    let matches ( tnames, ( tags, modules ) ) test =
            testName test `elem` tnames
                || maybe False (any (`elem` tags)) (lookup (testName test) lmTags)
                || testNameModule (testName test) `elem` modules
    filter (not . matches exclude) <$> case tfSelect of
        Nothing -> return allTests
        Just tnames -> do
            selected <- fmap partitionEithers . partitionEithers <$> mapM evalTerm tnames
            return $ filter (matches selected) allTests


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

    Spawn tvname@(TypedVarName (VarName tname)) target args killWith inner -> do
        case target of
            Left net -> withNode net (Right tvname) go
            Right node -> go node
      where
        go node = do
            env <- asks $ teGlobal . fst
            let pname = ProcName tname
                ( tool, cargs ) = fromMaybe ( "", [] ) $ uncons $ words $
                    fromMaybe (teDefaultTool env) (lookup pname $ teProcTools env)
                args' = cargs ++ map T.unpack args
                cmd = T.unwords $ T.pack tool : map escape (map T.pack args')
                escape = ("'" <>) . (<> "'") . T.replace "'" "'\\''"
            outProcName OutputChildExec pname cmd
            withProcess (Right node) pname killWith tool args' $ runStep . inner

    SpawnShell mbname node script inner -> do
        let tname | Just (TypedVarName (VarName name)) <- mbname = name
                  | otherwise = "shell"
        let pname = ProcName tname
        withShellProcess node pname script $ runStep . inner

    Send p line -> do
        outProc OutputChildStdin p line
        send p line

    Expect stack line p expr timeout captures inner -> do
        expect stack line p expr timeout captures $ runStep . inner

    Flush p regex -> do
        mapM_ (outProc OutputIgnored p) =<<
            atomicallyTest (flushProcessOutput p regex)

    Guard stack expr -> do
        testStepGuard stack expr

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
    testDir <- asks $ teTestDir . fst
    inet <- newInternet testDir
    flip finally (delInternet inet) $ do
        withNetwork (inetRoot inet) $ \net -> do
            withTypedVar rootNetworkVar net $ do
                inner net

withSubnet :: Network -> Maybe (TypedVarName Network) -> (Network -> TestRun a) -> TestRun a
withSubnet parent tvname inner = do
    net <- newSubnet parent (fromTypedVarName <$> tvname)
    withNetwork net inner

withNetwork :: Network -> (Network -> TestRun a) -> TestRun a
withNetwork net inner = do
    tcpdump <- asks (optTcpdump . teOptions . teGlobal . fst) >>= return . \case
        Just path -> withProcess (Left net) ProcNameTcpdump (Just (Signal softwareTermination))
            path [ "-i", "br0", "-w", "./br0.pcap", "-U", "-Z", "root" ] . const
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

exprFailed :: Text -> CallStack -> Maybe ProcName -> TestRun ()
exprFailed desc stack pname = do
    let prompt = maybe T.empty textProcName pname
    outLine (OutputMatchFail stack) (Just prompt) $ desc <> " failed"
    throwError Failed

expect :: CallStack -> SourceLine -> Process -> Traced Regex -> Scientific -> [ TypedVarName Text ] -> ([ Text ] -> TestRun ()) -> TestRun ()
expect (CallStack cs) sline p (Traced trace re) etimeout tvars inner = do
    let stack = CallStack (( sline, trace ) : cs)
    timeout <- (etimeout *) <$> getCurrentTimeout
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
                 outProc (OutputMatchFail stack) p $ T.pack "mismatched number of capture variables"
                 throwError Failed

             outProc OutputMatch p line
             inner capture

         Nothing -> exprFailed (T.pack "expect") stack (Just $ procName p)

testStepGuard :: CallStack -> Bool -> TestRun ()
testStepGuard stack x = do
    when (not x) $ exprFailed (T.pack "guard") stack Nothing
