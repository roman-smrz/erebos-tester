module Run (
    module Run.Monad,
    runTest,
) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T

import System.Directory
import System.Exit
import System.IO.Error
import System.Posix.Process
import System.Posix.Signals
import System.Process

import GDB
import Network
import Network.Ip
import Output
import Process
import Run.Monad
import Test
import Test.Builtins

runTest :: Output -> TestOptions -> Test -> [ ( VarName, SomeVarValue ) ] -> IO Bool
runTest out opts test variables = do
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
            , tsVars = builtins ++ variables
            , tsNodePacketLoss = M.empty
            , tsDisconnectedUp = S.empty
            , tsDisconnectedBridge = S.empty
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
        withInternet $ \_ -> do
            evalSteps (testSteps test)
            when (optWait opts) $ do
                void $ outPromptGetLine $ "Test '" <> testName test <> "' completed, waiting..."

    void $ installHandler processStatusChanged oldHandler Nothing

    Right () <- runExceptT $ flip runReaderT out $ do
        maybe (return ()) (closeProcess . snd) mgdb
    [] <- readMVar procVar

    failed <- atomically $ readTVar (teFailed tenv)
    case (res, failed) of
        (Right (), Nothing) -> do
            when (not $ optKeep opts) $ removeDirectoryRecursive testDir
            return True
        _ -> return False

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

    ExprStatement expr -> do
        TestBlock steps <- eval expr
        evalSteps steps

    Subnet name@(TypedVarName vname) parentExpr inner -> do
        parent <- eval parentExpr
        withSubnet parent (Just name) $ \net -> do
            withVar vname net $ evalSteps inner

    DeclNode name@(TypedVarName vname) net inner -> do
        withNode net (Left name) $ \node -> do
            withVar vname node $ evalSteps inner

    Spawn tvname@(TypedVarName vname@(VarName tname)) target inner -> do
        case target of
            Left net -> withNode net (Right tvname) go
            Right node -> go =<< eval node
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

    Flush pname expr -> do
        p <- eval pname
        flush p expr

    Guard line expr -> do
        testStepGuard line expr

    DisconnectNode node inner -> do
        n <- eval node
        withDisconnectedUp (nodeUpstream n) $ evalSteps inner

    DisconnectNodes net inner -> do
        n <- eval net
        withDisconnectedBridge (netBridge n) $ evalSteps inner

    DisconnectUpstream net inner -> do
        n <- eval net
        case netUpstream n of
            Just link -> withDisconnectedUp link $ evalSteps inner
            Nothing -> evalSteps inner

    PacketLoss loss node inner -> do
        l <- eval loss
        n <- eval node
        withNodePacketLoss n l $ evalSteps inner

    Wait -> do
        void $ outPromptGetLine "Waiting..."


withVar :: ExprType e => VarName -> e -> TestRun a -> TestRun a
withVar name value = local (fmap $ \s -> s { tsVars = ( name, SomeVarValue mempty $ const $ const value ) : tsVars s })

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

exprFailed :: Text -> SourceLine -> Maybe ProcName -> Expr a -> TestRun ()
exprFailed desc (SourceLine sline) pname expr = do
    let prompt = maybe T.empty textProcName pname
    exprVars <- gatherVars expr
    outLine OutputMatchFail (Just prompt) $ T.concat [desc, T.pack " failed on ", sline]
    forM_ exprVars $ \((name, sel), value) ->
        outLine OutputMatchFail (Just prompt) $ T.concat
            [ "  ", textVarName name, T.concat (map ("."<>) sel)
            , " = ", textSomeVarValue (SourceLine sline) value
            ]
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
             local (fmap $ \s -> s { tsVars = zip vars (map (SomeVarValue mempty . const . const) capture) ++ tsVars s }) inner

         Nothing -> exprFailed (T.pack "expect") (SourceLine sline) (Just $ procName p) expr

flush :: Process -> Maybe (Expr Regex) -> TestRun ()
flush p mbexpr = do
    mbre <- sequence $ fmap eval mbexpr
    atomicallyTest $ do
        writeTVar (procOutput p) =<< case mbre of
            Nothing -> return []
            Just re -> filter (either error isNothing . regexMatch re) <$> readTVar (procOutput p)

testStepGuard :: SourceLine -> Expr Bool -> TestRun ()
testStepGuard sline expr = do
    x <- eval expr
    when (not x) $ exprFailed (T.pack "guard") sline Nothing expr
