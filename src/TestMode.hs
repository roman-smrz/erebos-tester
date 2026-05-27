{-# LANGUAGE CPP #-}

module TestMode (
    testMode,
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import System.IO.Error

import Text.Megaparsec.Error
import Text.Megaparsec.Pos

import Config
import Output
import Parser
import Run
import Script.Expr
import Test


data TestModeInput = TestModeInput
    { tmiOutput :: Output
    , tmiConfig :: Maybe Config
    , tmiParams :: [ Text ]
    }

data TestModeState = TestModeState
    { tmsModules :: Maybe LoadedModules
    , tmsNextTestNumber :: Int
    }

initTestModeState :: TestModeState
initTestModeState = TestModeState
    { tmsModules = Nothing
    , tmsNextTestNumber = 1
    }

testMode :: Maybe Config -> IO ()
testMode tmiConfig = do
    tmiOutput <- startOutput OutputStyleTest False
    let testLoop = getLineMb >>= \case
            Just line -> do
                case T.words line of
                    cname : tmiParams
                        | Just (CommandM cmd) <- lookup cname commands -> do
                            runReaderT cmd $ TestModeInput {..}
                        | otherwise -> fail $ "Unknown command '" ++ T.unpack cname ++ "'"
                    [] -> return ()
                testLoop

            Nothing -> return ()

    runExceptT (evalStateT testLoop initTestModeState) >>= \case
        Left err -> flip runReaderT tmiOutput $ outLine OutputError Nothing $ T.pack err
        Right () -> return ()

getLineMb :: MonadIO m => m (Maybe Text)
getLineMb = liftIO $ catchIOError (Just <$> T.getLine) (\e -> if isEOFError e then return Nothing else ioError e)

cmdOut :: Text -> Command
cmdOut line = do
    out <- asks tmiOutput
    flip runReaderT out $ outLine OutputTestRaw Nothing line

getNextTestNumber :: CommandM Int
getNextTestNumber = do
    num <- gets tmsNextTestNumber
    modify $ \s -> s { tmsNextTestNumber = num + 1 }
    return num

runSingleTest :: Test -> CommandM Bool
runSingleTest test = do
    out <- asks tmiOutput
    num <- getNextTestNumber
    Just LoadedModules {..} <- gets tmsModules
    mbconfig <- asks tmiConfig
    let opts = defaultTestOptions
            { optDefaultTool = fromMaybe "/bin/true" $ configTool =<< mbconfig
            , optTestDir = ".test" <> show num
            , optKeep = True
            }
    liftIO (runTest out opts lmGlobalDefs test)


newtype CommandM a = CommandM (ReaderT TestModeInput (StateT TestModeState (ExceptT String IO)) a)
    deriving
    ( Functor, Applicative, Monad, MonadIO
    , MonadReader TestModeInput, MonadState TestModeState, MonadError String
    )

instance MonadFail CommandM where
    fail = throwError

type Command = CommandM ()

commands :: [ ( Text, Command ) ]
commands =
    [ ( "load", cmdLoad )
    , ( "load-config", cmdLoadConfig )
    , ( "run", cmdRun )
    ]

showError :: Text -> CustomTestError -> Command
showError prefix = \case
    ModuleNotFound moduleName -> do
        cmdOut $ prefix <> " module-not-found" <> textModuleName moduleName
    FileNotFound notFoundPath -> do
        cmdOut $ prefix <> " file-not-found " <> T.pack notFoundPath
    TestNotFound tname mbfile -> do
        cmdOut $ prefix <> " test-not-found " <> tname <> maybe "" ((" " <>) . T.pack) mbfile
    TestOrTagNotFound tname mbfile -> do
        cmdOut $ prefix <> " test-or-tag-not-found " <> tname <> maybe "" ((" " <>) . T.pack) mbfile
    ImportModuleError bundle -> do
#if MIN_VERSION_megaparsec(9,7,0)
        mapM_ (cmdOut . T.pack) $ lines $ errorBundlePrettyWith showParseError bundle
#endif
        cmdOut $ prefix <> " parse-error"
  where
    showParseError _ SourcePos {..} _ = concat
        [ "parse-error"
        , " ", sourceName
        , ":", show $ unPos sourceLine
        , ":", show $ unPos sourceColumn
        ]

cmdLoad :: Command
cmdLoad = do
    [ path ] <- asks tmiParams
    liftIO (loadModules [ ( T.unpack path, Nothing ) ]) >>= \case
        Right modules -> do
            modify $ \s -> s { tmsModules = Just modules }
            cmdOut "load-done"
        Left err -> showError "load-failed" err

cmdLoadConfig :: Command
cmdLoadConfig = do
    Just config <- asks tmiConfig
    liftIO (getConfigTestFiles config >>= loadModules . (map (, Nothing ))) >>= \case
        Right modules -> do
            modify $ \s -> s { tmsModules = Just modules }
            cmdOut "load-config-done"
        Left err -> showError "load-config-failed" err

cmdRun :: Command
cmdRun = do
    params <- asks tmiParams
    let ( select, exclude ) = fmap (map (T.drop 1)) $ partition (("^" /=) . T.take 1) params
        pfilter = (TestFilter (if select == [ "*" ] then Nothing else Just select) exclude)
    cfilter <- asks $ maybe mempty testFilterFromConfig . tmiConfig
    Just lm <- gets tmsModules
    case filterTests (cfilter <> pfilter) lm of
        Left err -> showError "run-failed" err
        Right tests -> do
            forM_ tests $ \test -> do
                res <- runSingleTest test
                cmdOut $ "run-test-result " <> testName test <> " " <> (if res then "done" else "failed")
            cmdOut "run-done"
