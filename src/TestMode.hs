{-# LANGUAGE CPP #-}

module TestMode (
    testMode,
) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Bifunctor
import Data.List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import System.IO.Error

import Text.Megaparsec.Error
import Text.Megaparsec.Pos

import Output
import Parser
import Run
import Script.Expr
import Script.Module
import Test


data TestModeInput = TestModeInput
    { tmiOutput :: Output
    , tmiParams :: [ Text ]
    }

data TestModeState = TestModeState
    { tmsModules :: [ Module ]
    , tmsGlobals :: GlobalDefs
    }

initTestModeState :: TestModeState
initTestModeState = TestModeState
    { tmsModules = mempty
    , tmsGlobals = mempty
    }

testMode :: IO ()
testMode = do
    out <- startOutput OutputStyleTest False
    let testLoop = getLineMb >>= \case
            Just line -> do
                case T.words line of
                    cname : params
                        | Just (CommandM cmd) <- lookup cname commands -> do
                            runReaderT cmd $ TestModeInput out params
                        | otherwise -> fail $ "Unknown command '" ++ T.unpack cname ++ "'"
                    [] -> return ()
                testLoop

            Nothing -> return ()

    runExceptT (evalStateT testLoop initTestModeState) >>= \case
        Left err -> flip runReaderT out $ outLine OutputError Nothing $ T.pack err
        Right () -> return ()

getLineMb :: MonadIO m => m (Maybe Text)
getLineMb = liftIO $ catchIOError (Just <$> T.getLine) (\e -> if isEOFError e then return Nothing else ioError e)

cmdOut :: Text -> Command
cmdOut line = do
    out <- asks tmiOutput
    flip runReaderT out $ outLine OutputTestRaw Nothing line


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
    , ( "run", cmdRun )
    ]

cmdLoad :: Command
cmdLoad = do
    [ path ] <- asks tmiParams
    liftIO (parseTestFiles [ T.unpack path ]) >>= \case
        Right ( modules, allModules ) -> do
            let globalDefs = evalGlobalDefs $ concatMap (\m -> map (first ( moduleName m, )) $ moduleDefinitions m) allModules
            modify $ \s -> s
                { tmsModules = modules
                , tmsGlobals = globalDefs
                }
            cmdOut "load-done"

        Left (ModuleNotFound moduleName) -> do
            cmdOut $ "load-failed module-not-found" <> textModuleName moduleName
        Left (FileNotFound notFoundPath) -> do
            cmdOut $ "load-failed file-not-found " <> T.pack notFoundPath
        Left (ImportModuleError bundle) -> do
#if MIN_VERSION_megaparsec(9,7,0)
            mapM_ (cmdOut . T.pack) $ lines $ errorBundlePrettyWith showParseError bundle
#endif
            cmdOut $ "load-failed parse-error"
  where
    showParseError _ SourcePos {..} _ = concat
        [ "parse-error"
        , " ", sourceName
        , ":", show $ unPos sourceLine
        , ":", show $ unPos sourceColumn
        ]

cmdRun :: Command
cmdRun = do
    [ name ] <- asks tmiParams
    TestModeState {..} <- get
    case find ((name ==) . testName) $ concatMap moduleTests tmsModules of
        Nothing -> cmdOut "run-not-found"
        Just test -> do
            out <- asks tmiOutput
            liftIO (runTest out defaultTestOptions tmsGlobals test) >>= \case
                True -> cmdOut "run-done"
                False -> cmdOut "run-failed"
