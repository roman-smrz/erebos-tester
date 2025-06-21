module Main (main) where

import Control.Monad

import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T

import Text.Read (readMaybe)

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Terminal
import System.Posix.Types

import Config
import Output
import Process
import Run
import Script.Module
import Test
import TestMode
import Util
import Version

data CmdlineOptions = CmdlineOptions
    { optTest :: TestOptions
    , optRepeat :: Int
    , optExclude :: [ Text ]
    , optVerbose :: Bool
    , optColor :: Maybe Bool
    , optShowHelp :: Bool
    , optShowVersion :: Bool
    , optTestMode :: Bool
    }

defaultCmdlineOptions :: CmdlineOptions
defaultCmdlineOptions = CmdlineOptions
    { optTest = defaultTestOptions
    , optRepeat = 1
    , optExclude = []
    , optVerbose = False
    , optColor = Nothing
    , optShowHelp = False
    , optShowVersion = False
    , optTestMode = False
    }

options :: [ OptDescr (CmdlineOptions -> CmdlineOptions) ]
options =
    [ Option ['T'] ["tool"]
        (ReqArg (\str -> to $ \opts -> case break (==':') str of
                                            (path, []) -> opts { optDefaultTool = path }
                                            (pname, (_:path)) -> opts { optProcTools = (ProcName (T.pack pname), path) : optProcTools opts }
                ) "<path>")
        "test tool to be used"
    , Option ['v'] ["verbose"]
        (NoArg (\opts -> opts { optVerbose = True }))
        "show output of processes and successful tests"
    , Option [] [ "color" ]
        (NoArg (\opts -> opts { optColor = Just True }))
        "always use colors for output (default when stdout is tty)"
    , Option [] [ "no-color" ]
        (NoArg (\opts -> opts { optColor = Just False }))
        "never use colors for output (default when stdout is not a tty)"
    , Option ['t'] ["timeout"]
        (ReqArg (\str -> to $ \opts -> case readMaybe str of
                                            Just timeout -> opts { optTimeout = timeout }
                                            Nothing -> error "timeout must be a number") "<seconds>")
        "default timeout in seconds with microsecond precision"
    , Option ['g'] ["gdb"]
        (NoArg $ to $ \opts -> opts { optGDB = True })
        "run GDB and attach spawned processes"
    , Option ['f'] ["force"]
        (NoArg $ to $ \opts -> opts { optForce = True })
        "remove test directory if it already exists instead of stopping"
    , Option ['k'] ["keep"]
        (NoArg $ to $ \opts -> opts { optKeep = True })
        "keep test directory even if all tests succeed"
    , Option ['r'] ["repeat"]
        (ReqArg (\str opts -> opts { optRepeat = read str }) "<count>")
        "number of times to repeat the test(s)"
    , Option [ 'e' ] [ "exclude" ]
        (ReqArg (\str opts -> opts { optExclude = T.pack str : optExclude opts }) "<test>")
        "exclude given test from execution"
    , Option [] ["wait"]
        (NoArg $ to $ \opts -> opts { optWait = True })
        "wait at the end of each test"
    , Option ['h'] ["help"]
        (NoArg $ \opts -> opts { optShowHelp = True })
        "show this help and exit"
    , Option ['V'] ["version"]
        (NoArg $ \opts -> opts { optShowVersion = True })
        "show version and exit"
    ]
  where
    to f opts = opts { optTest = f (optTest opts) }

hiddenOptions :: [ OptDescr (CmdlineOptions -> CmdlineOptions) ]
hiddenOptions =
    [ Option [] [ "test-mode" ]
        (NoArg (\opts -> opts { optTestMode = True }))
        "test mode"
    ]

main :: IO ()
main = do
    config <- mapM parseConfig =<< findConfig
    let baseDir = maybe "." configDir config

    envtool <- lookupEnv "EREBOS_TEST_TOOL" >>= \mbtool ->
        return $ fromMaybe (error "No test tool defined") $ mbtool `mplus` (return . (baseDir </>) =<< configTool =<< config)

    let initOpts = defaultCmdlineOptions
            { optTest = defaultTestOptions
                { optDefaultTool = envtool
                , optTestDir = normalise $ baseDir </> optTestDir defaultTestOptions
                , optTimeout = fromMaybe (optTimeout defaultTestOptions) $ configTimeout =<< config
                }
            }

    args <- getArgs
    (opts, oselection) <- case getOpt Permute (options ++ hiddenOptions) args of
        (o, files, []) -> return (foldl (flip id) initOpts o, files)
        (_, _, errs) -> do
            hPutStrLn stderr $ concat errs <> "Try `erebos-tester --help' for more information."
            exitFailure

    let ( ofiles, otests )
            | any (any isPathSeparator) oselection = ( oselection, [] )
            | otherwise = ( [], map T.pack oselection )

    when (optShowHelp opts) $ do
        let header = unlines
                [ "Usage: erebos-tester [<option>...] [<test-name>...]"
                , "   or: erebos-tester [<option>...] <script>[:<test>]..."
                , "  <test-name> name of a test from project configuration"
                , "  <script>    path to test script file"
                , "  <test>      name of the test to run"
                , ""
                ]
                <> "Options are:"
        putStrLn $ usageInfo header options
        exitSuccess

    when (optShowVersion opts) $ do
        putStrLn versionLine
        exitSuccess

    when (optTestMode opts) $ do
        testMode config
        exitSuccess

    case words $ optDefaultTool $ optTest opts of
        (path : _) -> getPermissions path >>= \perms -> do
            when (not $ executable perms) $ do
                fail $ "‘" <> path <> "’ is not executable"
        _ -> fail $ "invalid tool argument: ‘" <> optDefaultTool (optTest opts) <> "’"

    files <- if not (null ofiles)
        then return $ flip map ofiles $ \ofile ->
            case span (/= ':') ofile of
                (path, ':':test) -> (path, Just $ T.pack test)
                (path, _)        -> (path, Nothing)
        else map (, Nothing) <$> maybe (return []) (getConfigTestFiles) config

    when (null files) $ fail $ "No test files"

    useColor <- case optColor opts of
        Just use -> return use
        Nothing -> queryTerminal (Fd 1)
    let outputStyle
            | optVerbose opts = OutputStyleVerbose
            | otherwise       = OutputStyleQuiet
    out <- startOutput outputStyle useColor

    ( modules, globalDefs ) <- loadModules (map fst files)
    tests <- filter ((`notElem` optExclude opts) . testName) <$> if null otests
        then fmap concat $ forM (zip modules files) $ \( Module {..}, ( filePath, mbTestName )) -> do
            case mbTestName of
                Nothing -> return moduleTests
                Just name
                    | Just test <- find ((name ==) . testName) moduleTests
                    -> return [ test ]
                    | otherwise
                    -> do
                        hPutStrLn stderr $ "Test ‘" <> T.unpack name <> "’ not found in ‘" <> filePath <> "’"
                        exitFailure
        else forM otests $ \name -> if
                | Just test <- find ((name ==) . testName) $ concatMap moduleTests modules
                -> return test
                | otherwise
                -> do
                    hPutStrLn stderr $ "Test ‘" <> T.unpack name <> "’ not found"
                    exitFailure

    ok <- allM (runTest out (optTest opts) globalDefs) $
        concat $ replicate (optRepeat opts) tests
    when (not ok) exitFailure

foreign export ccall testerMain :: IO ()
testerMain :: IO ()
testerMain = main
