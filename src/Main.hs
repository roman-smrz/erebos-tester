module Main (main) where

import Control.Monad

import Data.Maybe
import qualified Data.Text as T

import Text.Read (readMaybe)

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.FilePath.Glob

import Config
import Output
import Parser
import Process
import Run
import Test
import Util
import Version

data CmdlineOptions = CmdlineOptions
    { optTest :: TestOptions
    , optRepeat :: Int
    , optShowVersion :: Bool
    , optVerbose :: Bool
    }

defaultCmdlineOptions :: CmdlineOptions
defaultCmdlineOptions = CmdlineOptions
    { optTest = defaultTestOptions
    , optRepeat = 1
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
        "remove test directory if it already exists instead of stopping"
    , Option ['k'] ["keep"]
        (NoArg $ to $ \opts -> opts { optKeep = True })
        "keep test directory even if all tests succeed"
    , Option ['r'] ["repeat"]
        (ReqArg (\str opts -> opts { optRepeat = read str }) "COUNT")
        "number of times to repeat the test(s)"
    , Option ['V'] ["version"]
        (NoArg $ \opts -> opts { optShowVersion = True })
        "show version and exit"
    , Option [] ["wait"]
        (NoArg $ to $ \opts -> opts { optWait = True })
        "wait at the end of each test"
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
        then return $ flip map ofiles $ \ofile ->
            case span (/= ':') ofile of
                (path, ':':test) -> (path, Just $ T.pack test)
                (path, _)        -> (path, Nothing)
        else map (, Nothing) . concat <$> mapM (flip globDir1 baseDir) (maybe [] configTests config)

    when (null files) $ fail $ "No test files"

    out <- startOutput $ optVerbose opts

    tests <- forM files $ \(path, mbTestName) -> do
        fileTests <- parseTestFile path
        return $ case mbTestName of
            Nothing -> fileTests
            Just name -> filter ((==name) . testName) fileTests

    ok <- allM (runTest out $ optTest opts) $
        concatMap (replicate (optRepeat opts)) $ concat tests
    when (not ok) exitFailure
