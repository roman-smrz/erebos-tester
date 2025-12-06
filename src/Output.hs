module Output (
    Output, OutputStyle(..), OutputType(..),
    MonadOutput(..),
    startOutput,
    resetOutputTime,
    outLine,
    outPromptGetLine,
    outPromptGetLineCompletion,
) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL

import System.Clock
import System.Console.Haskeline
import System.Console.Haskeline.History
import System.IO

import Text.Printf

import Script.Expr

data Output = Output
    { outState :: MVar OutputState
    , outConfig :: OutputConfig
    , outStartedAt :: MVar TimeSpec
    }

data OutputConfig = OutputConfig
    { outStyle :: OutputStyle
    , outUseColor :: Bool
    }

data OutputState = OutputState
    { outPrint :: TL.Text -> IO ()
    , outHistory :: History
    }

data OutputStyle
    = OutputStyleQuiet
    | OutputStyleVerbose
    | OutputStyleTest
    deriving (Eq)

data OutputType
    = OutputChildStdout
    | OutputChildStderr
    | OutputChildStdin
    | OutputChildInfo
    | OutputChildFail
    | OutputMatch
    | OutputMatchFail CallStack
    | OutputError
    | OutputAlways
    | OutputTestRaw

class MonadIO m => MonadOutput m where
    getOutput :: m Output

instance MonadIO m => MonadOutput (ReaderT Output m) where
    getOutput = ask

startOutput :: OutputStyle -> Bool -> IO Output
startOutput outStyle outUseColor = do
    outState <- newMVar OutputState { outPrint = TL.putStrLn, outHistory = emptyHistory }
    outConfig <- pure OutputConfig {..}
    outStartedAt <- newMVar =<< getTime Monotonic
    hSetBuffering stdout LineBuffering
    return Output {..}

resetOutputTime :: Output -> IO ()
resetOutputTime Output {..} = do
    modifyMVar_ outStartedAt . const $ getTime Monotonic

outColor :: OutputType -> Text
outColor OutputChildStdout = T.pack "0"
outColor OutputChildStderr = T.pack "31"
outColor OutputChildStdin = T.pack "0"
outColor OutputChildInfo = T.pack "0"
outColor OutputChildFail = T.pack "31"
outColor OutputMatch = T.pack "32"
outColor OutputMatchFail {} = T.pack "31"
outColor OutputError = T.pack "31"
outColor OutputAlways = "0"
outColor OutputTestRaw = "0"

outSign :: OutputType -> Text
outSign OutputChildStdout = " "
outSign OutputChildStderr = T.pack "!"
outSign OutputChildStdin = T.empty
outSign OutputChildInfo = T.pack "."
outSign OutputChildFail = T.pack "!!"
outSign OutputMatch = T.pack "+"
outSign OutputMatchFail {} = T.pack "/"
outSign OutputError = T.pack "!!"
outSign OutputAlways = T.empty
outSign OutputTestRaw = T.empty

outArr :: OutputType -> Text
outArr OutputChildStdin = "<"
outArr _ = ">"

outTestLabel :: OutputType -> Text
outTestLabel = \case
    OutputChildStdout -> "child-stdout"
    OutputChildStderr -> "child-stderr"
    OutputChildStdin -> "child-stdin"
    OutputChildInfo -> "child-info"
    OutputChildFail -> "child-fail"
    OutputMatch -> "match"
    OutputMatchFail {} -> "match-fail"
    OutputError -> "error"
    OutputAlways -> "other"
    OutputTestRaw -> ""

printWhenQuiet :: OutputType -> Bool
printWhenQuiet = \case
    OutputChildStderr -> True
    OutputChildFail -> True
    OutputMatchFail {} -> True
    OutputError -> True
    OutputAlways -> True
    _ -> False

ioWithOutput :: MonadOutput m => (Output -> IO a) -> m a
ioWithOutput act = liftIO . act =<< getOutput

outLine :: MonadOutput m => OutputType -> Maybe Text -> Text -> m ()
outLine otype prompt line = ioWithOutput $ \out ->
    case outStyle (outConfig out) of
        OutputStyleQuiet
            | printWhenQuiet otype -> normalOutput out
            | otherwise -> return ()
        OutputStyleVerbose -> normalOutput out
        OutputStyleTest -> testOutput out
  where
    normalOutput out = do
        stime <- readMVar (outStartedAt out)
        nsecs <- toNanoSecs . (`diffTimeSpec` stime) <$> getTime Monotonic
        withMVar (outState out) $ \st -> do
            forM_ (normalOutputLines otype line) $ \line' -> do
                outPrint st $ TL.fromChunks $ concat
                    [ [ T.pack $ printf "[% 2d.%03d] " (nsecs `quot` 1000000000) ((nsecs `quot` 1000000) `rem` 1000) ]
                    , if outUseColor (outConfig out)
                        then [ T.pack "\ESC[", outColor otype, T.pack "m" ]
                        else []
                    , [ maybe "" (<> outSign otype <> outArr otype <> " ") prompt ]
                    , [ line' ]
                    , if outUseColor (outConfig out)
                        then [ T.pack "\ESC[0m" ]
                        else []
                    ]

    testOutput out = do
        withMVar (outState out) $ \st -> do
            case otype of
                OutputTestRaw -> outPrint st $ TL.fromStrict line
                _ -> forM_ (testOutputLines otype (maybe "-" id prompt) line) $ outPrint st . TL.fromStrict


normalOutputLines :: OutputType -> Text -> [ Text ]
normalOutputLines (OutputMatchFail (CallStack stack)) msg = concat
    [ msg <> " on " <> textSourceLine stackTopLine : showVars stackTopVars
    , concat $ flip map stackRest $ \( sline, vars ) ->
        "  called from " <> textSourceLine sline : showVars vars
    ]
  where
    showVars =
        map $ \(( name, sel ), value ) -> T.concat
            [ "    ", textFqVarName name, T.concat (map ("."<>) sel)
            , " = ", textSomeVarValue value
            ]
    (( stackTopLine, stackTopVars ), stackRest ) =
        case stack of
            (stop : srest) -> ( stop, srest )
            [] -> (( SourceLine "unknown", [] ), [] )
normalOutputLines _ msg = [ msg ]


testOutputLines :: OutputType -> Text -> Text -> [ Text ]
testOutputLines otype@(OutputMatchFail (CallStack stack)) _ msg = concat
    [ [ T.concat [ outTestLabel otype, " ", msg ] ]
    , concat $ flip map stack $ \( sline, vars ) ->
        T.concat [ outTestLabel otype, "-line ", textSourceLine sline ] : showVars vars
    ]
  where
    showVars =
        map $ \(( name, sel ), value ) -> T.concat
            [ outTestLabel otype, "-var ", textFqVarName name, T.concat (map ("."<>) sel)
            , " ", textSomeVarValue value
            ]
testOutputLines otype prompt msg = [ T.concat [ outTestLabel otype, " ", prompt, " ", msg ] ]


outPromptGetLine :: MonadOutput m => Text -> m (Maybe Text)
outPromptGetLine = outPromptGetLineCompletion noCompletion

outPromptGetLineCompletion :: MonadOutput m => CompletionFunc IO -> Text -> m (Maybe Text)
outPromptGetLineCompletion compl prompt = ioWithOutput $ \out -> do
    st <- takeMVar (outState out)
    (x, st') <- runInputT (setComplete compl defaultSettings) $ do
        p <- getExternalPrint
        putHistory $ outHistory st
        liftIO $ putMVar (outState out) st { outPrint = p . TL.unpack . (<>"\n") }
        x <- getInputLine $ T.unpack prompt
        st' <- liftIO $ takeMVar (outState out)
        hist' <- getHistory
        return (x, st' { outPrint = outPrint st, outHistory = hist' })
    putMVar (outState out) st'
    return $ fmap T.pack x
