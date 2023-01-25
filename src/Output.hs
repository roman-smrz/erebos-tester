{-# LANGUAGE OverloadedStrings #-}

module Output (
    Output, OutputType(..),
    MonadOutput(..),
    startOutput,
    outLine,
    outPromptGetLine,
    outPromptGetLineCompletion,
) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL

import System.Console.Haskeline
import System.Console.Haskeline.History

data Output = Output
    { outState :: MVar OutputState
    , outConfig :: OutputConfig
    }

data OutputConfig = OutputConfig
    { outVerbose :: Bool
    }

data OutputState = OutputState
    { outPrint :: TL.Text -> IO ()
    , outHistory :: History
    }

data OutputType = OutputChildStdout
                | OutputChildStderr
                | OutputChildStdin
                | OutputChildInfo
                | OutputChildFail
                | OutputMatch
                | OutputMatchFail
                | OutputError
                | OutputAlways

class MonadIO m => MonadOutput m where
    getOutput :: m Output

instance MonadIO m => MonadOutput (ReaderT Output m) where
    getOutput = ask

startOutput :: Bool -> IO Output
startOutput verbose = Output
    <$> newMVar OutputState { outPrint = TL.putStrLn, outHistory = emptyHistory }
    <*> pure OutputConfig { outVerbose = verbose }

outColor :: OutputType -> Text
outColor OutputChildStdout = T.pack "0"
outColor OutputChildStderr = T.pack "31"
outColor OutputChildStdin = T.pack "0"
outColor OutputChildInfo = T.pack "0"
outColor OutputChildFail = T.pack "31"
outColor OutputMatch = T.pack "32"
outColor OutputMatchFail = T.pack "31"
outColor OutputError = T.pack "31"
outColor OutputAlways = "0"

outSign :: OutputType -> Text
outSign OutputChildStdout = T.empty
outSign OutputChildStderr = T.pack "!"
outSign OutputChildStdin = T.empty
outSign OutputChildInfo = T.pack "."
outSign OutputChildFail = T.pack "!!"
outSign OutputMatch = T.pack "+"
outSign OutputMatchFail = T.pack "/"
outSign OutputError = T.pack "!!"
outSign OutputAlways = T.empty

outArr :: OutputType -> Text
outArr OutputChildStdin = "<"
outArr _ = ">"

printWhenQuiet :: OutputType -> Bool
printWhenQuiet = \case
    OutputChildStderr -> True
    OutputChildFail -> True
    OutputMatchFail -> True
    OutputError -> True
    OutputAlways -> True
    _ -> False

ioWithOutput :: MonadOutput m => (Output -> IO a) -> m a
ioWithOutput act = liftIO . act =<< getOutput

outLine :: MonadOutput m => OutputType -> Maybe Text -> Text -> m ()
outLine otype prompt line = ioWithOutput $ \out ->
    when (outVerbose (outConfig out) || printWhenQuiet otype) $ do
        withMVar (outState out) $ \st -> do
            outPrint st $ TL.fromChunks
                [ T.pack "\ESC[", outColor otype, T.pack "m"
                , maybe "" (<> outSign otype <> outArr otype <> " ") prompt
                , line
                , T.pack "\ESC[0m"
                ]

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
