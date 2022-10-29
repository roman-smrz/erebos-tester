{-# LANGUAGE OverloadedStrings #-}

module Output (
    Output, OutputType(..),
    MonadOutput(..),
    startOutput,
    outLine,
    outPrompt, outClearPrompt,
) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL

import System.IO

data Output = Output
    { outState :: MVar OutputState
    , outConfig :: OutputConfig
    }

data OutputConfig = OutputConfig
    { outVerbose :: Bool
    }

data OutputState = OutputState
    { outCurPrompt :: Maybe Text
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
    <$> newMVar OutputState { outCurPrompt = Nothing }
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

clearPrompt :: OutputState -> IO ()
clearPrompt OutputState { outCurPrompt = Just _ } = T.putStr $ T.pack "\ESC[2K\r"
clearPrompt _ = return ()

showPrompt :: OutputState -> IO ()
showPrompt OutputState { outCurPrompt = Just p } = T.putStr p >> hFlush stdout
showPrompt _ = return ()

ioWithOutput :: MonadOutput m => (Output -> IO a) -> m a
ioWithOutput act = liftIO . act =<< getOutput

outLine :: MonadOutput m => OutputType -> Maybe Text -> Text -> m ()
outLine otype prompt line = ioWithOutput $ \out ->
    when (outVerbose (outConfig out) || printWhenQuiet otype) $ do
        withMVar (outState out) $ \st -> do
            clearPrompt st
            TL.putStrLn $ TL.fromChunks
                [ T.pack "\ESC[", outColor otype, T.pack "m"
                , maybe "" (<> outSign otype <> outArr otype <> " ") prompt
                , line
                , T.pack "\ESC[0m"
                ]
            showPrompt st

outPrompt :: MonadOutput m => Text -> m ()
outPrompt p = ioWithOutput $ \out -> modifyMVar_ (outState out) $ \st -> do
    clearPrompt st
    let st' = st { outCurPrompt = Just p }
    showPrompt st'
    return st'

outClearPrompt :: MonadOutput m => m ()
outClearPrompt = ioWithOutput $ \out -> modifyMVar_ (outState out) $ \st -> do
    clearPrompt st
    return st { outCurPrompt = Nothing }
