module Output (
    Output, OutputType(..),
    startOutput,
    outLine,
    outPrompt, outClearPrompt,
) where

import Control.Concurrent.MVar

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL

import System.IO

import Test

data Output = Output { outState :: MVar OutputState }

data OutputState = OutputState
    { outCurPrompt :: Maybe Text
    }

data OutputType = OutputChildStdout
                | OutputChildStderr
                | OutputChildInfo
                | OutputChildFail
                | OutputMatch
                | OutputMatchFail

startOutput :: IO Output
startOutput =  Output <$> newMVar OutputState { outCurPrompt = Nothing }

outColor :: OutputType -> Text
outColor OutputChildStdout = T.pack "0"
outColor OutputChildStderr = T.pack "31"
outColor OutputChildInfo = T.pack "0"
outColor OutputChildFail = T.pack "31"
outColor OutputMatch = T.pack "32"
outColor OutputMatchFail = T.pack "31"

outSign :: OutputType -> Text
outSign OutputChildStdout = T.empty
outSign OutputChildStderr = T.pack "!"
outSign OutputChildInfo = T.pack "."
outSign OutputChildFail = T.pack "!!"
outSign OutputMatch = T.pack "+"
outSign OutputMatchFail = T.pack "/"

clearPrompt :: OutputState -> IO ()
clearPrompt OutputState { outCurPrompt = Just _ } = T.putStr $ T.pack "\ESC[2K\r"
clearPrompt _ = return ()

showPrompt :: OutputState -> IO ()
showPrompt OutputState { outCurPrompt = Just p } = T.putStr p >> hFlush stdout
showPrompt _ = return ()

outLine :: Output -> OutputType -> Maybe ProcName -> Text -> IO ()
outLine out otype mbproc line = withMVar (outState out) $ \st -> do
    clearPrompt st
    TL.putStrLn $ TL.fromChunks
        [ T.pack "\ESC[", outColor otype, T.pack "m"
        , maybe T.empty textProcName mbproc
        , outSign otype
        , T.pack "> "
        , line
        , T.pack "\ESC[0m"
        ]
    showPrompt st

outPrompt :: Output -> Text -> IO ()
outPrompt out p = modifyMVar_ (outState out) $ \st -> do
    clearPrompt st
    let st' = st { outCurPrompt = Just p }
    showPrompt st'
    return st'

outClearPrompt :: Output -> IO ()
outClearPrompt out = modifyMVar_ (outState out) $ \st -> do
    clearPrompt st
    return st { outCurPrompt = Nothing }
