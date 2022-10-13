{-# LANGUAGE OverloadedStrings #-}

module GDB (
    GDB, gdbProcess,
    gdbStart,
    addInferior,
    gdbSession,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.Text qualified as T
import Data.Text.IO qualified as T

import System.IO.Error
import System.Process

import Output
import Process

data GDB = GDB
    { gdbProcess_ :: Process
    }

gdbProcess :: GDB -> Process
gdbProcess = gdbProcess_

gdbCmd :: String
gdbCmd = "gdb --quiet --interpreter=mi3"

gdbStart :: (MonadOutput m, MonadFail m) => m GDB
gdbStart = do
    (Just hin, Just hout, Just herr, handle) <- liftIO $ createProcess (shell gdbCmd)
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe
        }
    pout <- liftIO $ newTVarIO []

    let process = Process
            { procName = ProcNameGDB
            , procHandle = handle
            , procStdin = hin
            , procOutput = pout
            , procKillWith = Nothing
            , procNode = undefined
            }
        gdb = GDB
            { gdbProcess_ = process
            }

    out <- getOutput
    liftIO $ void $ forkIO $ flip runReaderT out $
        lineReadingLoop process hout $ outProc OutputChildStdout process
    liftIO $ void $ forkIO $ flip runReaderT out $
        lineReadingLoop process herr $ outProc OutputChildStderr process

    send process "-gdb-set schedule-multiple on"
    send process "-gdb-set mi-async on"
    send process "-gdb-set print symbol-loading off"

    return gdb

addInferior :: MonadIO m => GDB -> Int -> Pid -> m ()
addInferior GDB { gdbProcess_ = process } i pid = do
    send process $ "-add-inferior"
    send process $ "-target-attach --thread-group i" <> T.pack (show i) <> " " <> T.pack (show pid)
    send process $ "-exec-continue --thread-group i" <> T.pack (show i)

gdbSession :: MonadOutput m => GDB -> m ()
gdbSession gdb = do
    outPrompt "gdb> "
    liftIO loop
    outClearPrompt
  where
    loop = catchIOError (Just <$> T.getLine) (\e -> if isEOFError e then return Nothing else ioError e) >>= \case
        Just line -> do
            send (gdbProcess gdb) ("-interpreter-exec console \"" <> line <> "\"")
            loop
        Nothing -> return ()
