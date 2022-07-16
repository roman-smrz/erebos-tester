module GDB (
    gdbCmd, gdbInit,
    addInferior,
    gdbSession,
) where

import Control.Monad.IO.Class

import Data.Text qualified as T
import Data.Text.IO qualified as T

import System.IO.Error
import System.Process

import Process

gdbCmd :: String
gdbCmd = "gdb --quiet --interpreter=mi3"

gdbInit :: MonadIO m => Process -> m ()
gdbInit gdb = do
    send gdb $ T.pack "-gdb-set schedule-multiple on"
    send gdb $ T.pack "-gdb-set mi-async on"
    send gdb $ T.pack "-gdb-set print symbol-loading off"

addInferior :: MonadIO m => Process -> Int -> Pid -> m ()
addInferior gdb i pid = do
    send gdb $ T.pack $ "-add-inferior"
    send gdb $ T.pack $ "-target-attach --thread-group i" ++ show i ++ " " ++ show pid
    send gdb $ T.pack $ "-exec-continue --thread-group i" ++ show i

gdbSession :: MonadIO m => Process -> m ()
gdbSession gdb = liftIO $ do
    catchIOError (Just <$> T.getLine) (\e -> if isEOFError e then return Nothing else ioError e) >>= \case
        Just line -> do
            send gdb (T.pack "-interpreter-exec console \"" `T.append` line `T.append` T.pack "\"")
            gdbSession gdb
        Nothing -> return ()