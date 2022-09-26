module Process (
    Process(..),
    ProcName(..),
    textProcName, unpackProcName,
    send,
    outProc,
    closeProcess,
) where

import Control.Concurrent.STM
import Control.Monad.Except

import Data.Function
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Exit
import System.IO
import System.Posix.Signals
import System.Process

import Output

data Process = Process
    { procName :: ProcName
    , procHandle :: ProcessHandle
    , procStdin :: Handle
    , procOutput :: TVar [Text]
    , procKillWith :: Maybe Signal
    }

instance Eq Process where
    (==) = (==) `on` procStdin

data ProcName = ProcName Text
              | ProcNameTcpdump
              | ProcNameGDB
    deriving (Eq, Ord)

textProcName :: ProcName -> Text
textProcName (ProcName name) = name
textProcName ProcNameTcpdump = T.pack "tcpdump"
textProcName ProcNameGDB = T.pack "gdb"

unpackProcName :: ProcName -> String
unpackProcName = T.unpack . textProcName

send :: MonadIO m => Process -> Text -> m ()
send p line = liftIO $ do
    T.hPutStrLn (procStdin p) line
    hFlush (procStdin p)

outProc :: MonadOutput m => OutputType -> Process -> Text -> m ()
outProc otype p line = outLine otype (textProcName $ procName p) line

closeProcess :: (MonadIO m, MonadOutput m, MonadError () m) => Process -> m ()
closeProcess p = do
    liftIO $ hClose $ procStdin p
    case procKillWith p of
        Nothing -> return ()
        Just sig -> liftIO $ getPid (procHandle p) >>= \case
            Nothing -> return ()
            Just pid -> signalProcess sig pid

    liftIO (waitForProcess (procHandle p)) >>= \case
        ExitSuccess -> return ()
        ExitFailure code -> do
            outProc OutputChildFail p $ T.pack $ "exit code: " ++ show code
            throwError ()
