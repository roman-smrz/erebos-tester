module Process (
    Process(..),
    ProcName(..),
    textProcName, unpackProcName,
    send,
) where

import Control.Concurrent.STM
import Control.Monad.IO.Class

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.IO
import System.Posix.Signals
import System.Process

data Process = Process
    { procName :: ProcName
    , procHandle :: ProcessHandle
    , procStdin :: Handle
    , procOutput :: TVar [Text]
    , procKillWith :: Maybe Signal
    }

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
