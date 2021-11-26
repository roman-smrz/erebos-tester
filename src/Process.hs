module Process (
    Process(..),
    ProcName(..),
    textProcName, unpackProcName,
) where

import Control.Concurrent.STM

import Data.Text (Text)
import qualified Data.Text as T

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
