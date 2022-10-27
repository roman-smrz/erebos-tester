module Process (
    Process(..),
    ProcName(..),
    textProcName, unpackProcName,
    send,
    outProc,
    lineReadingLoop,
    closeProcess,
) where

import Control.Arrow
import Control.Concurrent.STM
import Control.Monad.Except

import Data.Function
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Exit
import System.IO
import System.IO.Error
import System.Posix.Signals
import System.Process

import Network
import Output
import Test

data Process = Process
    { procName :: ProcName
    , procHandle :: ProcessHandle
    , procStdin :: Handle
    , procOutput :: TVar [Text]
    , procKillWith :: Maybe Signal
    , procNode :: Node
    }

instance Eq Process where
    (==) = (==) `on` procStdin

instance ExprType Process where
    textExprType _ = T.pack "proc"
    textExprValue n = T.pack "p:" <> textProcName (procName n)
    emptyVarValue = Process (ProcName T.empty) undefined undefined undefined undefined emptyVarValue

    recordMembers = map (first T.pack)
        [ ("node", RecordSelector $ procNode)
        ]


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
outProc otype p line = outLine otype (Just $ textProcName $ procName p) line

lineReadingLoop :: MonadOutput m => Process -> Handle -> (Text -> m ()) -> m ()
lineReadingLoop process h act =
    liftIO (tryIOError (T.hGetLine h)) >>= \case
        Left err
            | isEOFError err -> return ()
            | otherwise -> outProc OutputChildFail process $ T.pack $ "IO error: " ++ show err
        Right line -> do
            act line
            lineReadingLoop process h act

closeProcess :: (MonadIO m, MonadOutput m, MonadError Failed m) => Process -> m ()
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
            throwError Failed
