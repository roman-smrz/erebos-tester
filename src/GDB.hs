module GDB (
    GDB, gdbProcess,
    gdbStart,
    addInferior,
    gdbSession,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.Reader

import Data.Char
import Data.List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import System.Console.Haskeline.Completion
import System.Process

import Output
import Process

data GDB = GDB
    { gdbProcess_ :: Process
    , gdbResult :: MVar (ResultClass, [(Text, MiValue)])
    , gdbInferiors :: MVar [Inferior]
    , gdbThreadGroups :: TChan Text
    , gdbOnCrash :: Process -> IO ()
    }

gdbProcess :: GDB -> Process
gdbProcess = gdbProcess_

data Inferior = Inferior
    { infProcess :: Process
    , infPid :: Pid
    , infThreadGroup :: Text
    , infThreads :: [Text]
    }

data MiRecord = ResultRecord ResultClass [(Text, MiValue)]
              | ExecAsyncOutput Text [(Text, MiValue)]
              | StatusAsyncOutput Text [(Text, MiValue)]
              | NotifyAsyncOutput Text [(Text, MiValue)]
              | ConsoleStreamOutput Text
              | TargetStreamOutput Text
              | LogStreamOutput Text
    deriving (Show)

data ResultClass = Done | Connected | Error | Exit
    deriving (Show)

data MiValue = MiString Text
             | MiTuple [(Text, MiValue)]
             | MiList [MiValue]
    deriving (Show)


gdbCmd :: String
gdbCmd = "gdb --quiet --interpreter=mi3"

gdbStart :: (MonadOutput m, MonadFail m) => (Process -> IO ()) -> m GDB
gdbStart onCrash = do
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
    gdb <- GDB
        <$> pure process
        <*> liftIO newEmptyMVar
        <*> liftIO (newMVar [])
        <*> liftIO newTChanIO
        <*> pure onCrash

    out <- getOutput
    liftIO $ void $ forkIO $ flip runReaderT out $
        lineReadingLoop process hout $ gdbLine gdb
    liftIO $ void $ forkIO $ flip runReaderT out $
        lineReadingLoop process herr $ outProc OutputChildStderr process

    gdbCommand gdb "-gdb-set schedule-multiple on"
    gdbCommand gdb "-gdb-set mi-async on"
    gdbCommand gdb "-gdb-set non-stop on"
    gdbCommand gdb "-gdb-set print symbol-loading off"

    return gdb

gdbLine :: GDB -> Text -> ReaderT Output IO ()
gdbLine _ "(gdb)" = return ()
gdbLine _ "(gdb) " = return ()
gdbLine gdb rline = either (outProc OutputError (gdbProcess gdb) . T.pack . errorBundlePretty) go $
    runParser miOutputRecord "" rline
  where
    go = \case
        ResultRecord cls params -> liftIO $ putMVar (gdbResult gdb) (cls, params)
        ExecAsyncOutput cls params -> (cls,) <$> liftIO (readMVar (gdbInferiors gdb)) >>= \case
            ("stopped", infs)
                | Just (MiString "signal-received") <- lookup "reason" params
                , Just (MiString tid) <- lookup "thread-id" params
                , Just inf <- find (elem tid . infThreads) infs
                -> do
                    -- It is needed to switch thread manually in non-stop mode,
                    -- fork to avoid blocking further input and reply processing.
                    out <- getOutput
                    void $ liftIO $ forkIO $ do
                        flip runReaderT out $ do
                            gdbCommand gdb $ "-thread-select " <> tid
                        gdbOnCrash gdb $ infProcess inf
            _ -> return ()
        StatusAsyncOutput cls params -> outProc OutputChildInfo (gdbProcess gdb) $ "status: " <> cls <> " " <> T.pack (show params)
        NotifyAsyncOutput cls params -> case cls of
            "thread-group-added" | Just (MiString tgid) <- lookup "id" params -> do
                liftIO $ atomically $ writeTChan (gdbThreadGroups gdb) tgid
            "thread-group-exited" | Just (MiString tgid) <- lookup "id" params -> do
                liftIO $ modifyMVar_ (gdbInferiors gdb) $ return . filter ((/=tgid) . infThreadGroup)
            "thread-created"
                | Just (MiString tid) <- lookup "id" params
                , Just (MiString tgid) <- lookup "group-id" params
                -> liftIO $ modifyMVar_ (gdbInferiors gdb) $ return . map (\inf -> if infThreadGroup inf == tgid then inf { infThreads = tid : infThreads inf } else inf)
            "thread-exited"
                | Just (MiString tid) <- lookup "id" params
                , Just (MiString tgid) <- lookup "group-id" params
                -> liftIO $ modifyMVar_ (gdbInferiors gdb) $ return . map (\inf -> if infThreadGroup inf == tgid then inf { infThreads = filter (/=tid) $ infThreads inf } else inf)
            _ -> return ()
        ConsoleStreamOutput line -> mapM_ (outLine OutputAlways Nothing) (T.lines line)
        TargetStreamOutput line -> mapM_ (outProc OutputChildStderr (gdbProcess gdb) . ("target-stream: " <>)) (T.lines line)
        LogStreamOutput line -> mapM_ (outProc OutputChildInfo (gdbProcess gdb) . ("log: " <>)) (T.lines line)

addInferior :: MonadOutput m => GDB -> Process -> m ()
addInferior gdb process = do
    liftIO (getPid $ procHandle process) >>= \case
        Nothing -> outProc OutputError process $ "failed to get PID"
        Just pid -> do
            tgid <- liftIO (atomically $ tryReadTChan $ gdbThreadGroups gdb) >>= \case
                Just tgid -> return tgid
                Nothing -> do
                    gdbCommand gdb $ "-add-inferior"
                    liftIO $ atomically $ readTChan $ gdbThreadGroups gdb

            liftIO $ modifyMVar_ (gdbInferiors gdb) $ return . (:) Inferior
                { infProcess = process
                , infPid = pid
                , infThreadGroup = tgid
                , infThreads = []
                }

            gdbCommand gdb $ "-target-attach --thread-group " <> tgid <> " " <> T.pack (show pid)
            gdbCommand gdb $ "-exec-continue --thread-group " <> tgid

gdbCommandRes :: MonadIO m => GDB -> Text -> m (ResultClass, [(Text, MiValue)])
gdbCommandRes gdb cmd = do
    send (gdbProcess gdb) cmd
    liftIO (takeMVar (gdbResult gdb))

gdbCommand :: MonadOutput m => GDB -> Text -> m ()
gdbCommand gdb cmd = do
    gdbCommandRes gdb cmd >>= \case
        (Done, _) -> return ()
        (Connected, _) -> outProc OutputChildInfo (gdbProcess gdb) "result connected"
        (Error, _) -> outProc OutputError (gdbProcess gdb) $ "command error: " <> cmd
        (Exit, _) -> outProc OutputError (gdbProcess gdb) "result exit"

gdbSession :: MonadOutput m => GDB -> m ()
gdbSession gdb = loop ""
  where
    loop prev = outPromptGetLineCompletion (gdbCompletion gdb) "gdb> " >>= \case
        Just line -> do
            let cmd = if T.null line then prev else line
            gdbCommand gdb ("-interpreter-exec console \"" <> cmd <> "\"")
            loop cmd
        Nothing -> return ()

gdbCompletion :: GDB -> CompletionFunc IO
gdbCompletion gdb (revcmd, _) = do
    gdbCommandRes gdb ("-complete " <> T.pack (show (reverse revcmd))) >>= \case
        (Done, resp)
            | Just (MiList matches) <- lookup "matches" resp -> do
                return ("", concatMap (\case MiString m -> [Completion (T.unpack m) (T.unpack m) False]; _ -> []) matches)
        _ -> return ("", [])


type MiParser = ParsecT Void MiStream Identity

type MiStream = Text

miOutputRecord :: MiParser MiRecord
miOutputRecord = choice
    [ miResultRecord
    , miExecAsync
    , miStatusAsync
    , miNotifyAsync
    , miConsoleStream
    , miTargetStream
    , miLogStream
    ]

miResultRecord :: MiParser MiRecord
miResultRecord = char '^' >> ResultRecord <$> resultClass <*> many (char ',' *> result)

miExecAsync, miStatusAsync, miNotifyAsync :: MiParser MiRecord
miExecAsync = char '*' >> ExecAsyncOutput <$> miString <*> many (char ',' *> result)
miStatusAsync = char '+' >> StatusAsyncOutput <$> miString <*> many (char ',' *> result)
miNotifyAsync = char '=' >> NotifyAsyncOutput <$> miString <*> many (char ',' *> result)

miConsoleStream, miTargetStream, miLogStream :: MiParser MiRecord
miConsoleStream = char '~' >> ConsoleStreamOutput <$> miCString
miTargetStream = char '@' >> TargetStreamOutput <$> miCString
miLogStream = char '&' >> LogStreamOutput <$> miCString

resultClass :: MiParser ResultClass
resultClass = label "result-class" $ choice
    [ return Done <* string "done"
    , return Done <* string "running" -- equivalent to "done" per documentation
    , return Connected <* string "connected"
    , return Error <* string "error"
    , return Exit <* string "exit"
    ]

result :: MiParser (Text, MiValue)
result = (,) <$> miString <* char '=' <*> miValue

miString :: MiParser Text
miString = label "string" $ takeWhile1P Nothing (\x -> isAlphaNum x || x == '_' || x == '-')

miCString :: MiParser Text
miCString = label "c-string" $ do
    void $ char '"'
    let go = choice
            [ char '"' >> return []
            , takeWhile1P Nothing (`notElem` ['\"', '\\']) >>= \s -> (s:) <$> go
            ,do void $ char '\\'
                c <- choice
                    [ char '\\' >> return '\\'
                    , char '"' >> return '"'
                    , char 'n' >> return '\n'
                    , char 'r' >> return '\r'
                    , char 't' >> return '\t'
                    ]
                ((T.singleton c) :) <$> go
            ]
    T.concat <$> go

listOf :: MiParser a -> MiParser [a]
listOf item = do
    x <- item
    (x:) <$> choice [ char ',' >> listOf item, return [] ]

miTuple :: MiParser [(Text, MiValue)]
miTuple = between (char '{') (char '}') $ listOf result <|> return []

miList :: MiParser [MiValue]
miList = between (char '[') (char ']') $ listOf miValue <|> return []

miValue :: MiParser MiValue
miValue = choice
    [ MiString <$> miCString
    , MiTuple <$> miTuple
    , MiList <$> miList
    ]
