module Output (
    Output, OutputType(..),
    startOutput,
    outLine,
) where

import Control.Concurrent.MVar

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL

import Test

data Output = Output { outState :: MVar () }

data OutputType = OutputChildStdout
                | OutputChildStderr
                | OutputChildInfo
                | OutputChildFail
                | OutputMatch
                | OutputMatchFail

startOutput :: IO Output
startOutput =  Output <$> newMVar ()

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

outLine :: Output -> OutputType -> Maybe ProcName -> Text -> IO ()
outLine out otype mbproc line = withMVar (outState out) $ \_ -> do
    TL.putStrLn $ TL.fromChunks
        [ T.pack "\ESC[", outColor otype, T.pack "m"
        , maybe T.empty textProcName mbproc
        , outSign otype
        , T.pack "> "
        , line
        , T.pack "\ESC[0m"
        ]
