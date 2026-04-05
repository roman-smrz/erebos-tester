module Process.Signal (
    Signal(..),
    signalBuiltins,
    signalProcess,
) where

import Control.Monad.IO.Class

import Data.Text (Text)
import Data.Text qualified as T

import Script.Expr

import System.Posix qualified as Posix


newtype Signal = Signal Posix.Signal
    deriving (Eq, Ord)

instance ExprType Signal where
    textExprType _ = "Signal"
    textExprValue (Signal sig)
        | sig == Posix.sigHUP = "SIGHUP"
        | sig == Posix.sigINT = "SIGINT"
        | sig == Posix.sigQUIT = "SIGQUIT"
        | sig == Posix.sigILL = "SIGILL"
        | sig == Posix.sigTRAP = "SIGTRAP"
        | sig == Posix.sigABRT = "SIGABRT"
        | sig == Posix.sigBUS = "SIGBUS"
        | sig == Posix.sigFPE = "SIGFPE"
        | sig == Posix.sigKILL = "SIGKILL"
        | sig == Posix.sigUSR1 = "SIGUSR1"
        | sig == Posix.sigSEGV = "SIGSEGV"
        | sig == Posix.sigUSR2 = "SIGUSR2"
        | sig == Posix.sigPIPE = "SIGPIPE"
        | sig == Posix.sigALRM = "SIGALRM"
        | sig == Posix.sigTERM = "SIGTERM"
        | sig == Posix.sigCHLD = "SIGCHLD"
        | sig == Posix.sigCONT = "SIGCONT"
        | sig == Posix.sigSTOP = "SIGSTOP"
        | sig == Posix.sigTSTP = "SIGTSTP"
        | sig == Posix.sigTTIN = "SIGTTIN"
        | sig == Posix.sigTTOU = "SIGTTOU"
        | sig == Posix.sigURG = "SIGURG"
        | sig == Posix.sigXCPU = "SIGXCPU"
        | sig == Posix.sigXFSZ = "SIGXFSZ"
        | sig == Posix.sigVTALRM = "SIGVTALRM"
        | sig == Posix.sigPROF = "SIGPROF"
        | sig == Posix.sigPOLL = "SIGPOLL"
        | sig == Posix.sigSYS = "SIGSYS"
        | otherwise = "<SIG_" <> T.pack (show sig) <> ">"


signalBuiltins :: [ ( Text, SomeVarValue ) ]
signalBuiltins = map (fmap someConstValue)
    [ ( "SIGHUP", Signal Posix.sigHUP )
    , ( "SIGINT", Signal Posix.sigINT )
    , ( "SIGQUIT", Signal Posix.sigQUIT )
    , ( "SIGILL", Signal Posix.sigILL )
    , ( "SIGTRAP", Signal Posix.sigTRAP )
    , ( "SIGABRT", Signal Posix.sigABRT )
    , ( "SIGBUS", Signal Posix.sigBUS )
    , ( "SIGFPE", Signal Posix.sigFPE )
    , ( "SIGKILL", Signal Posix.sigKILL )
    , ( "SIGUSR1", Signal Posix.sigUSR1 )
    , ( "SIGSEGV", Signal Posix.sigSEGV )
    , ( "SIGUSR2", Signal Posix.sigUSR2 )
    , ( "SIGPIPE", Signal Posix.sigPIPE )
    , ( "SIGALRM", Signal Posix.sigALRM )
    , ( "SIGTERM", Signal Posix.sigTERM )
    , ( "SIGCHLD", Signal Posix.sigCHLD )
    , ( "SIGCONT", Signal Posix.sigCONT )
    , ( "SIGSTOP", Signal Posix.sigSTOP )
    , ( "SIGTSTP", Signal Posix.sigTSTP )
    , ( "SIGTTIN", Signal Posix.sigTTIN )
    , ( "SIGTTOU", Signal Posix.sigTTOU )
    , ( "SIGURG", Signal Posix.sigURG )
    , ( "SIGXCPU", Signal Posix.sigXCPU )
    , ( "SIGXFSZ", Signal Posix.sigXFSZ )
    , ( "SIGVTALRM", Signal Posix.sigVTALRM )
    , ( "SIGPROF", Signal Posix.sigPROF )
    , ( "SIGPOLL", Signal Posix.sigPOLL )
    , ( "SIGSYS", Signal Posix.sigSYS )
    ]


signalProcess :: MonadIO m => Signal -> Posix.ProcessID -> m ()
signalProcess (Signal sig) pid = liftIO $ Posix.signalProcess sig pid
