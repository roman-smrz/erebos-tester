module Error (
    TesterError(..),
    throwOtherError
) where

import Control.Exception
import Control.Monad.Except
import Control.Monad.IO.Class

import Data.Text (Text)
import Data.Text qualified as T


data TesterError
    = OtherTesterError Text

instance Show TesterError where
    show = displayException

instance Exception TesterError where
    displayException = \case
        OtherTesterError err -> T.unpack err


throwOtherError :: (MonadIO m, MonadError IOException m) => Text -> m a
throwOtherError = liftIO . throwIO . OtherTesterError
