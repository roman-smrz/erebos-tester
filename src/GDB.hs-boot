module GDB where

import Output
import {-# SOURCE #-} Process

data GDB
gdbSession :: MonadOutput m => GDB -> m ()
addInferior :: MonadOutput m => GDB -> Process -> m ()
