module GDB where

import Output

data GDB
gdbSession :: MonadOutput m => GDB -> m ()
