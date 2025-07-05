module Sandbox (
    isolateFilesystem,
) where

import Foreign.C.String
import Foreign.C.Types

import System.Directory


isolateFilesystem :: FilePath -> IO Bool
isolateFilesystem rwDir = do
    absDir <- makeAbsolute rwDir
    withCString absDir c_isolate_fs >>= return . (== 0)

foreign import ccall unsafe "erebos_tester_isolate_fs" c_isolate_fs :: CString -> IO CInt
