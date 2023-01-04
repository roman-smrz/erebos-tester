module Main where

import GHC.Environment

import System.Directory
import System.Environment
import System.FilePath
import System.Linux.Namespaces
import System.Posix.Process
import System.Posix.User
import System.Process

main :: IO ()
main = do
    -- we must get uid/gid before unshare
    uid <- getEffectiveUserID
    gid <- getEffectiveGroupID

    unshare [User, Network, Mount]
    writeUserMappings Nothing [UserMapping 0 uid 1]
    writeGroupMappings Nothing [GroupMapping 0 gid 1] True

    -- needed for creating /run/netns
    callCommand "mount -t tmpfs tmpfs /run"

    epath <- takeDirectory <$> getExecutablePath -- directory containing executable
    fpath <- map takeDirectory . take 1 <$> getFullArgs
        -- directory used for invocation, can differ from above for symlinked executable

    let dirs = concat
            [ [ epath ]
            , [ epath </> "../../../erebos-tester-core/build/erebos-tester-core" ]
            , fpath
            ]

    args <- getArgs
    mapM_ (\file -> executeFile file False args Nothing) =<<
        findExecutablesInDirectories dirs "erebos-tester-core"

    fail "core binary not found"
