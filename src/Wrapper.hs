module Main where

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

    path <- getExecutablePath
    args <- getArgs
    executeFile (takeDirectory path </> "../../../erebos-tester-core/build/erebos-tester-core/erebos-tester-core") False args Nothing
