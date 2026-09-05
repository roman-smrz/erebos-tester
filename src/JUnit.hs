module JUnit (
    writeJUnitReport,
) where

import Control.Monad

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Function
import Data.List.NonEmpty qualified as NE
import Data.Text.Encoding

import System.Directory
import System.FilePath
import System.IO

import Run
import Script.Var


writeJUnitReport :: FilePath -> Report -> IO ()
writeJUnitReport path Report {..} = do
    createDirectoryIfMissing True $ takeDirectory path
    withFile path WriteMode $ \h -> do
        B.hPutStr h $ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
        B.hPutStr h $ "<testsuites time=\"" <> BC.pack (show reportTotalTime) <> "\">\n"
        forM_ (NE.groupBy ((==) `on` (testNameModule . reportTestName)) reportTests) $ \grp -> do
            B.hPutStr h $ "<testsuite name=\"" <> encodeUtf8 (textModuleName $ testNameModule $ reportTestName $ NE.head grp) <> "\" time=\"" <> BC.pack (show $ sum $ map reportTime $ NE.toList grp) <> "\">"
            forM_ grp $ \SingleTestReport {..} -> do
                B.hPutStr h $ B.concat
                    [ "<testcase name=\"", encodeUtf8 (testNameBase reportTestName), "\""
                    , " classname=\"", encodeUtf8 (textModuleName $ testNameModule reportTestName), "\""
                    , " time=\"", BC.pack (show reportTime), "\""
                    , case reportTestFailed of
                        Nothing -> do
                            " />"
                        Just Failed -> do
                            "><failure message=\"Test failed\"></failure></testcase>"
                        Just (ProcessCrashed _) -> do
                            "><error message=\"Process crashed\"></error></testcase>"
                    ]

            B.hPutStr h $ "</testsuite>"
        B.hPutStr h $ "</testsuites>\n"
