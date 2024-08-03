{-# OPTIONS_GHC -Wno-orphans #-}

module Parser (
    parseTestFile,
) where

import Control.Monad.State

import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import System.Directory
import System.Exit
import System.FilePath

import Parser.Core
import Parser.Expr
import Parser.Statement
import Test

parseTestDefinition :: TestParser Test
parseTestDefinition = label "test definition" $ toplevel $ do
    block (\name steps -> return $ Test name $ concat steps) header testStep
    where header = do
              wsymbol "test"
              lexeme $ TL.toStrict <$> takeWhileP (Just "test name") (/=':')

parseTestModule :: FilePath -> TestParser Module
parseTestModule absPath = do
    moduleName <- choice
        [ label "module declaration" $ do
            wsymbol "module"
            off <- stateOffset <$> getParserState
            x <- identifier
            name <- (x:) <$> many (symbol "." >> identifier)
            when (or (zipWith (/=) (reverse name) (reverse $ map T.pack $ splitDirectories $ dropExtension $ absPath))) $ do
                parseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $
                    "module name does not match file path"
            eol >> scn
            return name
        , do
            return $ [ T.pack $ takeBaseName absPath ]
        ]
    moduleTests <- many parseTestDefinition
    eof
    return Module { .. }

parseTestFile :: FilePath -> IO Module
parseTestFile path = do
    content <- TL.readFile path
    absPath <- makeAbsolute path
    let initState = TestParserState
            { testVars = []
            , testContext = SomeExpr RootNetwork
            }
    case evalState (runParserT (parseTestModule absPath) path content) initState of
         Left err -> putStr (errorBundlePretty err) >> exitFailure
         Right testModule -> return testModule
