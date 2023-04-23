{-# OPTIONS_GHC -Wno-orphans #-}

module Parser (
    parseTestFile,
) where

import Control.Monad.State

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import Text.Megaparsec hiding (State)

import System.Exit

import Parser.Core
import Parser.Statement
import Test

parseTestDefinition :: TestParser Test
parseTestDefinition = label "test definition" $ toplevel $ do
    block (\name steps -> return $ Test name $ concat steps) header testStep
    where header = do
              wsymbol "test"
              lexeme $ TL.toStrict <$> takeWhileP (Just "test name") (/=':')

parseTestDefinitions :: TestParser [Test]
parseTestDefinitions = do
    tests <- many parseTestDefinition
    eof
    return tests

parseTestFile :: FilePath -> IO [Test]
parseTestFile path = do
    content <- TL.readFile path
    let initState = TestParserState
            { testVars = []
            , testContext = SomeExpr RootNetwork
            }
    case evalState (runParserT parseTestDefinitions path content) initState of
         Left err -> putStr (errorBundlePretty err) >> exitFailure
         Right tests -> return tests
