{-# OPTIONS_GHC -Wno-orphans #-}

module Parser (
    parseTestFile,
) where

import Control.Monad.State

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL

import Text.Megaparsec hiding (State)

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

parseTestModule :: Text -> TestParser Module
parseTestModule defaultName = do
    moduleName <- choice
        [ label "module declaration" $ do
            wsymbol "module"
            x <- identifier
            (x:) <$> many (symbol "." >> identifier)
        , do
            return $ [ defaultName ]
        ]
    moduleTests <- many parseTestDefinition
    eof
    return Module { .. }

parseTestFile :: FilePath -> IO Module
parseTestFile path = do
    content <- TL.readFile path
    let initState = TestParserState
            { testVars = []
            , testContext = SomeExpr RootNetwork
            }
        defaultModuleName = T.pack $ takeBaseName path
    case evalState (runParserT (parseTestModule defaultModuleName) path content) initState of
         Left err -> putStr (errorBundlePretty err) >> exitFailure
         Right tests -> return tests
