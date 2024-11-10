{-# OPTIONS_GHC -Wno-orphans #-}

module Parser (
    parseTestFile,
) where

import Control.Monad
import Control.Monad.State

import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import System.Directory
import System.Exit
import System.FilePath

import Parser.Core
import Parser.Expr
import Parser.Statement
import Test
import Test.Builtins

parseTestDefinition :: TestParser Toplevel
parseTestDefinition = label "test definition" $ toplevel ToplevelTest $ do
    block (\name steps -> return $ Test name $ mconcat steps) header testStep
    where header = do
              wsymbol "test"
              lexeme $ TL.toStrict <$> takeWhileP (Just "test name") (/=':')

parseDefinition :: TestParser Toplevel
parseDefinition = label "symbol definition" $ toplevel ToplevelDefinition $ do
    def <- localState $ L.indentBlock scn $ do
        wsymbol "def"
        name <- varName
        choice
            [ do
                symbol ":"
                let finish steps = do
                        return $ ( name, SomeExpr $ mconcat steps )
                return $ L.IndentSome Nothing finish testStep
            ]
    modify $ \s -> s { testVars = fmap someExprType def : testVars s }
    return def

parseTestModule :: FilePath -> TestParser Module
parseTestModule absPath = do
    moduleName <- choice
        [ label "module declaration" $ do
            wsymbol "module"
            off <- stateOffset <$> getParserState
            x <- identifier
            name <- (x:) <$> many (symbol "." >> identifier)
            when (or (zipWith (/=) (reverse name) (reverse $ map T.pack $ splitDirectories $ dropExtension $ absPath))) $ do
                registerParseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $
                    "module name does not match file path"
            eol >> scn
            return name
        , do
            return $ [ T.pack $ takeBaseName absPath ]
        ]
    toplevels <- many $ choice
        [ parseTestDefinition
        , parseDefinition
        ]
    let moduleTests = catMaybes $ map (\case ToplevelTest x -> Just x; _ -> Nothing) toplevels
        moduleDefinitions = catMaybes $ map (\case ToplevelDefinition x -> Just x; _ -> Nothing) toplevels
    eof
    return Module {..}

parseTestFile :: FilePath -> IO Module
parseTestFile path = do
    content <- TL.readFile path
    absPath <- makeAbsolute path
    let initState = TestParserState
            { testVars = concat
                [ map (fmap someVarValueType) builtins
                ]
            , testContext = SomeExpr RootNetwork
            , testNextTypeVar = 0
            , testTypeUnif = M.empty
            }
        res = runTestParser path content initState $ parseTestModule absPath

    case res of
         Left err -> putStr (errorBundlePretty err) >> exitFailure
         Right testModule -> return testModule
