{-# OPTIONS_GHC -Wno-orphans #-}

module Parser (
    parseTestFile,
) where

import Control.Monad
import Control.Monad.State

import Data.Map qualified as M
import Data.Maybe
import Data.Proxy
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Void

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import System.Directory
import System.Exit
import System.FilePath

import Network
import Parser.Core
import Parser.Expr
import Parser.Statement
import Test
import Test.Builtins

parseTestDefinition :: TestParser Toplevel
parseTestDefinition = label "test definition" $ toplevel ToplevelTest $ do
    localState $ do
        modify $ \s -> s
            { testContext = SomeExpr $ varExpr SourceLineBuiltin rootNetworkVar
            }
        block (\name steps -> return $ Test name $ mconcat steps) header testStep
  where
    header = do
        wsymbol "test"
        lexeme $ TL.toStrict <$> takeWhileP (Just "test name") (/=':')

parseDefinition :: TestParser Toplevel
parseDefinition = label "symbol definition" $ toplevel ToplevelDefinition $ do
    def <- localState $ L.indentBlock scn $ do
        wsymbol "def"
        name <- varName
        argsDecl <- functionArguments (\off _ -> return . ( off, )) varName mzero (\_ -> return . VarName)
        atypes <- forM argsDecl $ \( off, vname :: VarName ) -> do
            tvar <- newTypeVar
            modify $ \s -> s { testVars = ( vname, ExprTypeVar tvar ) : testVars s }
            return ( off, vname, tvar )
        choice
            [ do
                osymbol ":"
                let finish steps = do
                        atypes' <- getInferredTypes atypes
                        ( name, ) . SomeExpr . ArgsReq atypes' . FunctionAbstraction <$> replaceDynArgs (mconcat steps)
                return $ L.IndentSome Nothing finish testStep
            , do
                osymbol "="
                SomeExpr (expr :: Expr e) <- someExpr
                atypes' <- getInferredTypes atypes
                L.IndentNone . ( name, ) . SomeExpr . ArgsReq atypes' . FunctionAbstraction <$> replaceDynArgs expr
            ]
    modify $ \s -> s { testVars = fmap someExprType def : testVars s }
    return def
  where
    getInferredTypes atypes = forM atypes $ \( off, vname, tvar@(TypeVar tvarname) ) -> do
        let err msg = do
                registerParseError . FancyError off . S.singleton . ErrorFail $ T.unpack msg
                return ( vname, SomeArgumentType (OptionalArgument @DynamicType) )
        gets (M.lookup tvar . testTypeUnif) >>= \case
            Just (ExprTypePrim (_ :: Proxy a)) -> return ( vname, SomeArgumentType (RequiredArgument @a) )
            Just (ExprTypeVar (TypeVar tvar')) -> err $ "ambiguous type for ‘" <> textVarName vname <> " : " <> tvar' <> "’"
            Just (ExprTypeFunction {}) -> err $ "unsupported function type of ‘" <> textVarName vname <> "’"
            Nothing -> err $ "ambiguous type for ‘" <> textVarName vname <> " : " <> tvarname <> "’"

    replaceDynArgs :: forall a. Expr a -> TestParser (Expr a)
    replaceDynArgs expr = do
        unif <- gets testTypeUnif
        return $ mapExpr (go unif) expr
      where
        go :: forall b. M.Map TypeVar SomeExprType -> Expr b -> Expr b
        go unif = \case
            ArgsApp args body -> ArgsApp (fmap replaceArgs args) body
              where
                replaceArgs (SomeExpr (DynVariable tvar sline vname))
                    | Just (ExprTypePrim (Proxy :: Proxy v)) <- M.lookup tvar unif
                    = SomeExpr (Variable sline vname :: Expr v)
                replaceArgs (SomeExpr e) = SomeExpr (go unif e)
            e -> e

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
            , testContext = SomeExpr (Undefined "void" :: Expr Void)
            , testNextTypeVar = 0
            , testTypeUnif = M.empty
            }
        res = runTestParser path content initState $ parseTestModule absPath

    case res of
         Left err -> putStr (errorBundlePretty err) >> exitFailure
         Right testModule -> return testModule
