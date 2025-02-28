{-# OPTIONS_GHC -Wno-orphans #-}

module Parser (
    parseTestFiles,
) where

import Control.Monad
import Control.Monad.State

import Data.IORef
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
import System.IO.Error

import Network
import Parser.Core
import Parser.Expr
import Parser.Statement
import Script.Expr
import Script.Module
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

parseDefinition :: TestParser ( VarName, SomeExpr )
parseDefinition = label "symbol definition" $ do
    def@( name, expr ) <- localState $ L.indentBlock scn $ do
        wsymbol "def"
        name <- varName
        argsDecl <- functionArguments (\off _ -> return . ( off, )) varName mzero (\_ -> return . VarName)
        atypes <- forM argsDecl $ \( off, vname :: VarName ) -> do
            tvar <- newTypeVar
            modify $ \s -> s { testVars = ( vname, ( LocalVarName vname, ExprTypeVar tvar )) : testVars s }
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
    modify $ \s -> s { testVars = ( name, ( GlobalVarName (testCurrentModuleName s) name, someExprType expr )) : testVars s }
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

parseExport :: TestParser [ Toplevel ]
parseExport = label "export declaration" $ toplevel id $ do
    wsymbol "export"
    choice
      [ do
        def@( name, _ ) <- parseDefinition
        return [ ToplevelDefinition def, ToplevelExport name ]
      , do
        names <- listOf varName
        eol >> scn
        return $ map ToplevelExport names
      ]

parseImport :: TestParser [ Toplevel ]
parseImport = label "import declaration" $ toplevel (\() -> []) $ do
    wsymbol "import"
    modName <- parseModuleName
    importedModule <- getOrParseModule modName
    modify $ \s -> s { testVars = map (fmap (fmap someExprType)) (moduleExportedDefinitions importedModule) ++ testVars s }
    eol >> scn

parseTestModule :: FilePath -> TestParser Module
parseTestModule absPath = do
    scn
    moduleName <- choice
        [ label "module declaration" $ do
            wsymbol "module"
            off <- stateOffset <$> getParserState
            name@(ModuleName tname) <- parseModuleName
            when (or (zipWith (/=) (reverse tname) (reverse $ map T.pack $ splitDirectories $ dropExtension $ absPath))) $ do
                registerParseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $
                    "module name does not match file path"
            eol >> scn
            return name
        , do
            return $ ModuleName [ T.pack $ takeBaseName absPath ]
        ]
    modify $ \s -> s { testCurrentModuleName = moduleName }
    toplevels <- fmap concat $ many $ choice
        [ (: []) <$> parseTestDefinition
        , (: []) <$> toplevel ToplevelDefinition parseDefinition
        , parseExport
        , parseImport
        ]
    let moduleTests = catMaybes $ map (\case ToplevelTest x -> Just x; _ -> Nothing) toplevels
        moduleDefinitions = catMaybes $ map (\case ToplevelDefinition x -> Just x; _ -> Nothing) toplevels
        moduleExports = catMaybes $ map (\case ToplevelExport x -> Just x; _ -> Nothing) toplevels
    eof
    return Module {..}

parseTestFiles :: [ FilePath ] -> IO ( [ Module ], [ Module ] )
parseTestFiles paths = do
    parsedModules <- newIORef []
    requestedModules <- reverse <$> foldM (go parsedModules) [] paths
    allModules <- map snd <$> readIORef parsedModules
    return ( requestedModules, allModules )
  where
    go parsedModules res path = do
        let moduleName = error "current module name should be set at the beginning of parseTestModule"
        parseTestFile parsedModules moduleName path >>= \case
            Left (ImportModuleError bundle) -> do
                putStr (errorBundlePretty bundle)
                exitFailure
            Left err -> do
                putStr (showErrorComponent err)
                exitFailure
            Right cur -> do
                return $ cur : res

parseTestFile :: IORef [ ( FilePath, Module ) ] -> ModuleName -> FilePath -> IO (Either CustomTestError Module)
parseTestFile parsedModules moduleName path = do
    absPath <- makeAbsolute path
    (lookup absPath <$> readIORef parsedModules) >>= \case
        Just found -> return $ Right found
        Nothing -> do
            let initState = TestParserState
                    { testVars = concat
                        [ map (\(( mname, name ), value ) -> ( name, ( GlobalVarName mname name, someVarValueType value ))) $ M.toList builtins
                        ]
                    , testContext = SomeExpr (Undefined "void" :: Expr Void)
                    , testNextTypeVar = 0
                    , testTypeUnif = M.empty
                    , testCurrentModuleName = moduleName
                    , testParseModule = \(ModuleName current) mname@(ModuleName imported) -> do
                        let projectRoot = iterate takeDirectory absPath !! length current
                        parseTestFile parsedModules mname $ projectRoot </> foldr (</>) "" (map T.unpack imported) <.> takeExtension absPath
                    }
            mbContent <- (Just <$> TL.readFile path) `catchIOError` \e ->
                if isDoesNotExistError e then return Nothing else ioError e
            case mbContent of
                Just content -> do
                    runTestParser path content initState (parseTestModule absPath) >>= \case
                        Left bundle -> do
                            return $ Left $ ImportModuleError bundle
                        Right testModule -> do
                            modifyIORef parsedModules (( absPath, testModule ) : )
                            return $ Right testModule
                Nothing -> return $ Left $ ModuleNotFound moduleName
