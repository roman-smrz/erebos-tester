{-# OPTIONS_GHC -Wno-orphans #-}

module Parser (
    parseTestFiles,
    CustomTestError(..),
) where

import Control.Monad
import Control.Monad.Except
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
import System.FilePath
import System.IO.Error

import Asset
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
        href <- L.indentLevel
        testNameBase <- header
        testNameModule <- gets testCurrentModuleName
        let testName = TestName {..}

        osymbol ":" <* eol <* scn

        ref <- L.indentGuard scn GT href
        testTags <- preamble ref
        testSteps <- fmap Scope <$> testBlock ref
        return Test {..}

  where
    header = do
        wsymbol "test"
        lexeme $ TL.toStrict <$> takeWhileP (Just "test name") (/=':')

    preamble :: Pos -> TestParser [ Expr Tag ]
    preamble ref = fmap catMaybes $ many $ do
        void $ L.indentGuard scn EQ ref
        off <- stateOffset <$> getParserState
        name <- try $ identifier <* osymbol ":"
            <* ((eol >> mzero) <|> return ()) -- continue only if not on EOL
        case name of
            "tag" -> do
                Just <$> typedExpr FunctionTerm <* eol <* scn
            _ -> do
                registerParseError $ FancyError off $ S.singleton $ ErrorFail $
                    "unexpected test metadata ‘" <> T.unpack name <> "’"
                takeWhileP Nothing (/= '\n') *> eol *> scn *> return Nothing


parseDefinition :: Pos -> TestParser ( VarName, SomeExpr )
parseDefinition href = label "symbol definition" $ do
    def@( name, expr ) <- localState $ do
        wsymbol "def"
        name <- varName
        argsDecl <- functionArguments (\off _ -> return . ( off, ))
            (typeAnnotated varName) mzero (\_ -> return . (, Nothing) . VarName)
        atypes <- forM argsDecl $ \( off, ( vname :: VarName, mbstype :: Maybe SomeExprType ) ) -> do
            stype <- maybe (ExprTypeVar <$> newTypeVar) return mbstype
            modify $ \s -> s { testVars = ( vname, ( LocalVarName vname, stype )) : testVars s }
            return ( off, vname, stype )
        SomeExpr expr <- choice
            [ do
                osymbol ":"
                scn
                ref <- L.indentGuard scn GT href
                SomeExpr <$> testBlock ref
            , do
                osymbol "="
                someExpr FunctionTerm <* eol
            ]
        scn
        atypes' <- getInferredTypes atypes
        sexpr <- SomeExpr . ArgsReq atypes' . FunctionAbstraction <$> replaceDynArgs expr
        return ( name, sexpr )
    modify $ \s -> s { testVars = ( name, ( GlobalVarName (testCurrentModuleName s) name, someExprType expr )) : testVars s }
    return def
  where
    getInferredTypes atypes = forM atypes $ \( _, vname, stype ) -> do
        ( vname, ) . SomeArgumentType OptionalArgument <$> typeClosure stype

    replaceDynArgs :: forall a. Expr a -> TestParser (Expr a)
    replaceDynArgs expr = do
        unif <- gets testTypeUnif
        return $ mapExpr (go unif) expr
      where
        go :: forall b. M.Map TypeVar SomeExprType -> Expr b -> Expr b
        go unif = \case
            ArgsApp args body -> ArgsApp (fmap replaceArgs args) body
              where
                replaceArgs (SomeExpr (DynVariable (ExprTypeVar tvar) sline vname))
                    | Just (ExprTypePrim (Proxy :: Proxy v)) <- M.lookup tvar unif
                    = SomeExpr (Variable sline vname :: Expr v)
                replaceArgs (SomeExpr e) = SomeExpr (go unif e)
            e -> e

    typeAnnotated p = do
        x <- p
        choice
            [ do
                void $ osymbol ":"
                stype <- typeExpr
                return ( x, Just stype )

            , do
                return ( x, Nothing )
            ]

parseAsset :: Pos -> TestParser ( VarName, SomeExpr )
parseAsset href = label "asset definition" $ do
    wsymbol "asset"
    name <- varName
    osymbol ":"
    void eol
    ref <- L.indentGuard scn GT href

    wsymbol "path"
    osymbol ":"
    off <- stateOffset <$> getParserState
    path <- TL.unpack <$> takeWhile1P Nothing (/= '\n')
    dir <- takeDirectory <$> gets testSourcePath
    absPath <- liftIO (makeAbsolute $ dir </> path)
    let assetPath = AssetPath absPath
    liftIO (doesPathExist absPath) >>= \case
        True -> return ()
        False -> registerParseError $ FancyError off $ S.singleton $ ErrorCustom $ FileNotFound absPath

    void $ L.indentGuard scn LT ref
    let expr = SomeExpr $ Pure Asset {..}
    modify $ \s -> s { testVars = ( name, ( GlobalVarName (testCurrentModuleName s) name, someExprType expr )) : testVars s }
    return ( name, expr )

parseTag :: Pos -> TestParser ( VarName, SomeExpr )
parseTag _ = label "tag definition" $ do
    wsymbol "tag"
    name <- constrName
    void eol
    cmn <- gets testCurrentModuleName
    let expr = SomeExpr $ Pure $ Tag cmn name
    modify $ \s -> s { testVars = ( name, ( GlobalVarName cmn name, someExprType expr )) : testVars s }
    scn
    return ( name, expr )

parseExport :: TestParser [ Toplevel ]
parseExport = label "export declaration" $ toplevel id $ do
    ref <- L.indentLevel
    wsymbol "export"
    choice
      [ do
        def@( name, _ ) <- parseDefinition ref <|> parseAsset ref <|> parseTag ref
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
        , (: []) <$> toplevel ToplevelDefinition (parseDefinition pos1)
        , (: []) <$> toplevel ToplevelDefinition (parseAsset pos1)
        , (: []) <$> toplevel ToplevelDefinition (parseTag pos1)
        , parseExport
        , parseImport
        ]
    let moduleTests = catMaybes $ map (\case ToplevelTest x -> Just x; _ -> Nothing) toplevels
        moduleDefinitions = catMaybes $ map (\case ToplevelDefinition x -> Just x; _ -> Nothing) toplevels
        moduleExports = catMaybes $ map (\case ToplevelExport x -> Just x; _ -> Nothing) toplevels
    eof
    return Module {..}

parseTestFiles :: [ SomePrimType ] -> [ FilePath ] -> IO (Either CustomTestError ( [ Module ], [ Module ] ))
parseTestFiles builtinTypes paths = do
    parsedModules <- newIORef []
    runExceptT $ do
        requestedModules <- reverse <$> foldM (go parsedModules) [] paths
        allModules <- map snd <$> liftIO (readIORef parsedModules)
        return ( requestedModules, allModules )
  where
    builtinTypes' = map (\(SomePrimType p) -> ( VarName (textExprType p), ExprTypePrim p )) builtinTypes
    go parsedModules res path = do
        liftIO (parseTestFile builtinTypes' parsedModules Nothing path) >>= \case
            Left err -> do
                throwError err
            Right cur -> do
                return $ cur : res

parseTestFile :: [ ( VarName, SomeExprType ) ] -> IORef [ ( FilePath, Module ) ] -> Maybe ModuleName -> FilePath -> IO (Either CustomTestError Module)
parseTestFile builtinTypes parsedModules mbModuleName path = do
    absPath <- makeAbsolute path
    (lookup absPath <$> readIORef parsedModules) >>= \case
        Just found -> return $ Right found
        Nothing -> do
            let initState = TestParserState
                    { testSourcePath = path
                    , testVars = concat
                        [ map (\(( mname, name ), value ) -> ( name, ( GlobalVarName mname name, someExprType value ))) $ M.toList builtins
                        ]
                    , testTypeVars = builtinTypes
                    , testContext = SomeExpr (Undefined "void" :: Expr Void)
                    , testNextTypeVar = 0
                    , testTypeUnif = M.empty
                    , testCurrentModuleName = fromMaybe (error "current module name should be set at the beginning of parseTestModule") mbModuleName
                    , testParseModule = \(ModuleName current) mname@(ModuleName imported) -> do
                        let projectRoot = iterate takeDirectory absPath !! length current
                        parseTestFile builtinTypes parsedModules (Just mname) $ projectRoot </> foldr (</>) "" (map T.unpack imported) <.> takeExtension absPath
                    }
            mbContent <- (Just <$> TL.readFile path) `catchIOError` \e ->
                if isDoesNotExistError e then return Nothing else ioError e
            case mbContent of
                Just content -> do
                    runTestParser content initState (parseTestModule absPath) >>= \case
                        Left bundle -> do
                            return $ Left $ ImportModuleError bundle
                        Right testModule -> do
                            modifyIORef parsedModules (( absPath, testModule ) : )
                            return $ Right testModule
                Nothing -> return $ Left $ maybe (FileNotFound path) ModuleNotFound mbModuleName
