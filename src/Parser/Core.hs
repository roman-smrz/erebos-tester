module Parser.Core where

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Typeable

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Network ()
import Test

newtype TestParser a = TestParser (StateT TestParserState (ParsecT CustomTestError TestStream IO) a)
    deriving
        ( Functor, Applicative, Alternative, Monad
        , MonadState TestParserState
        , MonadPlus
        , MonadFail
        , MonadParsec CustomTestError TestStream
        )

type TestStream = TL.Text

type TestParseError = ParseError TestStream CustomTestError

data CustomTestError
    = ModuleNotFound ModuleName
    | ImportModuleError (ParseErrorBundle TestStream CustomTestError)
    deriving (Eq)

instance Ord CustomTestError where
    compare (ModuleNotFound a) (ModuleNotFound b) = compare a b
    compare (ModuleNotFound _) _                  = LT
    compare _                  (ModuleNotFound _) = GT

    -- Ord instance is required to store errors in Set, but there shouldn't be
    -- two ImportModuleErrors at the same possition, so "dummy" comparison
    -- should be ok.
    compare (ImportModuleError _) (ImportModuleError _) = EQ

instance ShowErrorComponent CustomTestError where
    showErrorComponent (ModuleNotFound name) = "module `" <> T.unpack (textModuleName name) <> "' not found"
    showErrorComponent (ImportModuleError bundle) = "error parsing imported module:\n" <> errorBundlePretty bundle

runTestParser :: String -> TestStream -> TestParserState -> TestParser a -> IO (Either (ParseErrorBundle TestStream CustomTestError) a)
runTestParser path content initState (TestParser parser) = flip (flip runParserT path) content . flip evalStateT initState $ parser

data Toplevel
    = ToplevelTest Test
    | ToplevelDefinition ( VarName, SomeExpr )
    | ToplevelExport VarName
    | ToplevelImport ( ModuleName, VarName )

data TestParserState = TestParserState
    { testVars :: [ ( VarName, ( FqVarName, SomeExprType )) ]
    , testContext :: SomeExpr
    , testNextTypeVar :: Int
    , testTypeUnif :: Map TypeVar SomeExprType
    , testCurrentModuleName :: ModuleName
    , testParseModule :: ModuleName -> ModuleName -> IO (Either CustomTestError Module)
    }

newTypeVar :: TestParser TypeVar
newTypeVar = do
    idx <- gets testNextTypeVar
    modify $ \s -> s { testNextTypeVar = idx + 1 }
    return $ TypeVar $ T.pack $ 'a' : show idx

lookupVarType :: Int -> VarName -> TestParser ( FqVarName, SomeExprType )
lookupVarType off name = do
    gets (lookup name . testVars) >>= \case
        Nothing -> do
            registerParseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $
                "variable not in scope: `" <> textVarName name <> "'"
            vtype <- ExprTypeVar <$> newTypeVar
            let fqName = LocalVarName name
            modify $ \s -> s { testVars = ( name, ( fqName, vtype )) : testVars s }
            return ( fqName, vtype )
        Just ( fqName, t@(ExprTypeVar tvar) ) -> do
            ( fqName, ) <$> gets (fromMaybe t . M.lookup tvar . testTypeUnif)
        Just x -> return x

lookupVarExpr :: Int -> SourceLine -> VarName -> TestParser SomeExpr
lookupVarExpr off sline name = do
    ( fqn, etype ) <- lookupVarType off name
    case etype of
        ExprTypePrim (Proxy :: Proxy a) -> return $ SomeExpr $ (Variable sline fqn :: Expr a)
        ExprTypeVar tvar -> return $ SomeExpr $ DynVariable tvar sline fqn
        ExprTypeFunction args (_ :: Proxy a) -> return $ SomeExpr $ (FunVariable args sline fqn :: Expr (FunctionType a))

lookupScalarVarExpr :: Int -> SourceLine -> VarName -> TestParser SomeExpr
lookupScalarVarExpr off sline name = do
    ( fqn, etype ) <- lookupVarType off name
    case etype of
        ExprTypePrim (Proxy :: Proxy a) -> return $ SomeExpr $ (Variable sline fqn :: Expr a)
        ExprTypeVar tvar -> return $ SomeExpr $ DynVariable tvar sline fqn
        ExprTypeFunction args (pa :: Proxy a) -> do
            SomeExpr <$> unifyExpr off pa (FunVariable args sline fqn :: Expr (FunctionType a))

unify :: Int -> SomeExprType -> SomeExprType -> TestParser SomeExprType
unify _ (ExprTypeVar aname) (ExprTypeVar bname) | aname == bname = do
    cur <- gets testTypeUnif
    case M.lookup aname cur of
        Just a -> return a
        Nothing -> return (ExprTypeVar aname)

unify off (ExprTypeVar aname) (ExprTypeVar bname) = do
    cur <- gets testTypeUnif
    case ( M.lookup aname cur, M.lookup bname cur ) of
        ( Just a, Just b ) -> do
            c <- unify off a b
            modify $ \s -> s { testTypeUnif = M.insert aname c $ M.insert bname c $ cur }
            return c

        ( Just a, Nothing ) -> do
            modify $ \s -> s { testTypeUnif = M.insert bname a $ cur }
            return a

        ( Nothing, Just b ) -> do
            modify $ \s -> s { testTypeUnif = M.insert aname b $ cur }
            return b

        ( Nothing, Nothing ) -> do
            let b = ExprTypeVar bname
            modify $ \s -> s { testTypeUnif = M.insert aname b $ cur }
            return b

unify off (ExprTypeVar aname) b = do
    cur <- gets testTypeUnif
    case M.lookup aname cur of
        Just a -> do
            c <- unify off a b
            modify $ \s -> s { testTypeUnif = M.insert aname c $ cur }
            return c
        Nothing -> do
            modify $ \s -> s { testTypeUnif = M.insert aname b $ cur }
            return b

unify off a (ExprTypeVar bname) = do
    cur <- gets testTypeUnif
    case M.lookup bname cur of
        Just b -> do
            c <- unify off a b
            modify $ \s -> s { testTypeUnif = M.insert bname c $ cur }
            return c

        Nothing -> do
            modify $ \s -> s { testTypeUnif = M.insert bname a $ cur }
            return a

unify _ res@(ExprTypePrim (Proxy :: Proxy a)) (ExprTypePrim (Proxy :: Proxy b))
    | Just (Refl :: a :~: b) <- eqT
    = return res

unify off a b = do
    parseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $
        "couldn't match expected type `" <> textSomeExprType a <> "' with actual type `" <> textSomeExprType b <> "'"


unifyExpr :: forall a b proxy. (ExprType a, ExprType b) => Int -> proxy a -> Expr b -> TestParser (Expr a)
unifyExpr off pa expr = if
    | Just (Refl :: a :~: b) <- eqT
    -> return expr

    | DynVariable tvar sline name <- expr
    -> do
        _ <- unify off (ExprTypePrim (Proxy :: Proxy a)) (ExprTypeVar tvar)
        return $ Variable sline name

    | Just (Refl :: FunctionType a :~: b) <- eqT
    -> do
        let FunctionArguments remaining = exprArgs expr
            showType ( Nothing, SomeArgumentType atype ) = "`<" <> textExprType atype <> ">'"
            showType ( Just (ArgumentKeyword kw), SomeArgumentType atype ) = "`" <> kw <> " <" <> textExprType atype <> ">'"
            err = parseError . FancyError off . S.singleton . ErrorFail . T.unpack

        defaults <- fmap catMaybes $ forM (M.toAscList remaining) $ \case
            arg@(_, SomeArgumentType RequiredArgument) -> err $ "missing " <> showType arg <> " argument"
            (_, SomeArgumentType OptionalArgument) -> return Nothing
            (kw, SomeArgumentType (ExprDefault def)) -> return $ Just ( kw, SomeExpr def )
            (kw, SomeArgumentType atype@ContextDefault) -> do
                SomeExpr context <- gets testContext
                context' <- unifyExpr off atype context
                return $ Just ( kw, SomeExpr context' )
        return (FunctionEval $ ArgsApp (FunctionArguments $ M.fromAscList defaults) expr)

    | Just (Refl :: DynamicType :~: b) <- eqT
    , Undefined msg <- expr
    -> do
        return $ Undefined msg

    | otherwise
    -> do
        parseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $
            "couldn't match expected type `" <> textExprType pa <> "' with actual type `" <> textExprType expr <> "'"


skipLineComment :: TestParser ()
skipLineComment = L.skipLineComment $ TL.pack "#"

scn :: TestParser ()
scn = L.space space1 skipLineComment empty

sc :: TestParser ()
sc = L.space hspace1 skipLineComment empty

wordChar :: TestParser (Token TestStream)
wordChar = alphaNumChar <|> char '_'

lexeme :: TestParser a -> TestParser a
lexeme = L.lexeme sc

symbol, osymbol, wsymbol :: String -> TestParser ()
symbol str  = void $       (string (TL.pack str)) <* sc
osymbol str = void $ try $ (string (TL.pack str) <* notFollowedBy operatorChar) <* sc
wsymbol str = void $ try $ (string (TL.pack str) <* notFollowedBy wordChar) <* sc

operatorChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
operatorChar = satisfy $ (`elem` ['.', '+', '-', '*', '/', '='])
{-# INLINE operatorChar #-}

localState :: TestParser a -> TestParser a
localState inner = do
    s <- get
    x <- inner
    s' <- get
    put s { testNextTypeVar = testNextTypeVar s', testTypeUnif = testTypeUnif s' }
    return x

toplevel :: (a -> b) -> TestParser a -> TestParser b
toplevel f = return . f <=< L.nonIndented scn

block :: (a -> [b] -> TestParser c) -> TestParser a -> TestParser b -> TestParser c
block merge header item = L.indentBlock scn $ do
    h <- header
    choice
        [ do symbol ":"
             return $ L.IndentSome Nothing (merge h) item
        , L.IndentNone <$> merge h []
        ]

listOf :: TestParser a -> TestParser [a]
listOf item = do
    x <- item
    (x:) <$> choice [ symbol "," >> listOf item, return [] ]


getSourceLine :: TestParser SourceLine
getSourceLine = do
    pstate <- statePosState <$> getParserState
    return $ SourceLine $ T.concat
        [ T.pack $ sourcePosPretty $ pstateSourcePos pstate
        , T.pack ": "
        , TL.toStrict $ TL.takeWhile (/='\n') $ pstateInput pstate
        ]


getOrParseModule :: ModuleName -> TestParser Module
getOrParseModule name = do
    current <- gets testCurrentModuleName
    parseModule <- gets testParseModule
    (TestParser $ lift $ lift $ parseModule current name) >>= \case
        Right parsed -> return parsed
        Left err -> customFailure err
