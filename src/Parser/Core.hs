module Parser.Core where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Typeable

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Network ()
import Script.Expr
import Script.Expr.Class
import Script.Module
import Test
import Util

newtype TestParser a = TestParser (StateT TestParserState (ParsecT CustomTestError TestStream IO) a)
    deriving
        ( Functor, Applicative, Alternative, Monad
        , MonadState TestParserState
        , MonadPlus
        , MonadFail
        , MonadIO
        , MonadParsec CustomTestError TestStream
        )

type TestStream = TL.Text

type TestParseError = ParseError TestStream CustomTestError

data CustomTestError
    = ModuleNotFound ModuleName
    | FileNotFound FilePath
    | TestNotFound Text (Maybe FilePath)
    | TestOrTagNotFound Text (Maybe FilePath)
    | ImportModuleError (ParseErrorBundle TestStream CustomTestError)
    deriving (Eq)

instance Ord CustomTestError where
    compare (ModuleNotFound a) (ModuleNotFound b) = compare a b
    compare (ModuleNotFound _) _                  = LT
    compare _                  (ModuleNotFound _) = GT

    compare (FileNotFound a) (FileNotFound b) = compare a b
    compare (FileNotFound _) _                = LT
    compare _                (FileNotFound _) = GT

    compare (TestNotFound a a') (TestNotFound b b') = compare ( a, a' ) ( b, b' )
    compare (TestNotFound _ _ ) _                   = LT
    compare _                   (TestNotFound _ _ ) = GT

    compare (TestOrTagNotFound a a') (TestOrTagNotFound b b') = compare ( a, a' ) ( b, b' )
    compare (TestOrTagNotFound _ _ ) _                        = LT
    compare _                        (TestOrTagNotFound _ _ ) = GT

    -- Ord instance is required to store errors in Set, but there shouldn't be
    -- two ImportModuleErrors at the same possition, so "dummy" comparison
    -- should be ok.
    compare (ImportModuleError _) (ImportModuleError _) = EQ

instance ShowErrorComponent CustomTestError where
    showErrorComponent (ImportModuleError bundle) = "error parsing imported module:\n" <> errorBundlePretty bundle
    showErrorComponent err = showCustomTestError err

showCustomTestError :: CustomTestError -> String
showCustomTestError = \case
    ModuleNotFound name -> "module ‘" <> T.unpack (textModuleName name) <> "’ not found"
    FileNotFound path -> "file ‘" <> path <> "’ not found"
    TestNotFound tname mbpath -> "test ‘" <> T.unpack tname <> "’ not found" <> maybe "" (\path -> " in ‘" <> path <> "’") mbpath
    TestOrTagNotFound tname mbpath -> "test or tag ‘" <> T.unpack tname <> "’ not found" <> maybe "" (\path -> " in ‘" <> path <> "’") mbpath
    ImportModuleError bundle -> errorBundlePretty bundle

runTestParser :: TestStream -> TestParserState -> TestParser a -> IO (Either (ParseErrorBundle TestStream CustomTestError) a)
runTestParser content initState (TestParser parser) = flip (flip runParserT (testSourcePath initState)) content . flip evalStateT initState $ parser

data Toplevel
    = ToplevelTest Test
    | ToplevelDefinition ( VarName, SomeExpr )
    | ToplevelExport VarName
    | ToplevelImport ( ModuleName, VarName )

data TestParserState = TestParserState
    { testSourcePath :: FilePath
    , testVars :: [ ( VarName, ( FqVarName, SomeExprType )) ]
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
        ExprTypeConstr1 _ -> return $ SomeExpr $ (Undefined "incomplete type" :: Expr DynamicType)
        ExprTypeFunction args (ExprTypePrim (_ :: Proxy a)) -> return $ SomeExpr $ (FunVariable args sline fqn :: Expr (FunctionType a))
        stype -> return $ SomeExpr $ DynVariable stype sline fqn

lookupScalarVarExpr :: Int -> SourceLine -> VarName -> TestParser SomeExpr
lookupScalarVarExpr off sline name = do
    ( fqn, etype ) <- lookupVarType off name
    case etype of
        ExprTypePrim (Proxy :: Proxy a) -> return $ SomeExpr $ (Variable sline fqn :: Expr a)
        ExprTypeConstr1 _ -> return $ SomeExpr $ (Undefined "incomplete type" :: Expr DynamicType)
        ExprTypeFunction args (ExprTypePrim (pa :: Proxy a)) -> do
            SomeExpr <$> unifyExpr off pa (FunVariable args sline fqn :: Expr (FunctionType a))
        stype -> return $ SomeExpr $ DynVariable stype sline fqn


resolveKnownTypeVars :: SomeExprType -> TestParser ( SomeExprType, [ TypeVar ] )
resolveKnownTypeVars = fmap (fmap (uniq . sort)) . runWriterT . go
    where
      go stype = case stype of
        ExprTypePrim {} -> return stype
        ExprTypeConstr1 {} -> return stype
        ExprTypeVar tvar -> do
            gets (M.lookup tvar . testTypeUnif) >>= \case
                Just stype' -> go stype'
                Nothing -> tell [ tvar ] >> return stype
        ExprTypeFunction args body -> ExprTypeFunction <$> go args <*> go body
        ExprTypeArguments args -> ExprTypeArguments <$> mapM (\(SomeArgumentType a t) -> SomeArgumentType a <$> go t) args
        ExprTypeApp ctor params -> do
            ctor' <- go ctor
            params' <- mapM go params
            return $ case ( ctor', params' ) of
                ( ExprTypeConstr1 (Proxy :: Proxy c'), [ ExprTypePrim (Proxy :: Proxy p') ] )
                    -> ExprTypePrim (Proxy :: Proxy (c' p'))
                _ -> ExprTypeApp ctor' params'
        ExprTypeForall tvar inner -> ExprTypeForall tvar <$> go inner

typeClosure :: SomeExprType -> TestParser SomeExprType
typeClosure stype = do
    ( stype', freeVars ) <- resolveKnownTypeVars stype
    return $ go freeVars stype'
  where
    go []       t = t
    go (v : vs) t = ExprTypeForall v $ go vs t

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

unify _ res@(ExprTypeConstr1 (Proxy :: Proxy a)) (ExprTypeConstr1 (Proxy :: Proxy b))
    | Just (Refl :: a :~: b) <- eqT
    = return res

unify off (ExprTypeFunction args res) (ExprTypeFunction args' res')
    = ExprTypeFunction
        <$> unify off args args'
        <*> unify off res res'

unify off (ExprTypeApp ac aparams) (ExprTypeApp bc bparams)
    | length aparams == length bparams
    = do
        c <- unify off ac bc
        params <- zipWithM (unify off) aparams bparams
        return $ case ( c, params ) of
            ( ExprTypeConstr1 (Proxy :: Proxy c'), [ ExprTypePrim (Proxy :: Proxy p') ] )
                -> ExprTypePrim (Proxy :: Proxy (c' p'))
            _ -> ExprTypeApp c params

unify off a@(ExprTypeApp {}) (ExprTypePrim bproxy)
    | TypeDeconstructor1 c p <- matchTypeConstructor bproxy
    = unify off a (ExprTypeApp (ExprTypeConstr1 c) [ ExprTypePrim p ])

unify off (ExprTypePrim aproxy) b@(ExprTypeApp {})
    | TypeDeconstructor1 c p <- matchTypeConstructor aproxy
    = unify off (ExprTypeApp (ExprTypeConstr1 c) [ ExprTypePrim p ]) b

unify off a b = do
    parseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $
        "couldn't match expected type ‘" <> textSomeExprType a <> "’ with actual type ‘" <> textSomeExprType b <> "’"


unifyArguments
    :: FunctionArguments SomeArgumentType
    -> FunctionArguments ( Int, SomeExpr )
    -> TestParser ( FunctionArguments SomeExpr, ( FunctionArguments SomeArgumentType, FunctionArguments ( Int, SomeExpr ) ) )
unifyArguments (FunctionArguments am) (FunctionArguments bm) = (toArgs *** (toArgs *** toArgs)) <$> go (M.toAscList am) (M.toAscList bm)
  where
    toArgs = FunctionArguments . M.fromAscList
    go [] bs = return ( [], ( [], bs ) )
    go as [] = return ( [], ( as, [] ) )
    go (a@( ak, SomeArgumentType _ at ) : as) (b@( bk, ( off, expr ) ) : bs)
        | ak < bk = second (first  (a :)) <$> go as (b : bs)
        | bk < ak = second (second (b :)) <$> go (a : as) bs
        | otherwise = do
            expr' <- unifySomeExpr off at expr
            first (( ak, expr' ) :) <$> go as bs


unifyExpr :: forall a b proxy. (ExprType a, ExprType b) => Int -> proxy a -> Expr b -> TestParser (Expr a)
unifyExpr off pa expr = if
    | Just (Refl :: a :~: b) <- eqT
    -> return expr

    | DynVariable stype sline name <- expr
    , ExprTypeForall qvar itype <- stype
    -> do
        tvar <- newTypeVar
        res <- unify off (ExprTypePrim (Proxy :: Proxy a)) $ renameVarInType qvar tvar itype
        rtype <- M.lookup tvar <$> gets testTypeUnif
        return $ ExposePrimType $ TypeApp res (fromMaybe (ExprTypeVar tvar) rtype) (Variable sline name)

    | DynVariable stype sline name <- expr
    -> do
        _ <- unify off (ExprTypePrim (Proxy :: Proxy a)) stype
        return $ Variable sline name

    | HidePrimType (_ :: Expr b') <- expr
    -> unifyExpr off pa (ExposePrimType expr :: Expr b')

    | HideFunType args (_ :: Expr (FunctionType b')) <- expr
    -> unifyExpr off pa (ExposeFunType args expr :: Expr (FunctionType b'))

    | TypeLambda tvar t f <- expr
    -> do
        _ <- unify off (ExprTypePrim (Proxy :: Proxy a)) t
        Just (ExprTypePrim pt) <- M.lookup tvar <$> gets testTypeUnif
        unifyExpr off pa (f $ ExprTypePrim pt)

    | Just (Refl :: FunctionType a :~: b) <- eqT
    -> evalRemainingArguments off (exprArgs expr) expr

    | Just (Refl :: DynamicType :~: b) <- eqT
    , Undefined msg <- expr
    -> do
        return $ Undefined msg

    | otherwise
    -> do
        parseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $
            "couldn't match expected type ‘" <> textExprType pa <> "’ with actual type ‘" <> textExprType expr <> "’"


evalRemainingArguments :: ExprType a => Int -> FunctionArguments SomeArgumentType -> Expr (FunctionType a) -> TestParser (Expr a)
evalRemainingArguments off (FunctionArguments remaining) expr = do
    let showType ( Nothing, SomeArgumentType _ stype ) = "‘<" <> textSomeExprType stype <> ">’"
        showType ( Just (ArgumentKeyword kw), SomeArgumentType _ stype ) = "‘" <> kw <> " <" <> textSomeExprType stype <> ">’"
        err = parseError . FancyError off . S.singleton . ErrorFail . T.unpack

    defaults <- fmap catMaybes $ forM (M.toAscList remaining) $ \case
        arg@( _, SomeArgumentType RequiredArgument _ ) -> err $ "missing " <> showType arg <> " argument"
        ( _, SomeArgumentType OptionalArgument _ ) -> return Nothing
        ( kw, SomeArgumentType (ExprDefault def) _ ) -> return $ Just ( kw, def )
        ( kw, SomeArgumentType ContextDefault (ExprTypePrim atype) ) -> do
            SomeExpr context <- gets testContext
            context' <- unifyExpr off atype context
            return $ Just ( kw, SomeExpr context' )
        ( _, SomeArgumentType ContextDefault _ ) -> err "non-primitive context requirement"
    sline <- getSourceLine
    return (FunctionEval sline $ ArgsApp (FunctionArguments $ M.fromAscList defaults) expr)


unifySomeExpr :: Int -> SomeExprType -> SomeExpr -> TestParser SomeExpr
unifySomeExpr off stype sexpr@(SomeExpr (expr :: Expr a))
    | ExprTypePrim pa <- stype
    = SomeExpr <$> unifyExpr off pa expr

    | ExprTypeConstr1 {} <- stype
    = parseError $ FancyError off $ S.singleton $ ErrorFail $ T.unpack $ "unification with incomplete type"

    | ExprTypeVar tvar <- stype
    = do
        _ <- unify off (ExprTypeVar tvar) (someExprType sexpr)
        return sexpr

    | Just (Refl :: a :~: DynamicType) <- eqT
    , ExprTypeForall qvar itype <- someExprType sexpr
    = do
        tvar <- newTypeVar
        itype' <- unify off stype $ renameVarInType qvar tvar itype
        rtype <- M.lookup tvar <$> gets testTypeUnif
        return $ SomeExpr (TypeApp itype' (fromMaybe (ExprTypeVar tvar) rtype) expr)

    | ExprTypeFunction args res <- stype
    = case someExprType sexpr of
        ExprTypeFunction args' res' -> do
            _ <- unify off args args'
            _ <- unify off res res'
            return sexpr
        _ -> do
            _ <- unify off args (ExprTypeArguments mempty)
            SomeExpr expr' <- unifySomeExpr off res sexpr
            return $ SomeExpr $ FunctionAbstraction expr'

    | ExprTypeApp _ _ <- stype
    , ExprTypeFunction args' res' <- someExprType sexpr
    = do
        ( _, ( remaining, _ ) ) <- case args' of
            ExprTypeArguments args'' -> do
                unifyArguments args'' mempty
            _ -> do
                _ <- unify off (ExprTypeArguments mempty) args'
                return ( mempty, ( mempty, mempty ) )
        unify off stype res' >>= \case
            ExprTypePrim (Proxy :: Proxy r) | Just (Refl :: a :~: FunctionType r) <- eqT ->
                SomeExpr <$> evalRemainingArguments off remaining expr
            _ | Just (Refl :: a :~: FunctionType DynamicType) <- eqT ->
                SomeExpr <$> evalRemainingArguments off remaining expr
            _ ->
                error $ "expecting function type, got: " <> show (typeRep expr)

    | otherwise
    = do
        _ <- unify off stype (someExprType sexpr)
        return sexpr


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
operatorChar = satisfy $ (`elem` [ '.', '+', '-', '*', '/', '=', '<', '>', '|' ])
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

listOf :: TestParser a -> TestParser [a]
listOf item = do
    x <- item
    (x:) <$> choice [ symbol "," >> listOf item, return [] ]

blockOf :: Monoid a => Pos -> TestParser a -> TestParser a
blockOf indent step = go
  where
    go = do
        scn
        pos <- L.indentLevel
        optional eof >>= \case
            Just _ -> return mempty
            _ | pos <  indent -> return mempty
              | pos == indent -> mappend <$> step <*> go
              | otherwise     -> L.incorrectIndent EQ indent pos


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
