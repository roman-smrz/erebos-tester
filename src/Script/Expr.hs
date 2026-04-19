module Script.Expr (
    Expr(..), varExpr, mapExpr,

    MonadEval(..), VariableDictionary, GlobalDefs,
    lookupVar, tryLookupVar, withVar, withTypedVar,
    eval, evalSome, evalSomeWith,

    FunctionType, DynamicType,
    ExprType(..), SomeExpr(..),
    TypeVar(..), SomeExprType(..), someExprType, textSomeExprType,
    renameTypeVar, renameVarInType,

    VarValue(..), SomeVarValue(..),
    svvVariables, svvArguments,
    someConstValue, fromConstValue,
    fromSomeVarValue, textSomeVarValue, someVarValueType,

    ArgumentKeyword(..), FunctionArguments(..),
    anull, exprArgs,
    SomeArgumentType(..), ArgumentType(..),

    Traced(..), EvalTrace, CallStack(..), VarNameSelectors, gatherVars,
    AppAnnotation(..),
    callStackVarName, callStackFqVarName,

    module Script.Var,

    Regex(RegexPart, RegexString),
    regexCompile, regexMatch,
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

import Data.Char
import Data.Foldable
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Scientific
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable

import Text.Regex.TDFA qualified as RE
import Text.Regex.TDFA.Text qualified as RE

import Script.Expr.Class
import Script.Var
import Util


data Expr a where
    Let :: forall a b. ExprType b => SourceLine -> TypedVarName b -> Expr b -> Expr a -> Expr a
    Variable :: ExprType a => SourceLine -> FqVarName -> Expr a
    DynVariable :: SomeExprType -> SourceLine -> FqVarName -> Expr DynamicType
    FunVariable :: ExprType a => FunctionArguments SomeArgumentType -> SourceLine -> FqVarName -> Expr (FunctionType a)
    OptVariable :: ExprType a => SourceLine -> FqVarName -> Expr (Maybe a)
    ArgsReq :: ExprType a => FunctionArguments ( VarName, SomeArgumentType ) -> Expr (FunctionType a) -> Expr (FunctionType a)
    ArgsApp :: ExprType a => FunctionArguments SomeExpr -> Expr (FunctionType a) -> Expr (FunctionType a)
    FunctionAbstraction :: ExprType a => Expr a -> Expr (FunctionType a)
    FunctionEval :: ExprType a => SourceLine -> Expr (FunctionType a) -> Expr a
    HideType :: forall a. ExprType a => Expr a -> Expr DynamicType
    TypeLambda :: TypeVar -> SomeExprType -> (SomeExprType -> Expr DynamicType) -> Expr DynamicType
    TypeApp :: forall a. ExprType a => Expr DynamicType -> SomeExprType -> Expr a
    LambdaAbstraction :: ExprType a => TypedVarName a -> Expr b -> Expr (a -> b)
    Pure :: a -> Expr a
    App :: AppAnnotation b -> Expr (a -> b) -> Expr a -> Expr b
    Concat :: [ Expr Text ] -> Expr Text
    Regex :: [ Expr Regex ] -> Expr Regex
    Undefined :: String -> Expr a
    Trace :: Expr a -> Expr (Traced a)

data AppAnnotation b = AnnNone
                     | ExprType b => AnnRecord Text

instance Functor Expr where
    fmap f x = Pure f <*> x

instance Applicative Expr where
    pure = Pure
    (<*>) = App AnnNone

instance Semigroup a => Semigroup (Expr a) where
    e <> f = (<>) <$> e <*> f

instance Monoid a => Monoid (Expr a) where
    mempty = Pure mempty

varExpr :: ExprType a => SourceLine -> TypedVarName a -> Expr a
varExpr sline (TypedVarName name) = Variable sline (LocalVarName name)

mapExpr :: forall a. (forall b. Expr b -> Expr b) -> Expr a -> Expr a
mapExpr f = go
  where
    go :: forall c. Expr c -> Expr c
    go = \case
        Let sline vname vval expr -> f $ Let sline vname (go vval) (go expr)
        e@Variable {} -> f e
        e@DynVariable {} -> f e
        e@FunVariable {} -> f e
        e@OptVariable {} -> f e
        ArgsReq args expr -> f $ ArgsReq args (go expr)
        ArgsApp args expr -> f $ ArgsApp (fmap (\(SomeExpr e) -> SomeExpr (go e)) args) (go expr)
        FunctionAbstraction expr -> f $ FunctionAbstraction (go expr)
        FunctionEval sline expr -> f $ FunctionEval sline (go expr)
        HideType expr -> HideType $ go expr
        TypeLambda tvar stype efun -> TypeLambda tvar stype (go . efun)
        TypeApp expr stype -> TypeApp (go expr) stype
        LambdaAbstraction tvar expr -> f $ LambdaAbstraction tvar (go expr)
        e@Pure {} -> f e
        App ann efun earg -> f $ App ann (go efun) (go earg)
        e@Concat {} -> f e
        e@Regex {} -> f e
        e@Undefined {} -> f e
        Trace expr -> f $ Trace (go expr)



class MonadFail m => MonadEval m where
    askGlobalDefs :: m GlobalDefs
    askDictionary :: m VariableDictionary
    withDictionary :: (VariableDictionary -> VariableDictionary) -> m a -> m a

type GlobalDefs = Map ( ModuleName, VarName ) SomeExpr

type VariableDictionary = [ ( VarName, SomeExpr ) ]

lookupVar :: MonadEval m => FqVarName -> m SomeExpr
lookupVar name = maybe (fail $ "variable not in scope: '" ++ unpackFqVarName name ++ "'") return =<< tryLookupVar name

tryLookupVar :: MonadEval m => FqVarName -> m (Maybe SomeExpr)
tryLookupVar (LocalVarName name) = lookup name <$> askDictionary
tryLookupVar (GlobalVarName mname var) = M.lookup ( mname, var ) <$> askGlobalDefs

withVar :: (MonadEval m, ExprType e) => VarName -> e -> m a -> m a
withVar name value = withDictionary (( name, SomeExpr (Pure value) ) : )

withTypedVar :: (MonadEval m, ExprType e) => TypedVarName e -> e -> m a -> m a
withTypedVar (TypedVarName name) = withVar name

isInternalVar :: FqVarName -> Bool
isInternalVar (GlobalVarName {}) = False
isInternalVar (LocalVarName (VarName name))
    | Just ( '$', _ ) <- T.uncons name = True
    | otherwise                        = False



newtype SimpleEval a = SimpleEval (ReaderT ( GlobalDefs, VariableDictionary ) (Except String) a)
    deriving (Functor, Applicative, Monad, MonadError String)

runSimpleEval :: SimpleEval a -> GlobalDefs -> VariableDictionary -> a
runSimpleEval (SimpleEval x) gdefs dict = either error id $ runExcept $ runReaderT x ( gdefs, dict )

trySimpleEval :: SimpleEval a -> GlobalDefs -> VariableDictionary -> Maybe a
trySimpleEval (SimpleEval x) gdefs dict = either (const Nothing) Just $ runExcept $ runReaderT x ( gdefs, dict )

instance MonadFail SimpleEval where
    fail = throwError . ("eval failed: " <>)

instance MonadEval SimpleEval where
    askGlobalDefs = SimpleEval (asks fst)
    askDictionary = SimpleEval (asks snd)
    withDictionary f (SimpleEval inner) = SimpleEval (local (fmap f) inner)

callStackVarName :: VarName
callStackVarName = VarName "$STACK"

callStackFqVarName :: FqVarName
callStackFqVarName = LocalVarName callStackVarName

eval :: forall m a. MonadEval m => Expr a -> m a
eval = \case
    Let _ (TypedVarName name) valExpr expr -> do
        val <- eval valExpr
        withVar name val $ eval expr
    Variable _ name -> evalSomeExpr name =<< lookupVar name
    DynVariable _ _ name -> fail $ "ambiguous type of ‘" <> unpackFqVarName name <> "’"
    FunVariable _ _ name -> evalSomeExpr name =<< lookupVar name
    OptVariable _ name -> maybe (return Nothing) (fmap Just . evalSomeExpr name) =<< tryLookupVar name
    ArgsReq (FunctionArguments req) efun -> do
        gdefs <- askGlobalDefs
        dict <- askDictionary
        return $ FunctionType $ \stack (FunctionArguments args) ->
            let used = M.intersectionWith (\(SomeVarValue value) ( vname, _ ) -> ( vname, SomeExpr $ Pure $ vvFunction value (CallStack []) mempty )) args req
                FunctionType fun = runSimpleEval (eval efun) gdefs (toList used ++ dict)
             in fun stack $ FunctionArguments $ args `M.difference` req
    ArgsApp eargs efun -> do
        FunctionType fun <- eval efun
        args <- mapM evalSome eargs
        return $ FunctionType $ \stack args' -> fun stack (args <> args')
    FunctionAbstraction expr -> do
        gdefs <- askGlobalDefs
        dict <- askDictionary
        return $ FunctionType $ \stack _ ->
            runSimpleEval (eval expr) gdefs (( callStackVarName, SomeExpr (Pure stack) ) : filter ((callStackVarName /=) . fst) dict)
    FunctionEval sline efun -> do
        vars <- gatherVars efun
        CallStack cs <- maybe (return $ CallStack []) (evalSomeExpr callStackFqVarName) =<< tryLookupVar callStackFqVarName
        let cs' = CallStack (( sline, vars ) : cs)
        FunctionType fun <- withVar callStackVarName cs' $ eval efun
        return $ fun cs' mempty
    HideType expr -> DynamicType <$> eval expr
    TypeLambda _ _ f -> do
        gdefs <- askGlobalDefs
        dict <- askDictionary
        return $ DynamicType $ \t -> runSimpleEval (eval $ f t) gdefs dict
    TypeApp expr stype -> do
        DynamicType f <- eval expr
        case cast f of
            Just f' -> do
                case f' stype of
                    DynamicType x -> case cast x of
                        Just x' -> return x'
                        n@Nothing -> fail $ "type error in type application result " <> show ( typeOf x, typeOf n )
            n@Nothing -> fail $ "type error in type application " <> show ( typeOf f, typeOf n )
    LambdaAbstraction (TypedVarName name) expr -> do
        gdefs <- askGlobalDefs
        dict <- askDictionary
        return $ \x -> runSimpleEval (eval expr) gdefs (( name, SomeExpr $ Pure x ) : dict)
    Pure value -> return value
    App _ f x -> eval f <*> eval x
    Concat xs -> T.concat <$> mapM eval xs
    Regex xs -> mapM eval xs >>= \case
        [ re@RegexCompiled {} ] -> return re
        parts -> case regexCompile $ T.concat $ map regexSource parts of
            Left err -> fail err
            Right re -> return re
    Undefined err -> fail err
    Trace expr -> Traced <$> gatherVars expr <*> eval expr

evalSomeExpr :: forall m a. (MonadEval m, ExprType a) => FqVarName -> SomeExpr -> m a
evalSomeExpr name (SomeExpr (e :: Expr b)) = do
    maybe (fail err) eval $ cast e
  where
    err = T.unpack $ T.concat [ T.pack "expected ", textExprType @a Proxy, T.pack ", but variable ‘", textFqVarName name, T.pack "’ has type type ",
            textExprType @b Proxy ]

evalToVarValue :: MonadEval m => Expr a -> m (VarValue a)
evalToVarValue expr = do
    VarValue
        <$> gatherVars expr
        <*> pure mempty
        <*> (const . const <$> eval expr)

evalFunToVarValue :: MonadEval m => Expr (FunctionType a) -> m (VarValue a)
evalFunToVarValue expr = do
    FunctionType fun <- eval expr
    VarValue
        <$> gatherVars expr
        <*> pure (exprArgs expr)
        <*> pure fun

evalSome :: MonadEval m => SomeExpr -> m SomeVarValue
evalSome (SomeExpr expr)
    | IsFunType <- asFunType expr = SomeVarValue <$> evalFunToVarValue expr
    | otherwise = SomeVarValue <$> evalToVarValue expr

evalSomeWith :: GlobalDefs -> SomeExpr -> SomeVarValue
evalSomeWith gdefs sexpr = runSimpleEval (evalSome sexpr) gdefs []


data FunctionType a = FunctionType (CallStack -> FunctionArguments SomeVarValue -> a)

instance ExprType a => ExprType (FunctionType a) where
    textExprType _ = "function type"
    textExprValue _ = "<function type>"

data DynamicType = forall a. Typeable a => DynamicType a

instance ExprType DynamicType where
    textExprType _ = "ambiguous type"
    textExprValue _ = "<dynamic type>"


data SomeExpr = forall a. ExprType a => SomeExpr (Expr a)

newtype TypeVar = TypeVar Text
    deriving (Eq, Ord)

data SomeExprType
    = forall a. ExprType a => ExprTypePrim (Proxy a)
    | forall a. ExprTypeConstr1 a => ExprTypeConstr1 (Proxy a)
    | ExprTypeVar TypeVar
    | ExprTypeFunction (FunctionArguments SomeArgumentType) SomeExprType
    | ExprTypeApp SomeExprType [ SomeExprType ]
    | ExprTypeForall TypeVar SomeExprType

someExprType :: SomeExpr -> SomeExprType
someExprType (SomeExpr expr) = go expr
  where
    go :: forall e. ExprType e => Expr e -> SomeExprType
    go = \case
        DynVariable stype _ _ -> stype
        HideType e -> go e
        TypeLambda tvar stype _ -> ExprTypeForall tvar stype

        ArgsReq args inner -> exprTypeFunction (fmap snd args) (go inner)
        ArgsApp (FunctionArguments used) inner
            | ExprTypeFunction (FunctionArguments args) x <- go inner
            -> ExprTypeFunction (FunctionArguments (args `M.difference` used)) x
        FunctionAbstraction inner -> exprTypeFunction mempty (go inner)
        FunctionEval _ inner
            | ExprTypeFunction _ x <- go inner -> x

        (_ :: Expr a) -> ExprTypePrim (Proxy @a)

    exprTypeFunction :: FunctionArguments SomeArgumentType -> SomeExprType -> SomeExprType
    exprTypeFunction args (ExprTypeFunction args' inner) = ExprTypeFunction (args <> args') inner
    exprTypeFunction args inner = ExprTypeFunction args inner


renameTypeVar :: TypeVar -> TypeVar -> Expr a -> Expr a
renameTypeVar a b = go
  where
    go :: Expr e -> Expr e
    go orig = case orig of
        Let sline vname x y -> Let sline vname (go x) (go y)
        Variable {} -> orig
        DynVariable stype sline name -> DynVariable (renameVarInType a b stype) sline name
        FunVariable {} -> orig
        OptVariable {} -> orig
        ArgsReq args body -> ArgsReq args (go body)
        ArgsApp args fun -> ArgsApp (fmap (renameTypeVarInSomeExpr a b) args) (go fun)
        FunctionAbstraction expr -> FunctionAbstraction (go expr)
        FunctionEval sline expr -> FunctionEval sline (go expr)
        HideType expr -> HideType (go expr)
        TypeLambda tvar stype expr
            | tvar == a -> orig
            | tvar == b -> error "type var collision"
            | otherwise -> TypeLambda tvar (renameVarInType a b stype) (go . expr)
        TypeApp expr stype -> TypeApp (go expr) (renameVarInType a b stype)
        LambdaAbstraction vname expr -> LambdaAbstraction vname (go expr)
        Pure {} -> orig
        App ann f x -> App ann (go f) (go x)
        Concat xs -> Concat (map go xs)
        Regex xs -> Regex (map go xs)
        Undefined {} -> orig
        Trace expr -> Trace (go expr)

renameTypeVarInSomeExpr :: TypeVar -> TypeVar -> SomeExpr -> SomeExpr
renameTypeVarInSomeExpr a b (SomeExpr e) = SomeExpr (renameTypeVar a b e)

renameVarInType :: TypeVar -> TypeVar -> SomeExprType -> SomeExprType
renameVarInType a b = go
  where
    go orig = case orig of
        ExprTypePrim {} -> orig
        ExprTypeConstr1 {} -> orig
        ExprTypeVar tvar | tvar == a -> ExprTypeVar b
                         | otherwise -> orig
        ExprTypeFunction {} -> orig
        ExprTypeApp c xs -> ExprTypeApp (go c) (map go xs)
        ExprTypeForall tvar stype
            | tvar == a -> orig
            | tvar == b -> error "type var collision"
            | otherwise -> ExprTypeForall tvar (go stype)


textSomeExprType :: SomeExprType -> Text
textSomeExprType = go []
  where
    go _ (ExprTypePrim p) = textExprType p
    go (x : _) (ExprTypeConstr1 c) = textExprTypeConstr1 c x
    go [] (ExprTypeConstr1 _) = "<incomplte type>"
    go _ (ExprTypeVar (TypeVar name)) = name
    go _ (ExprTypeFunction _ r) = "function:" <> textSomeExprType r
    go _ (ExprTypeApp c xs) = go (map textSomeExprType xs) c
    go _ (ExprTypeForall (TypeVar name) ctype) = "∀" <> name <> "." <> go [] ctype

data AsFunType a
    = forall b. (a ~ FunctionType b, ExprType b) => IsFunType
    | NotFunType

asFunType :: Expr a -> AsFunType a
asFunType = \case
    Let _ _ _ expr -> asFunType expr
    FunVariable {} -> IsFunType
    ArgsReq {} -> IsFunType
    ArgsApp {} -> IsFunType
    FunctionAbstraction {} -> IsFunType
    _ -> NotFunType


data VarValue a = VarValue
    { vvVariables :: EvalTrace
    , vvArguments :: FunctionArguments SomeArgumentType
    , vvFunction :: CallStack -> FunctionArguments SomeVarValue -> a
    }

data SomeVarValue = forall a. ExprType a => SomeVarValue (VarValue a)

svvVariables :: SomeVarValue -> EvalTrace
svvVariables (SomeVarValue vv) = vvVariables vv

svvArguments :: SomeVarValue -> FunctionArguments SomeArgumentType
svvArguments (SomeVarValue vv) = vvArguments vv

someConstValue :: ExprType a => a -> SomeVarValue
someConstValue = SomeVarValue . VarValue [] mempty . const . const

fromConstValue :: forall a m. (ExprType a, MonadFail m) => CallStack -> FqVarName -> VarValue a -> m a
fromConstValue stack name (VarValue _ args value :: VarValue b) = do
    maybe (fail err) return $ do
        guard $ anull args
        cast $ value stack mempty
  where
    err = T.unpack $ T.concat [ T.pack "expected ", textExprType @a Proxy, T.pack ", but variable '", textFqVarName name, T.pack "' has type ",
            if anull args then textExprType @b Proxy else "function type" ]

fromSomeVarValue :: forall a m. (ExprType a, MonadFail m) => CallStack -> FqVarName -> SomeVarValue -> m a
fromSomeVarValue stack name (SomeVarValue (VarValue _ args value :: VarValue b)) = do
    maybe (fail err) return $ do
        guard $ anull args
        cast $ value stack mempty
  where
    err = T.unpack $ T.concat [ T.pack "expected ", textExprType @a Proxy, T.pack ", but variable '", textFqVarName name, T.pack "' has type ",
            if anull args then textExprType @b Proxy else "function type" ]

textSomeVarValue :: SomeVarValue -> Text
textSomeVarValue (SomeVarValue (VarValue _ args value))
    | anull args = textExprValue $ value (CallStack []) mempty
    | otherwise  =  "<function>"

someVarValueType :: SomeVarValue -> SomeExprType
someVarValueType (SomeVarValue (VarValue _ args _ :: VarValue a))
    | anull args = ExprTypePrim (Proxy @a)
    | otherwise  = ExprTypeFunction args (ExprTypePrim (Proxy @a))


newtype ArgumentKeyword = ArgumentKeyword Text
    deriving (Show, Eq, Ord, IsString)

newtype FunctionArguments a = FunctionArguments (Map (Maybe ArgumentKeyword) a)
    deriving (Show, Semigroup, Monoid, Functor, Foldable, Traversable)

anull :: FunctionArguments a -> Bool
anull (FunctionArguments args) = M.null args

exprArgs :: Expr (FunctionType a) -> FunctionArguments SomeArgumentType
exprArgs = \case
    Let _ _ _ expr -> exprArgs expr
    Variable {} -> mempty
    FunVariable args _ _ -> args
    ArgsReq args expr -> fmap snd args <> exprArgs expr
    ArgsApp (FunctionArguments applied) expr ->
        let FunctionArguments args = exprArgs expr
         in FunctionArguments (args `M.difference` applied)
    FunctionAbstraction {} -> mempty
    FunctionEval {} -> mempty
    TypeApp {} -> error "exprArgs: type application"
    Pure {} -> error "exprArgs: pure"
    App {} -> error "exprArgs: app"
    Undefined {} -> error "exprArgs: undefined"

data SomeArgumentType = forall a. ExprType a => SomeArgumentType (ArgumentType a)

data ArgumentType a
    = RequiredArgument
    | OptionalArgument
    | ExprDefault (Expr a)
    | ContextDefault


data Traced a = Traced EvalTrace a

type VarNameSelectors = ( FqVarName, [ Text ] )
type EvalTrace = [ ( VarNameSelectors, SomeVarValue ) ]
newtype CallStack = CallStack [ ( SourceLine, EvalTrace ) ]

instance ExprType CallStack where
    textExprType _ = T.pack "callstack"
    textExprValue _ = T.pack "<callstack>"

gatherVars :: forall a m. MonadEval m => Expr a -> m EvalTrace
gatherVars = fmap (uniqOn fst . sortOn fst) . helper
  where
    helper :: forall b. Expr b -> m EvalTrace
    helper = \case
        Let _ (TypedVarName var) _ expr -> withDictionary (filter ((var /=) . fst)) $ helper expr
        e@(Variable _ var) -> gatherLocalVar var e
        e@(DynVariable _ _ var) -> gatherLocalVar var e
        e@(FunVariable _ _ var) -> gatherLocalVar var e
        e@(OptVariable _ var) -> gatherLocalVar var e
        ArgsReq args expr -> withDictionary (filter ((`notElem` map fst (toList args)) . fst)) $ helper expr
        ArgsApp (FunctionArguments args) fun -> do
            v <- helper fun
            vs <- mapM (\(SomeExpr e) -> helper e) $ M.elems args
            return $ concat (v : vs)
        FunctionAbstraction expr -> helper expr
        FunctionEval _ efun -> helper efun
        HideType expr -> helper expr
        TypeLambda {} -> return []
        TypeApp expr _ -> helper expr
        LambdaAbstraction (TypedVarName var) expr -> withDictionary (filter ((var /=) . fst)) $ helper expr
        Pure _ -> return []
        e@(App (AnnRecord sel) _ x)
            | Just (var, sels) <- gatherSelectors x
            -> do
                gdefs <- askGlobalDefs
                dict <- askDictionary
                let mbVal = SomeVarValue . VarValue [] mempty . const . const <$> trySimpleEval (eval e) gdefs dict
                return $ catMaybes [ (( var, sels ++ [ sel ] ), ) <$> mbVal ]
            | otherwise -> do
                helper x
        App _ f x -> (++) <$> helper f <*> helper x
        Concat es -> concat <$> mapM helper es
        Regex es -> concat <$> mapM helper es
        Undefined {} -> return []
        Trace expr -> helper expr

    gatherLocalVar :: forall b. ExprType b => FqVarName -> Expr b -> m EvalTrace
    gatherLocalVar var expr
        | GlobalVarName {} <- var = return []
        | isInternalVar var = return []
        | otherwise = do
            gdefs <- askGlobalDefs
            dict <- askDictionary
            let mbVal = SomeVarValue . VarValue [] mempty . const . const <$> trySimpleEval (eval expr) gdefs dict
            return $ maybe [] (\x -> [ ( ( var, [] ), x ) ]) mbVal

    gatherSelectors :: forall b. Expr b -> Maybe ( FqVarName, [ Text ] )
    gatherSelectors = \case
        Variable _ var -> Just (var, [])
        App (AnnRecord sel) _ x -> do
            (var, sels) <- gatherSelectors x
            return (var, sels ++ [sel])
        _ -> Nothing


data Regex = RegexCompiled Text RE.Regex
           | RegexPart Text
           | RegexString Text

instance ExprType Regex where
    textExprType _ = T.pack "regex"
    textExprValue _ = T.pack "<regex>"

    exprExpansionConvFrom = listToMaybe $ catMaybes
        [ cast (RegexString)
        , cast (RegexString . T.pack . show @Integer)
        , cast (RegexString . T.pack . show @Scientific)
        ]

regexCompile :: Text -> Either String Regex
regexCompile src = either Left (Right . RegexCompiled src) $ RE.compile RE.defaultCompOpt RE.defaultExecOpt $
    T.singleton '^' <> src <> T.singleton '$'

regexMatch :: Regex -> Text -> Either String (Maybe (Text, Text, Text, [Text]))
regexMatch (RegexCompiled _ re) text = RE.regexec re text
regexMatch _ _ = Left "regex not compiled"

regexSource :: Regex -> Text
regexSource (RegexCompiled src _) = src
regexSource (RegexPart src) = src
regexSource (RegexString str) = T.concatMap escapeChar str
  where
    escapeChar c | isAlphaNum c = T.singleton c
                 | c `elem` ['`', '\'', '<', '>'] = T.singleton c
                 | otherwise = T.pack ['\\', c]
