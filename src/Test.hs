module Test (
    Module(..), ModuleName(..), textModuleName, moduleExportedDefinitions,
    Test(..),
    TestStep(..),
    TestBlock(..),
    SourceLine(..), textSourceLine,

    MonadEval(..), lookupVar, tryLookupVar, withVar,
    VarName(..), textVarName, unpackVarName,
    FqVarName(..), textFqVarName, unpackFqVarName, unqualifyName,
    TypedVarName(..), withTypedVar,
    ExprType(..), SomeExpr(..),
    TypeVar(..), SomeExprType(..), someExprType, textSomeExprType,
    FunctionType, DynamicType,

    VarValue(..), SomeVarValue(..), GlobalDefs,
    svvVariables, svvArguments,
    someConstValue, fromConstValue,
    fromSomeVarValue, textSomeVarValue, someVarValueType,

    Expr(..), varExpr, mapExpr, eval, evalSome, evalSomeWith,
    Traced(..), EvalTrace, VarNameSelectors, gatherVars,
    AppAnnotation(..),

    ArgumentKeyword(..), FunctionArguments(..),
    anull, exprArgs,
    SomeArgumentType(..), ArgumentType(..),

    Regex(RegexPart, RegexString), regexMatch,
) where

import Control.Monad
import Control.Monad.Reader

import Data.Char
import Data.Foldable
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Scientific
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable

import Text.Regex.TDFA qualified as RE
import Text.Regex.TDFA.Text qualified as RE

import {-# SOURCE #-} Network
import {-# SOURCE #-} Process
import Script.Expr.Class
import Util

data Module = Module
    { moduleName :: ModuleName
    , moduleTests :: [ Test ]
    , moduleDefinitions :: [ ( VarName, SomeExpr ) ]
    , moduleExports :: [ VarName ]
    }

newtype ModuleName = ModuleName [ Text ]
    deriving (Eq, Ord, Show)

textModuleName :: ModuleName -> Text
textModuleName (ModuleName parts) = T.intercalate "." parts

moduleExportedDefinitions :: Module -> [ ( VarName, ( FqVarName, SomeExpr )) ]
moduleExportedDefinitions Module {..} =
    map (\( var, expr ) -> ( var, ( GlobalVarName moduleName var, expr ))) $
        filter ((`elem` moduleExports) . fst) moduleDefinitions

data Test = Test
    { testName :: Text
    , testSteps :: Expr TestBlock
    }

newtype TestBlock = TestBlock [ TestStep ]
    deriving (Semigroup, Monoid)

data TestStep
    = Subnet (TypedVarName Network) Network (Network -> TestBlock)
    | DeclNode (TypedVarName Node) Network (Node -> TestBlock)
    | Spawn (TypedVarName Process) (Either Network Node) (Process -> TestBlock)
    | Send Process Text
    | Expect SourceLine Process (Traced Regex) [ TypedVarName Text ] ([ Text ] -> TestBlock)
    | Flush Process (Maybe Regex)
    | Guard SourceLine EvalTrace Bool
    | DisconnectNode Node TestBlock
    | DisconnectNodes Network TestBlock
    | DisconnectUpstream Network TestBlock
    | PacketLoss Scientific Node TestBlock
    | Wait

data SourceLine
    = SourceLine Text
    | SourceLineBuiltin

textSourceLine :: SourceLine -> Text
textSourceLine (SourceLine text) = text
textSourceLine SourceLineBuiltin = "<builtin>"


class MonadFail m => MonadEval m where
    askGlobalDefs :: m GlobalDefs
    askDictionary :: m VariableDictionary
    withDictionary :: (VariableDictionary -> VariableDictionary) -> m a -> m a

type VariableDictionary = [ ( VarName, SomeVarValue ) ]

lookupVar :: MonadEval m => FqVarName -> m SomeVarValue
lookupVar name = maybe (fail $ "variable not in scope: '" ++ unpackFqVarName name ++ "'") return =<< tryLookupVar name

tryLookupVar :: MonadEval m => FqVarName -> m (Maybe SomeVarValue)
tryLookupVar (LocalVarName name) = lookup name <$> askDictionary
tryLookupVar (GlobalVarName mname var) = M.lookup ( mname, var ) <$> askGlobalDefs

withVar :: (MonadEval m, ExprType e) => VarName -> e -> m a -> m a
withVar name value = withDictionary (( name, someConstValue value ) : )


newtype VarName = VarName Text
    deriving (Eq, Ord)

textVarName :: VarName -> Text
textVarName (VarName name) = name

unpackVarName :: VarName -> String
unpackVarName = T.unpack . textVarName

data FqVarName
    = GlobalVarName ModuleName VarName
    | LocalVarName VarName
    deriving (Eq, Ord)

textFqVarName :: FqVarName -> Text
textFqVarName (GlobalVarName mname vname) = textModuleName mname <> "." <> textVarName vname
textFqVarName (LocalVarName vname) = textVarName vname

unpackFqVarName :: FqVarName -> String
unpackFqVarName = T.unpack . textFqVarName

unqualifyName :: FqVarName -> VarName
unqualifyName (GlobalVarName _ name) = name
unqualifyName (LocalVarName name) = name

newtype TypedVarName a = TypedVarName { fromTypedVarName :: VarName }
    deriving (Eq, Ord)

withTypedVar :: (MonadEval m, ExprType e) => TypedVarName e -> e -> m a -> m a
withTypedVar (TypedVarName name) = withVar name

isInternalVar :: FqVarName -> Bool
isInternalVar (GlobalVarName {}) = False
isInternalVar (LocalVarName (VarName name))
    | Just ( '$', _ ) <- T.uncons name = True
    | otherwise                        = False


instance ExprType Regex where
    textExprType _ = T.pack "regex"
    textExprValue _ = T.pack "<regex>"

instance ExprType TestBlock where
    textExprType _ = "test block"
    textExprValue _ = "<test block>"


data FunctionType a = FunctionType (FunctionArguments SomeVarValue -> a)

instance ExprType a => ExprType (FunctionType a) where
    textExprType _ = "function type"
    textExprValue _ = "<function type>"

data DynamicType

instance ExprType DynamicType where
    textExprType _ = "ambiguous type"
    textExprValue _ = "<dynamic type>"

data SomeExpr = forall a. ExprType a => SomeExpr (Expr a)

newtype TypeVar = TypeVar Text
    deriving (Eq, Ord)

data SomeExprType
    = forall a. ExprType a => ExprTypePrim (Proxy a)
    | ExprTypeVar TypeVar
    | forall a. ExprType a => ExprTypeFunction (FunctionArguments SomeArgumentType) (Proxy a)

someExprType :: SomeExpr -> SomeExprType
someExprType (SomeExpr expr) = go expr
  where
    go :: forall e. ExprType e => Expr e -> SomeExprType
    go = \case
        DynVariable tvar _ _ -> ExprTypeVar tvar
        (e :: Expr a)
            | IsFunType <- asFunType e -> ExprTypeFunction (gof e) (proxyOfFunctionType e)
            | otherwise -> ExprTypePrim (Proxy @a)

    gof :: forall e. ExprType e => Expr (FunctionType e) -> FunctionArguments SomeArgumentType
    gof = \case
        Let _ _ _ body -> gof body
        Variable {} -> error "someExprType: gof: variable"
        FunVariable params _ _ -> params
        ArgsReq args body -> fmap snd args <> gof body
        ArgsApp (FunctionArguments used) body ->
            let FunctionArguments args = gof body
             in FunctionArguments $ args `M.difference` used
        FunctionAbstraction {} -> mempty
        FunctionEval {} -> error "someExprType: gof: function eval"
        Pure {} -> error "someExprType: gof: pure"
        App {} -> error "someExprType: gof: app"
        Undefined {} -> error "someExprType: gof: undefined"

    proxyOfFunctionType :: Expr (FunctionType a) -> Proxy a
    proxyOfFunctionType _ = Proxy

textSomeExprType :: SomeExprType -> Text
textSomeExprType (ExprTypePrim p) = textExprType p
textSomeExprType (ExprTypeVar (TypeVar name)) = name
textSomeExprType (ExprTypeFunction _ r) = "function:" <> textExprType r

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


data SomeVarValue = forall a. ExprType a => SomeVarValue (VarValue a)

svvVariables :: SomeVarValue -> EvalTrace
svvVariables (SomeVarValue vv) = vvVariables vv

svvArguments :: SomeVarValue -> FunctionArguments SomeArgumentType
svvArguments (SomeVarValue vv) = vvArguments vv

data VarValue a = VarValue
    { vvVariables :: EvalTrace
    , vvArguments :: FunctionArguments SomeArgumentType
    , vvFunction :: SourceLine -> FunctionArguments SomeVarValue -> a
    }

type GlobalDefs = Map ( ModuleName, VarName ) SomeVarValue

someConstValue :: ExprType a => a -> SomeVarValue
someConstValue = SomeVarValue . VarValue [] mempty . const . const

fromConstValue :: forall a m. (ExprType a, MonadFail m) => SourceLine -> FqVarName -> VarValue a -> m a
fromConstValue sline name (VarValue _ args value :: VarValue b) = do
    maybe (fail err) return $ do
        guard $ anull args
        cast $ value sline mempty
  where
    err = T.unpack $ T.concat [ T.pack "expected ", textExprType @a Proxy, T.pack ", but variable '", textFqVarName name, T.pack "' has type ",
            if anull args then textExprType @b Proxy else "function type" ]

fromSomeVarValue :: forall a m. (ExprType a, MonadFail m) => SourceLine -> FqVarName -> SomeVarValue -> m a
fromSomeVarValue sline name (SomeVarValue (VarValue _ args value :: VarValue b)) = do
    maybe (fail err) return $ do
        guard $ anull args
        cast $ value sline mempty
  where
    err = T.unpack $ T.concat [ T.pack "expected ", textExprType @a Proxy, T.pack ", but variable '", textFqVarName name, T.pack "' has type ",
            if anull args then textExprType @b Proxy else "function type" ]

textSomeVarValue :: SourceLine -> SomeVarValue -> Text
textSomeVarValue sline (SomeVarValue (VarValue _ args value))
    | anull args = textExprValue $ value sline mempty
    | otherwise  =  "<function>"

someVarValueType :: SomeVarValue -> SomeExprType
someVarValueType (SomeVarValue (VarValue _ args _ :: VarValue a))
    | anull args = ExprTypePrim (Proxy @a)
    | otherwise  = ExprTypeFunction args (Proxy @a)



data Expr a where
    Let :: forall a b. ExprType b => SourceLine -> TypedVarName b -> Expr b -> Expr a -> Expr a
    Variable :: ExprType a => SourceLine -> FqVarName -> Expr a
    DynVariable :: TypeVar -> SourceLine -> FqVarName -> Expr DynamicType
    FunVariable :: ExprType a => FunctionArguments SomeArgumentType -> SourceLine -> FqVarName -> Expr (FunctionType a)
    ArgsReq :: ExprType a => FunctionArguments ( VarName, SomeArgumentType ) -> Expr (FunctionType a) -> Expr (FunctionType a)
    ArgsApp :: ExprType a => FunctionArguments SomeExpr -> Expr (FunctionType a) -> Expr (FunctionType a)
    FunctionAbstraction :: ExprType a => Expr a -> Expr (FunctionType a)
    FunctionEval :: ExprType a => Expr (FunctionType a) -> Expr a
    LambdaAbstraction :: ExprType a => TypedVarName a -> Expr b -> Expr (a -> b)
    Pure :: a -> Expr a
    App :: AppAnnotation b -> Expr (a -> b) -> Expr a -> Expr b
    Concat :: [Expr Text] -> Expr Text
    Regex :: [Expr Regex] -> Expr Regex
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
        ArgsReq args expr -> f $ ArgsReq args (go expr)
        ArgsApp args expr -> f $ ArgsApp (fmap (\(SomeExpr e) -> SomeExpr (go e)) args) (go expr)
        FunctionAbstraction expr -> f $ FunctionAbstraction (go expr)
        FunctionEval expr -> f $ FunctionEval (go expr)
        LambdaAbstraction tvar expr -> f $ LambdaAbstraction tvar (go expr)
        e@Pure {} -> f e
        App ann efun earg -> f $ App ann (go efun) (go earg)
        e@Concat {} -> f e
        e@Regex {} -> f e
        e@Undefined {} -> f e
        Trace expr -> f $ Trace (go expr)


newtype SimpleEval a = SimpleEval (Reader ( GlobalDefs, VariableDictionary ) a)
    deriving (Functor, Applicative, Monad)

runSimpleEval :: SimpleEval a -> GlobalDefs -> VariableDictionary -> a
runSimpleEval (SimpleEval x) = curry $ runReader x

instance MonadFail SimpleEval where
    fail = error . ("eval failed: " <>)

instance MonadEval SimpleEval where
    askGlobalDefs = SimpleEval (asks fst)
    askDictionary = SimpleEval (asks snd)
    withDictionary f (SimpleEval inner) = SimpleEval (local (fmap f) inner)


eval :: forall m a. MonadEval m => Expr a -> m a
eval = \case
    Let _ (TypedVarName name) valExpr expr -> do
        val <- eval valExpr
        withVar name val $ eval expr
    Variable sline name -> fromSomeVarValue sline name =<< lookupVar name
    DynVariable _ _ name -> fail $ "ambiguous type of ‘" <> unpackFqVarName name <> "’"
    FunVariable _ sline name -> funFromSomeVarValue sline name =<< lookupVar name
    ArgsReq (FunctionArguments req) efun -> do
        gdefs <- askGlobalDefs
        dict <- askDictionary
        return $ FunctionType $ \(FunctionArguments args) ->
            let used = M.intersectionWith (\value ( vname, _ ) -> ( vname, value )) args req
                FunctionType fun = runSimpleEval (eval efun) gdefs (toList used ++ dict)
             in fun $ FunctionArguments $ args `M.difference` req
    ArgsApp eargs efun -> do
        FunctionType fun <- eval efun
        args <- mapM evalSome eargs
        return $ FunctionType $ \args' -> fun (args <> args')
    FunctionAbstraction expr -> do
        val <- eval expr
        return $ FunctionType $ const val
    FunctionEval efun -> do
        FunctionType fun <- eval efun
        return $ fun mempty
    LambdaAbstraction (TypedVarName name) expr -> do
        gdefs <- askGlobalDefs
        dict <- askDictionary
        return $ \x -> runSimpleEval (eval expr) gdefs (( name, someConstValue x ) : dict)
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
        <*> pure (const fun)

evalSome :: MonadEval m => SomeExpr -> m SomeVarValue
evalSome (SomeExpr expr)
    | IsFunType <- asFunType expr = SomeVarValue <$> evalFunToVarValue expr
    | otherwise = SomeVarValue <$> evalToVarValue expr

evalSomeWith :: GlobalDefs -> SomeExpr -> SomeVarValue
evalSomeWith gdefs sexpr = runSimpleEval (evalSome sexpr) gdefs []


data Traced a = Traced EvalTrace a

type VarNameSelectors = ( FqVarName, [ Text ] )
type EvalTrace = [ ( VarNameSelectors, SomeVarValue ) ]

gatherVars :: forall a m. MonadEval m => Expr a -> m EvalTrace
gatherVars = fmap (uniqOn fst . sortOn fst) . helper
  where
    helper :: forall b. Expr b -> m EvalTrace
    helper = \case
        Let _ (TypedVarName var) _ expr -> withDictionary (filter ((var /=) . fst)) $ helper expr
        Variable _ var
            | isInternalVar var -> return []
            | otherwise -> maybe [] (\x -> [ (( var, [] ), x ) ]) <$> tryLookupVar var
        DynVariable _ _ var -> maybe [] (\x -> [ (( var, [] ), x ) ]) <$> tryLookupVar var
        FunVariable _ _ var -> maybe [] (\x -> [ (( var, [] ), x ) ]) <$> tryLookupVar var
        ArgsReq args expr -> withDictionary (filter ((`notElem` map fst (toList args)) . fst)) $ helper expr
        ArgsApp (FunctionArguments args) fun -> do
            v <- helper fun
            vs <- mapM (\(SomeExpr e) -> helper e) $ M.elems args
            return $ concat (v : vs)
        FunctionAbstraction expr -> helper expr
        FunctionEval efun -> helper efun
        LambdaAbstraction (TypedVarName var) expr -> withDictionary (filter ((var /=) . fst)) $ helper expr
        Pure _ -> return []
        e@(App (AnnRecord sel) _ x)
            | Just (var, sels) <- gatherSelectors x
            -> do
                val <- SomeVarValue . VarValue [] mempty . const . const <$> eval e
                return [ (( var, sels ++ [ sel ] ), val ) ]
            | otherwise -> do
                helper x
        App _ f x -> (++) <$> helper f <*> helper x
        Concat es -> concat <$> mapM helper es
        Regex es -> concat <$> mapM helper es
        Undefined {} -> return []
        Trace expr -> helper expr

    gatherSelectors :: forall b. Expr b -> Maybe ( FqVarName, [ Text ] )
    gatherSelectors = \case
        Variable _ var -> Just (var, [])
        App (AnnRecord sel) _ x -> do
            (var, sels) <- gatherSelectors x
            return (var, sels ++ [sel])
        _ -> Nothing


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
    Pure {} -> error "exprArgs: pure"
    App {} -> error "exprArgs: app"
    Undefined {} -> error "exprArgs: undefined"

funFromSomeVarValue :: forall a m. (ExprType a, MonadFail m) => SourceLine -> FqVarName -> SomeVarValue -> m (FunctionType a)
funFromSomeVarValue sline name (SomeVarValue (VarValue _ args value :: VarValue b)) = do
    maybe (fail err) return $ do
        FunctionType <$> cast (value sline)
  where
    err = T.unpack $ T.concat [ T.pack "expected function returning ", textExprType @a Proxy, T.pack ", but variable '", textFqVarName name, T.pack "' has ",
            (if anull args then "type " else "function type returting ") <> textExprType @b Proxy ]

data SomeArgumentType = forall a. ExprType a => SomeArgumentType (ArgumentType a)

data ArgumentType a
    = RequiredArgument
    | OptionalArgument
    | ExprDefault (Expr a)
    | ContextDefault


data Regex = RegexCompiled Text RE.Regex
           | RegexPart Text
           | RegexString Text

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
