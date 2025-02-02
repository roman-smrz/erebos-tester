module Test (
    Module(..), ModuleName(..), textModuleName,
    Test(..),
    TestStep(..),
    TestBlock(..),
    SourceLine(..), textSourceLine,

    MonadEval(..), lookupVar, tryLookupVar, withVar,
    VarName(..), TypedVarName(..), textVarName, unpackVarName, withTypedVar,
    ExprType(..), SomeExpr(..),
    TypeVar(..), SomeExprType(..), someExprType, textSomeExprType,
    FunctionType, DynamicType,

    VarValue(..), SomeVarValue(..),
    svvVariables, svvArguments,
    someConstValue, fromConstValue,
    fromSomeVarValue, textSomeVarValue, someVarValueType,

    RecordSelector(..),
    ExprListUnpacker(..),
    ExprEnumerator(..),
    Expr(..), varExpr, mapExpr, eval, evalSome,
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
import Data.Void

import Text.Regex.TDFA qualified as RE
import Text.Regex.TDFA.Text qualified as RE

import {-# SOURCE #-} Network
import {-# SOURCE #-} Process
import Util

data Module = Module
    { moduleName :: ModuleName
    , moduleTests :: [ Test ]
    , moduleDefinitions :: [ ( VarName, SomeExpr ) ]
    , moduleExports :: [ VarName ]
    }

newtype ModuleName = ModuleName [ Text ]
    deriving (Eq, Ord)

textModuleName :: ModuleName -> Text
textModuleName (ModuleName parts) = T.intercalate "." parts

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
    askDictionary :: m VariableDictionary
    withDictionary :: (VariableDictionary -> VariableDictionary) -> m a -> m a

type VariableDictionary = [ ( VarName, SomeVarValue ) ]

lookupVar :: MonadEval m => VarName -> m SomeVarValue
lookupVar name = maybe (fail $ "variable not in scope: '" ++ unpackVarName name ++ "'") return . lookup name =<< askDictionary

tryLookupVar :: MonadEval m => VarName -> m (Maybe SomeVarValue)
tryLookupVar name = lookup name <$> askDictionary

withVar :: (MonadEval m, ExprType e) => VarName -> e -> m a -> m a
withVar name value = withDictionary (( name, someConstValue value ) : )

newtype VarName = VarName Text
    deriving (Eq, Ord, Show)

newtype TypedVarName a = TypedVarName { fromTypedVarName :: VarName }
    deriving (Eq, Ord)

textVarName :: VarName -> Text
textVarName (VarName name ) = name

unpackVarName :: VarName -> String
unpackVarName = T.unpack . textVarName

isInternalVar :: VarName -> Bool
isInternalVar (VarName name)
    | Just ( '$', _ ) <- T.uncons name = True
    | otherwise                        = False

withTypedVar :: (MonadEval m, ExprType e) => TypedVarName e -> e -> m a -> m a
withTypedVar (TypedVarName name) = withVar name


class Typeable a => ExprType a where
    textExprType :: proxy a -> Text
    textExprValue :: a -> Text

    recordMembers :: [(Text, RecordSelector a)]
    recordMembers = []

    exprListUnpacker :: proxy a -> Maybe (ExprListUnpacker a)
    exprListUnpacker _ = Nothing

    exprEnumerator :: proxy a -> Maybe (ExprEnumerator a)
    exprEnumerator _ = Nothing

instance ExprType Integer where
    textExprType _ = T.pack "integer"
    textExprValue x = T.pack (show x)

    exprEnumerator _ = Just $ ExprEnumerator enumFromTo enumFromThenTo

instance ExprType Scientific where
    textExprType _ = T.pack "number"
    textExprValue x = T.pack (show x)

instance ExprType Bool where
    textExprType _ = T.pack "bool"
    textExprValue True = T.pack "true"
    textExprValue False = T.pack "false"

instance ExprType Text where
    textExprType _ = T.pack "string"
    textExprValue x = T.pack (show x)

instance ExprType Regex where
    textExprType _ = T.pack "regex"
    textExprValue _ = T.pack "<regex>"

instance ExprType Void where
    textExprType _ = T.pack "void"
    textExprValue _ = T.pack "<void>"

instance ExprType a => ExprType [a] where
    textExprType _ = "[" <> textExprType @a Proxy <> "]"
    textExprValue x = "[" <> T.intercalate ", " (map textExprValue x) <> "]"

    exprListUnpacker _ = Just $ ExprListUnpacker id (const Proxy)

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

someConstValue :: ExprType a => a -> SomeVarValue
someConstValue = SomeVarValue . VarValue [] mempty . const . const

fromConstValue :: forall a m. (ExprType a, MonadFail m) => SourceLine -> VarName -> VarValue a -> m a
fromConstValue sline name (VarValue _ args value :: VarValue b) = do
    maybe (fail err) return $ do
        guard $ anull args
        cast $ value sline mempty
  where
    err = T.unpack $ T.concat [ T.pack "expected ", textExprType @a Proxy, T.pack ", but variable '", textVarName name, T.pack "' has type ",
            if anull args then textExprType @b Proxy else "function type" ]

fromSomeVarValue :: forall a m. (ExprType a, MonadFail m) => SourceLine -> VarName -> SomeVarValue -> m a
fromSomeVarValue sline name (SomeVarValue (VarValue _ args value :: VarValue b)) = do
    maybe (fail err) return $ do
        guard $ anull args
        cast $ value sline mempty
  where
    err = T.unpack $ T.concat [ T.pack "expected ", textExprType @a Proxy, T.pack ", but variable '", textVarName name, T.pack "' has type ",
            if anull args then textExprType @b Proxy else "function type" ]

textSomeVarValue :: SourceLine -> SomeVarValue -> Text
textSomeVarValue sline (SomeVarValue (VarValue _ args value))
    | anull args = textExprValue $ value sline mempty
    | otherwise  =  "<function>"

someVarValueType :: SomeVarValue -> SomeExprType
someVarValueType (SomeVarValue (VarValue _ args _ :: VarValue a))
    | anull args = ExprTypePrim (Proxy @a)
    | otherwise  = ExprTypeFunction args (Proxy @a)


data RecordSelector a = forall b. ExprType b => RecordSelector (a -> b)

data ExprListUnpacker a = forall e. ExprType e => ExprListUnpacker (a -> [e]) (Proxy a -> Proxy e)

data ExprEnumerator a = ExprEnumerator (a -> a -> [a]) (a -> a -> a -> [a])


data Expr a where
    Let :: forall a b. ExprType b => SourceLine -> TypedVarName b -> Expr b -> Expr a -> Expr a
    Variable :: ExprType a => SourceLine -> VarName -> Expr a
    DynVariable :: TypeVar -> SourceLine -> VarName -> Expr DynamicType
    FunVariable :: ExprType a => FunctionArguments SomeArgumentType -> SourceLine -> VarName -> Expr (FunctionType a)
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
varExpr sline (TypedVarName name) = Variable sline name

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


newtype SimpleEval a = SimpleEval (Reader VariableDictionary a)
    deriving (Functor, Applicative, Monad)

runSimpleEval :: SimpleEval a -> VariableDictionary -> a
runSimpleEval (SimpleEval x) = runReader x

instance MonadFail SimpleEval where
    fail = error . ("eval failed: " <>)

instance MonadEval SimpleEval where
    askDictionary = SimpleEval ask
    withDictionary f (SimpleEval inner) = SimpleEval (local f inner)


eval :: forall m a. MonadEval m => Expr a -> m a
eval = \case
    Let _ (TypedVarName name) valExpr expr -> do
        val <- eval valExpr
        withVar name val $ eval expr
    Variable sline name -> fromSomeVarValue sline name =<< lookupVar name
    DynVariable _ _ name -> fail $ "ambiguous type of ‘" <> unpackVarName name <> "’"
    FunVariable _ sline name -> funFromSomeVarValue sline name =<< lookupVar name
    ArgsReq (FunctionArguments req) efun -> do
        dict <- askDictionary
        return $ FunctionType $ \(FunctionArguments args) ->
            let used = M.intersectionWith (\value ( vname, _ ) -> ( vname, value )) args req
                FunctionType fun = runSimpleEval (eval efun) (toList used ++ dict)
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
        dict <- askDictionary
        return $ \x -> runSimpleEval (eval expr) (( name, someConstValue x ) : dict)
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

evalSome :: MonadEval m => SomeExpr -> m SomeVarValue
evalSome (SomeExpr expr)
    | IsFunType <- asFunType expr = do
        FunctionType fun <- eval expr
        fmap SomeVarValue $ VarValue
            <$> gatherVars expr
            <*> pure (exprArgs expr)
            <*> pure (const fun)
    | otherwise = do
        fmap SomeVarValue $ VarValue
            <$> gatherVars expr
            <*> pure mempty
            <*> (const . const <$> eval expr)

data Traced a = Traced EvalTrace a

type VarNameSelectors = ( VarName, [ Text ] )
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

    gatherSelectors :: forall b. Expr b -> Maybe (VarName, [Text])
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

funFromSomeVarValue :: forall a m. (ExprType a, MonadFail m) => SourceLine -> VarName -> SomeVarValue -> m (FunctionType a)
funFromSomeVarValue sline name (SomeVarValue (VarValue _ args value :: VarValue b)) = do
    maybe (fail err) return $ do
        guard $ not $ anull args
        FunctionType <$> cast (value sline)
  where
    err = T.unpack $ T.concat [ T.pack "expected function returning ", textExprType @a Proxy, T.pack ", but variable '", textVarName name, T.pack "' has ",
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
