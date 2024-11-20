module Test (
    Module(..),
    Test(..),
    TestStep(..),
    TestBlock(..),
    SourceLine(..),

    MonadEval(..),
    VarName(..), TypedVarName(..), textVarName, unpackVarName,
    ExprType(..), SomeExpr(..),
    TypeVar(..), SomeExprType(..), someExprType, textSomeExprType,
    FunctionType, DynamicType,
    SomeVarValue(..), fromSomeVarValue, textSomeVarValue, someVarValueType,
    RecordSelector(..),
    ExprListUnpacker(..),
    ExprEnumerator(..),
    Expr(..), eval, evalSome,
    EvalTrace, VarNameSelectors, gatherVars,
    AppAnnotation(..),

    ArgumentKeyword(..), FunctionArguments(..),
    anull, exprArgs,
    SomeArgumentType(..), ArgumentType(..),

    Regex(RegexPart, RegexString), regexMatch,
) where

import Control.Monad

import Data.Char
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
import Util

data Module = Module
    { moduleName :: [ Text ]
    , moduleTests :: [ Test ]
    , moduleDefinitions :: [ ( VarName, SomeExpr ) ]
    }

data Test = Test
    { testName :: Text
    , testSteps :: Expr TestBlock
    }

newtype TestBlock = TestBlock [ TestStep ]
    deriving (Semigroup, Monoid)

data TestStep = forall a. ExprType a => Let SourceLine (TypedVarName a) (Expr a) (Expr TestBlock)
              | forall a. ExprType a => For SourceLine (TypedVarName a) (Expr [ a ]) (Expr TestBlock)
              | Subnet (TypedVarName Network) Network (Expr TestBlock)
              | DeclNode (TypedVarName Node) Network (Expr TestBlock)
              | Spawn (TypedVarName Process) (Either Network Node) (Expr TestBlock)
              | Send Process Text
              | Expect SourceLine Process (Expr Regex) [ TypedVarName Text ] (Expr TestBlock)
              | Flush Process (Maybe Regex)
              | Guard SourceLine EvalTrace Bool
              | DisconnectNode Node (Expr TestBlock)
              | DisconnectNodes Network (Expr TestBlock)
              | DisconnectUpstream Network (Expr TestBlock)
              | PacketLoss Scientific Node (Expr TestBlock)
              | Wait

newtype SourceLine = SourceLine Text


class MonadFail m => MonadEval m where
    lookupVar :: VarName -> m SomeVarValue
    rootNetwork :: m Network
    withVar :: ExprType e => VarName -> e -> m a -> m a


newtype VarName = VarName Text
    deriving (Eq, Ord, Show)

newtype TypedVarName a = TypedVarName { fromTypedVarName :: VarName }
    deriving (Eq, Ord)

textVarName :: VarName -> Text
textVarName (VarName name ) = name

unpackVarName :: VarName -> String
unpackVarName = T.unpack . textVarName


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
someExprType (SomeExpr (DynVariable tvar _ _)) = ExprTypeVar tvar
someExprType (SomeExpr fun@(FunVariable params _ _)) = ExprTypeFunction params (proxyOfFunctionType fun)
  where
    proxyOfFunctionType :: Expr (FunctionType a) -> Proxy a
    proxyOfFunctionType _ = Proxy
someExprType (SomeExpr (_ :: Expr a)) = ExprTypePrim (Proxy @a)

textSomeExprType :: SomeExprType -> Text
textSomeExprType (ExprTypePrim p) = textExprType p
textSomeExprType (ExprTypeVar (TypeVar name)) = name
textSomeExprType (ExprTypeFunction _ r) = "function:" <> textExprType r


data SomeVarValue = forall a. ExprType a => SomeVarValue
    { svvVariables :: EvalTrace
    , svvArguments :: FunctionArguments SomeArgumentType
    , svvFunction :: SourceLine -> FunctionArguments SomeVarValue -> a
    }

fromSomeVarValue :: forall a m. (ExprType a, MonadFail m) => SourceLine -> VarName -> SomeVarValue -> m a
fromSomeVarValue sline name (SomeVarValue _ args (value :: SourceLine -> args -> b)) = do
    maybe (fail err) return $ do
        guard $ anull args
        cast $ value sline mempty
  where
    err = T.unpack $ T.concat [ T.pack "expected ", textExprType @a Proxy, T.pack ", but variable '", textVarName name, T.pack "' has type ",
            if anull args then textExprType @b Proxy else "function type" ]

textSomeVarValue :: SourceLine -> SomeVarValue -> Text
textSomeVarValue sline (SomeVarValue _ args value)
    | anull args = textExprValue $ value sline mempty
    | otherwise  =  "<function>"

someVarValueType :: SomeVarValue -> SomeExprType
someVarValueType (SomeVarValue _ args (_ :: SourceLine -> args -> a))
    | anull args = ExprTypePrim (Proxy @a)
    | otherwise  = ExprTypeFunction args (Proxy @a)


data RecordSelector a = forall b. ExprType b => RecordSelector (a -> b)

data ExprListUnpacker a = forall e. ExprType e => ExprListUnpacker (a -> [e]) (Proxy a -> Proxy e)

data ExprEnumerator a = ExprEnumerator (a -> a -> [a]) (a -> a -> a -> [a])


data Expr a where
    Variable :: ExprType a => SourceLine -> VarName -> Expr a
    DynVariable :: TypeVar -> SourceLine -> VarName -> Expr DynamicType
    FunVariable :: ExprType a => FunctionArguments SomeArgumentType -> SourceLine -> VarName -> Expr (FunctionType a)
    ArgsApp :: FunctionArguments SomeExpr -> Expr (FunctionType a) -> Expr (FunctionType a)
    FunctionEval :: Expr (FunctionType a) -> Expr a
    Pure :: a -> Expr a
    App :: AppAnnotation b -> Expr (a -> b) -> Expr a -> Expr b
    Concat :: [Expr Text] -> Expr Text
    Regex :: [Expr Regex] -> Expr Regex
    RootNetwork :: Expr Network
    Undefined :: String -> Expr a

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

eval :: forall m a. MonadEval m => Expr a -> m a
eval = \case
    Variable sline name -> fromSomeVarValue sline name =<< lookupVar name
    DynVariable _ _ _ -> fail "ambiguous type"
    FunVariable _ sline name -> funFromSomeVarValue sline name =<< lookupVar name
    ArgsApp eargs efun -> do
        FunctionType fun <- eval efun
        args <- mapM evalSome eargs
        return $ FunctionType $ \args' -> fun (args <> args')
    FunctionEval efun -> do
        FunctionType fun <- eval efun
        return $ fun mempty
    Pure value -> return value
    App _ f x -> eval f <*> eval x
    Concat xs -> T.concat <$> mapM eval xs
    Regex xs -> mapM eval xs >>= \case
        [ re@RegexCompiled {} ] -> return re
        parts -> case regexCompile $ T.concat $ map regexSource parts of
            Left err -> fail err
            Right re -> return re
    RootNetwork -> rootNetwork
    Undefined err -> fail err

evalSome :: MonadEval m => SomeExpr -> m SomeVarValue
evalSome (SomeExpr expr) = SomeVarValue
    <$> gatherVars expr
    <*> pure mempty
    <*> (const . const <$> eval expr)

type VarNameSelectors = ( VarName, [ Text ] )
type EvalTrace = [ ( VarNameSelectors, SomeVarValue ) ]

gatherVars :: forall a m. MonadEval m => Expr a -> m EvalTrace
gatherVars = fmap (uniqOn fst . sortOn fst) . helper
  where
    helper :: forall b. Expr b -> m EvalTrace
    helper = \case
        Variable _ var -> (: []) . (( var, [] ), ) <$> lookupVar var
        DynVariable _ _ var -> (: []) . (( var, [] ), ) <$> lookupVar var
        FunVariable _ _ var -> (: []) . (( var, [] ), ) <$> lookupVar var
        ArgsApp (FunctionArguments args) fun -> do
            v <- helper fun
            vs <- mapM (\(SomeExpr e) -> helper e) $ M.elems args
            return $ concat (v : vs)
        FunctionEval efun -> helper efun
        Pure _ -> return []
        e@(App (AnnRecord sel) _ x)
            | Just (var, sels) <- gatherSelectors x
            -> do
                val <- SomeVarValue [] mempty . const . const <$> eval e
                return [ (( var, sels ++ [ sel ] ), val ) ]
            | otherwise -> do
                helper x
        App _ f x -> (++) <$> helper f <*> helper x
        Concat es -> concat <$> mapM helper es
        Regex es -> concat <$> mapM helper es
        RootNetwork -> return []
        Undefined {} -> return []

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
exprArgs (FunVariable args _ _) = args
exprArgs (ArgsApp (FunctionArguments applied) expr) =
    let FunctionArguments args = exprArgs expr
     in FunctionArguments (args `M.difference` applied)
exprArgs _ = error "exprArgs on unexpected type"

funFromSomeVarValue :: forall a m. (ExprType a, MonadFail m) => SourceLine -> VarName -> SomeVarValue -> m (FunctionType a)
funFromSomeVarValue sline name (SomeVarValue _ args (value :: SourceLine -> args -> b)) = do
    maybe (fail err) return $ do
        guard $ not $ anull args
        FunctionType <$> cast (value sline)
  where
    err = T.unpack $ T.concat [ T.pack "expected function returning ", textExprType @a Proxy, T.pack ", but variable '", textVarName name, T.pack "' has ",
            (if anull args then "type" else "function type returting ") <> textExprType @b Proxy ]

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
