module Test (
    Module(..),
    Test(..),
    TestStep(..),
    TestBlock(..),
    SourceLine(..),

    MonadEval(..),
    VarName(..), TypedVarName(..), textVarName, unpackVarName,
    ExprType(..),
    SomeVarValue(..), fromSomeVarValue, textSomeVarValue,
    RecordSelector(..),
    ExprListUnpacker(..),
    ExprEnumerator(..),
    Expr(..), eval, gatherVars,
    AppAnnotation(..),

    Regex(RegexPart, RegexString), regexMatch,
) where

import Data.Char
import Data.List
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable

import Text.Regex.TDFA qualified as RE
import Text.Regex.TDFA.Text qualified as RE

import {-# SOURCE #-} Network
import {-# SOURCE #-} Process
import Util

data Module = Module
    { moduleName :: [ Text ]
    , moduleTests :: [ Test ]
    }

data Test = Test
    { testName :: Text
    , testSteps :: [TestStep]
    }

newtype TestBlock = TestBlock [ TestStep ]

data TestStep = forall a. ExprType a => Let SourceLine (TypedVarName a) (Expr a) [TestStep]
              | forall a. ExprType a => For SourceLine (TypedVarName a) (Expr [a]) [TestStep]
              | ExprStatement (Expr TestBlock)
              | Subnet (TypedVarName Network) (Expr Network) [TestStep]
              | DeclNode (TypedVarName Node) (Expr Network) [TestStep]
              | Spawn (TypedVarName Process) (Either (Expr Network) (Expr Node)) [TestStep]
              | Send (Expr Process) (Expr Text)
              | Expect SourceLine (Expr Process) (Expr Regex) [TypedVarName Text] [TestStep]
              | Flush (Expr Process) (Maybe (Expr Regex))
              | Guard SourceLine (Expr Bool)
              | DisconnectNode (Expr Node) [TestStep]
              | DisconnectNodes (Expr Network) [TestStep]
              | DisconnectUpstream (Expr Network) [TestStep]
              | PacketLoss (Expr Scientific) (Expr Node) [TestStep]
              | Wait

newtype SourceLine = SourceLine Text


class MonadFail m => MonadEval m where
  lookupVar :: VarName -> m SomeVarValue
  rootNetwork :: m Network


newtype VarName = VarName Text
    deriving (Eq, Ord)

newtype TypedVarName a = TypedVarName { fromTypedVarName :: VarName }
    deriving (Eq, Ord)

textVarName :: VarName -> Text
textVarName (VarName name ) = name

unpackVarName :: VarName -> String
unpackVarName = T.unpack . textVarName


class Typeable a => ExprType a where
    textExprType :: proxy a -> Text
    textExprValue :: a -> Text
    emptyVarValue :: a

    recordMembers :: [(Text, RecordSelector a)]
    recordMembers = []

    exprListUnpacker :: proxy a -> Maybe (ExprListUnpacker a)
    exprListUnpacker _ = Nothing

    exprEnumerator :: proxy a -> Maybe (ExprEnumerator a)
    exprEnumerator _ = Nothing

instance ExprType Integer where
    textExprType _ = T.pack "integer"
    textExprValue x = T.pack (show x)
    emptyVarValue = 0

    exprEnumerator _ = Just $ ExprEnumerator enumFromTo enumFromThenTo

instance ExprType Scientific where
    textExprType _ = T.pack "number"
    textExprValue x = T.pack (show x)
    emptyVarValue = 0

instance ExprType Bool where
    textExprType _ = T.pack "bool"
    textExprValue True = T.pack "true"
    textExprValue False = T.pack "false"
    emptyVarValue = False

instance ExprType Text where
    textExprType _ = T.pack "string"
    textExprValue x = T.pack (show x)
    emptyVarValue = T.empty

instance ExprType Regex where
    textExprType _ = T.pack "regex"
    textExprValue _ = T.pack "<regex>"
    emptyVarValue = either error id $ regexCompile T.empty

instance ExprType a => ExprType [a] where
    textExprType _ = "[" <> textExprType @a Proxy <> "]"
    textExprValue x = "[" <> T.intercalate ", " (map textExprValue x) <> "]"
    emptyVarValue = []

    exprListUnpacker _ = Just $ ExprListUnpacker id (const Proxy)

instance ExprType TestBlock where
    textExprType _ = "test block"
    textExprValue _ = "<test block>"
    emptyVarValue = TestBlock []

data SomeVarValue = forall a. ExprType a => SomeVarValue a

data RecordSelector a = forall b. ExprType b => RecordSelector (a -> b)

fromSomeVarValue :: forall a m. (ExprType a, MonadFail m) => VarName -> SomeVarValue -> m a
fromSomeVarValue name (SomeVarValue value) = maybe (fail err) return $ cast value
  where err = T.unpack $ T.concat [ T.pack "expected ", textExprType @a Proxy, T.pack ", but variable '", textVarName name, T.pack "' has type ", textExprType (Just value) ]

textSomeVarValue :: SomeVarValue -> Text
textSomeVarValue (SomeVarValue value) = textExprValue value

data ExprListUnpacker a = forall e. ExprType e => ExprListUnpacker (a -> [e]) (Proxy a -> Proxy e)

data ExprEnumerator a = ExprEnumerator (a -> a -> [a]) (a -> a -> a -> [a])


data Expr a where
    Variable :: ExprType a => VarName -> Expr a
    Pure :: a -> Expr a
    App :: AppAnnotation b -> Expr (a -> b) -> Expr a -> Expr b
    Concat :: [Expr Text] -> Expr Text
    Regex :: [Expr Regex] -> Expr Regex
    RootNetwork :: Expr Network

data AppAnnotation b = AnnNone
                     | ExprType b => AnnRecord Text

instance Functor Expr where
    fmap f x = Pure f <*> x

instance Applicative Expr where
    pure = Pure
    (<*>) = App AnnNone

eval :: MonadEval m => Expr a -> m a
eval (Variable name) = fromSomeVarValue name =<< lookupVar name
eval (Pure value) = return value
eval (App _ f x) = eval f <*> eval x
eval (Concat xs) = T.concat <$> mapM eval xs
eval (Regex xs) = mapM eval xs >>= \case
    [re@RegexCompiled {}] -> return re
    parts -> case regexCompile $ T.concat $ map regexSource parts of
        Left err -> fail err
        Right re -> return re
eval (RootNetwork) = rootNetwork

gatherVars :: forall a m. MonadEval m => Expr a -> m [((VarName, [Text]), SomeVarValue)]
gatherVars = fmap (uniqOn fst . sortOn fst) . helper
  where
    helper :: forall b. Expr b -> m [((VarName, [Text]), SomeVarValue)]
    helper (Variable var) = (:[]) . ((var, []),) <$> lookupVar var
    helper (Pure _) = return []
    helper e@(App (AnnRecord sel) _ x)
        | Just (var, sels) <- gatherSelectors x
        = do val <- SomeVarValue <$> eval e
             return [((var, sels ++ [sel]), val)]
        | otherwise = helper x
    helper (App _ f x) = (++) <$> helper f <*> helper x
    helper (Concat es) = concat <$> mapM helper es
    helper (Regex es) = concat <$> mapM helper es
    helper (RootNetwork) = return []

    gatherSelectors :: forall b. Expr b -> Maybe (VarName, [Text])
    gatherSelectors = \case
        Variable var -> Just (var, [])
        App (AnnRecord sel) _ x -> do
            (var, sels) <- gatherSelectors x
            return (var, sels ++ [sel])
        _ -> Nothing

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
