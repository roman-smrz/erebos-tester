module Test (
    Test(..),
    TestStep(..),
    SourceLine(..),

    MonadEval(..),
    VarName(..), TypedVarName(..), textVarName, unpackVarName,
    ExprType(..),
    SomeVarValue(..), fromSomeVarValue, textSomeVarValue,
    RecordSelector(..),
    Expr(..), eval, gatherVars,

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

data Test = Test
    { testName :: Text
    , testSteps :: [TestStep]
    }

data TestStep = forall a. ExprType a => Let SourceLine VarName (Expr a) [TestStep]
              | DeclNode (TypedVarName Node) (Expr Network) [TestStep]
              | Spawn (TypedVarName Process) (Either (TypedVarName Node) (Either (Expr Network) (Expr Node))) [TestStep]
              | Send (Expr Process) (Expr Text)
              | Expect SourceLine (Expr Process) (Expr Regex) [TypedVarName Text] [TestStep]
              | Guard SourceLine (Expr Bool)
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

instance ExprType Integer where
    textExprType _ = T.pack "integer"
    textExprValue x = T.pack (show x)
    emptyVarValue = 0

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

data SomeVarValue = forall a. ExprType a => SomeVarValue a

data RecordSelector a = forall b. ExprType b => RecordSelector (a -> b)

fromSomeVarValue :: forall a m. (ExprType a, MonadFail m) => VarName -> SomeVarValue -> m a
fromSomeVarValue name (SomeVarValue value) = maybe (fail err) return $ cast value
  where err = T.unpack $ T.concat [ T.pack "expected ", textExprType @a Proxy, T.pack ", but variable '", textVarName name, T.pack "' has type ", textExprType (Just value) ]

textSomeVarValue :: SomeVarValue -> Text
textSomeVarValue (SomeVarValue value) = textExprValue value


data Expr a where
    Variable :: ExprType a => VarName -> Expr a
    Literal :: ExprType a => a -> Expr a
    Concat :: [Expr Text] -> Expr Text
    Regex :: [Expr Regex] -> Expr Regex
    UnOp :: (b -> a) -> Expr b -> Expr a
    BinOp :: (b -> c -> a) -> Expr b -> Expr c -> Expr a
    RootNetwork :: Expr Network

eval :: MonadEval m => Expr a -> m a
eval (Variable name) = fromSomeVarValue name =<< lookupVar name
eval (Literal value) = return value
eval (Concat xs) = T.concat <$> mapM eval xs
eval (Regex xs) = mapM eval xs >>= \case
    [re@RegexCompiled {}] -> return re
    parts -> case regexCompile $ T.concat $ map regexSource parts of
        Left err -> fail err
        Right re -> return re
eval (UnOp f x) = f <$> eval x
eval (BinOp f x y) = f <$> eval x <*> eval y
eval (RootNetwork) = rootNetwork

gatherVars :: forall a m. MonadEval m => Expr a -> m [(VarName, SomeVarValue)]
gatherVars = fmap (uniqOn fst . sortOn fst) . helper
  where
    helper :: forall b. Expr b -> m [(VarName, SomeVarValue)]
    helper (Variable var) = (:[]) . (var,) <$> lookupVar var
    helper (Literal _) = return []
    helper (Concat es) = concat <$> mapM helper es
    helper (Regex es) = concat <$> mapM helper es
    helper (UnOp _ e) = helper e
    helper (BinOp _ e f) = (++) <$> helper e <*> helper f
    helper (RootNetwork) = return []


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
