module Test (
    Test(..),
    TestStep(..),
    SourceLine(..),

    ProcName(..), textProcName, unpackProcName,
    NodeName(..), textNodeName, unpackNodeName,

    MonadEval(..),
    VarName(..), textVarName, unpackVarName,
    ExprType(..),
    SomeVarValue(..), fromSomeVarValue, textSomeVarValue,
    Expr(..), eval, gatherVars,
    Regex,
) where

import Control.Monad

import Data.Char
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable

import Text.Regex.TDFA
import Text.Regex.TDFA.Text

import Process
import Util

data Test = Test
    { testName :: Text
    , testSteps :: [TestStep]
    }

data TestStep = forall a. ExprType a => Let SourceLine VarName (Expr a)
              | Spawn ProcName NodeName
              | Send ProcName (Expr Text)
              | Expect SourceLine ProcName (Expr Regex) [VarName]
              | Guard SourceLine (Expr Bool)
              | Wait

newtype SourceLine = SourceLine Text

newtype NodeName = NodeName Text
    deriving (Eq, Ord)

textNodeName :: NodeName -> Text
textNodeName (NodeName name) = name

unpackNodeName :: NodeName -> String
unpackNodeName (NodeName tname) = T.unpack tname


class MonadFail m => MonadEval m where
  lookupVar :: VarName -> m SomeVarValue


data VarName = VarName [Text]
    deriving (Eq, Ord)

textVarName :: VarName -> Text
textVarName (VarName name) = T.concat $ intersperse (T.singleton '.') name

unpackVarName :: VarName -> String
unpackVarName = T.unpack . textVarName


class Typeable a => ExprType a where
  textExprType :: proxy a -> Text
  textExprValue :: a -> Text
  emptyVarValue :: a

instance ExprType Integer where
  textExprType _ = T.pack "integer"
  textExprValue x = T.pack (show x)
  emptyVarValue = 0

instance ExprType Text where
  textExprType _ = T.pack "string"
  textExprValue x = T.pack (show x)
  emptyVarValue = T.empty

instance ExprType Bool where
  textExprType _ = T.pack "bool"
  textExprValue True = T.pack "true"
  textExprValue False = T.pack "false"
  emptyVarValue = False

data SomeVarValue = forall a. ExprType a => SomeVarValue a

fromSomeVarValue :: forall a m. (ExprType a, MonadFail m) => VarName -> SomeVarValue -> m a
fromSomeVarValue name (SomeVarValue value) = maybe (fail err) return $ cast value
  where err = T.unpack $ T.concat [ T.pack "expected ", textExprType @a Proxy, T.pack ", but variable '", textVarName name, T.pack "' has type ", textExprType (Just value) ]

textSomeVarValue :: SomeVarValue -> Text
textSomeVarValue (SomeVarValue value) = textExprValue value


data Expr a where
    Variable :: ExprType a => VarName -> Expr a
    Literal :: ExprType a => a -> Expr a
    Concat :: [Expr Text] -> Expr Text
    Regex :: [Expr Text] -> Expr Regex
    UnOp :: (b -> a) -> Expr b -> Expr a
    BinOp :: (b -> c -> a) -> Expr b -> Expr c -> Expr a

eval :: MonadEval m => Expr a -> m a
eval (Variable name) = fromSomeVarValue name =<< lookupVar name
eval (Literal value) = return value
eval (Concat xs) = T.concat <$> mapM eval xs
eval (Regex xs) = do
    parts <- forM xs $ \case
        Literal value | Just str <- cast value -> return str
                      | otherwise -> fail $ "regex expansion not defined for " ++ T.unpack (textExprType $ Just value)
        expr -> T.concatMap escapeChar <$> eval expr
          where
            escapeChar c | isAlphaNum c = T.singleton c
                         | c `elem` "`'<>" = T.singleton c
                         | otherwise = T.pack ['\\', c]
    case compile defaultCompOpt defaultExecOpt $ T.concat $ concat [[T.singleton '^'], parts, [T.singleton '$']] of
        Left err -> fail err
        Right re -> return re
eval (UnOp f x) = f <$> eval x
eval (BinOp f x y) = f <$> eval x <*> eval y

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
