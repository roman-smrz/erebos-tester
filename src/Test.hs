module Test (
    Test(..),
    TestStep(..),

    ProcName(..), textProcName, unpackProcName,
    NodeName(..), textNodeName, unpackNodeName,

    MonadEval(..),
    VarName(..), textVarName, unpackVarName,
    Expr(..), eval,
    Regex,
) where

import Control.Monad

import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.TDFA
import Text.Regex.TDFA.Text

import Process

data Test = Test
    { testName :: Text
    , testSteps :: [TestStep]
    }

data TestStep = Spawn ProcName NodeName
              | Send ProcName (Expr Text)
              | Expect ProcName (Expr Regex) [VarName]
              | Wait

newtype NodeName = NodeName Text
    deriving (Eq, Ord)

textNodeName :: NodeName -> Text
textNodeName (NodeName name) = name

unpackNodeName :: NodeName -> String
unpackNodeName (NodeName tname) = T.unpack tname


class MonadFail m => MonadEval m where
  lookupStringVar :: VarName -> m Text


data VarName = VarName [Text]
    deriving (Eq, Ord)

textVarName :: VarName -> Text
textVarName (VarName name) = T.concat $ intersperse (T.singleton '.') name

unpackVarName :: VarName -> String
unpackVarName = T.unpack . textVarName


data Expr a where
    StringVar :: VarName -> Expr Text
    StringLit :: Text -> Expr Text
    Concat :: [Expr Text] -> Expr Text
    Regex :: [Expr Text] -> Expr Regex

eval :: MonadEval m => Expr a -> m a
eval (StringVar var) = lookupStringVar var
eval (StringLit str) = return str
eval (Concat xs) = T.concat <$> mapM eval xs
eval (Regex xs) = do
    parts <- forM xs $ \case
        StringLit str -> return str
        expr -> T.concatMap (\c -> T.pack ['\\', c]) <$> eval expr
    case compile defaultCompOpt defaultExecOpt $ T.concat $ concat [[T.singleton '^'], parts, [T.singleton '$']] of
        Left err -> fail err
        Right re -> return re
