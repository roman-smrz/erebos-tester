module Test (
    Test(..),
    TestStep(..),
    SourceLine(..),

    ProcName(..), textProcName, unpackProcName,
    NodeName(..), textNodeName, unpackNodeName,

    MonadEval(..),
    VarName(..), textVarName, unpackVarName,
    Expr(..), eval, gatherVars,
    Regex,
) where

import Control.Monad

import Data.Char
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.TDFA
import Text.Regex.TDFA.Text

import Process
import Util

data Test = Test
    { testName :: Text
    , testSteps :: [TestStep]
    }

data TestStep = Spawn ProcName NodeName
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
    BinOp :: (b -> c -> a) -> Expr b -> Expr c -> Expr a

eval :: MonadEval m => Expr a -> m a
eval (StringVar var) = lookupStringVar var
eval (StringLit str) = return str
eval (Concat xs) = T.concat <$> mapM eval xs
eval (Regex xs) = do
    parts <- forM xs $ \case
        StringLit str -> return str
        expr -> T.concatMap escapeChar <$> eval expr
          where
            escapeChar c | isAlphaNum c = T.singleton c
                         | c `elem` "`'<>" = T.singleton c
                         | otherwise = T.pack ['\\', c]
    case compile defaultCompOpt defaultExecOpt $ T.concat $ concat [[T.singleton '^'], parts, [T.singleton '$']] of
        Left err -> fail err
        Right re -> return re
eval (BinOp f x y) = f <$> eval x <*> eval y

gatherVars :: forall a m. MonadEval m => Expr a -> m [(VarName, Text)]
gatherVars = fmap (uniq . sort) . helper
  where
    helper :: forall b. Expr b -> m [(VarName, Text)]
    helper (StringVar var) = (:[]) . (var,) <$> lookupStringVar var
    helper (StringLit _) = return []
    helper (Concat es) = concat <$> mapM helper es
    helper (Regex es) = concat <$> mapM helper es
    helper (BinOp _ e f) = (++) <$> helper e <*> helper f
