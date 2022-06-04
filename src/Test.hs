module Test (
    Test(..),
    TestStep(..),

    ProcName(..), textProcName, unpackProcName,
    NodeName(..), textNodeName, unpackNodeName,

    MonadEval(..),
    VarName(..), unpackVarName,
    StringExpr(..), evalStringExpr,
    RegexExpr(..), evalRegexExpr,
) where

import Control.Monad

import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.TDFA
import Text.Regex.TDFA.String

import Process

data Test = Test
    { testName :: Text
    , testSteps :: [TestStep]
    }

data TestStep = Spawn ProcName NodeName
              | Send ProcName StringExpr
              | Expect ProcName RegexExpr
              | Wait

newtype NodeName = NodeName Text
    deriving (Eq, Ord)

textNodeName :: NodeName -> Text
textNodeName (NodeName name) = name

unpackNodeName :: NodeName -> String
unpackNodeName (NodeName tname) = T.unpack tname


class Monad m => MonadEval m where
  lookupStringVar :: VarName -> m Text


data VarName = VarName [Text]
    deriving (Eq, Ord)

unpackVarName :: VarName -> String
unpackVarName (VarName name) = concat $ intersperse "." $ map T.unpack name

data StringExpr = StringExpr [Either Text VarName]

evalStringExpr :: MonadEval m => StringExpr -> m Text
evalStringExpr (StringExpr xs) = fmap T.concat $ forM xs $ \case
    Left text -> return text
    Right var -> lookupStringVar var

data RegexExpr = RegexExpr [Either String VarName]

evalRegexExpr :: (MonadFail m, MonadEval m) => RegexExpr -> m Regex
evalRegexExpr (RegexExpr xs) = do
    parts <- forM xs $ \case
        Left str -> return str
        Right var -> concatMap (\c -> ['\\',c]) . T.unpack <$> lookupStringVar var
    case compile defaultCompOpt defaultExecOpt $ concat $ concat [["^"], parts, ["$"]] of
        Left err -> fail err
        Right re -> return re
