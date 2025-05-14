module Test (
    Test(..),
    TestStep(..),
    TestBlock(..),
) where

import Data.Scientific
import Data.Text (Text)
import Data.Typeable

import Network
import Process
import Script.Expr
import Script.Shell

data Test = Test
    { testName :: Text
    , testSteps :: Expr (TestBlock ())
    }

data TestBlock a where
    EmptyTestBlock :: TestBlock ()
    TestBlockStep :: TestBlock () -> TestStep a -> TestBlock a

instance Semigroup (TestBlock ()) where
    EmptyTestBlock <> block = block
    block <> EmptyTestBlock = block
    block <> TestBlockStep block' step = TestBlockStep (block <> block') step

instance Monoid (TestBlock ()) where
    mempty = EmptyTestBlock

data TestStep a where
    Subnet :: TypedVarName Network -> Network -> (Network -> TestBlock a) -> TestStep a
    DeclNode :: TypedVarName Node -> Network -> (Node -> TestBlock a) -> TestStep a
    Spawn :: TypedVarName Process -> Either Network Node -> (Process -> TestBlock a) -> TestStep a
    SpawnShell :: Maybe (TypedVarName Process) -> Node -> ShellScript -> (Process -> TestBlock a) -> TestStep a
    Send :: Process -> Text -> TestStep ()
    Expect :: SourceLine -> Process -> Traced Regex -> [ TypedVarName Text ] -> ([ Text ] -> TestBlock a) -> TestStep a
    Flush :: Process -> Maybe Regex -> TestStep ()
    Guard :: SourceLine -> EvalTrace -> Bool -> TestStep ()
    DisconnectNode :: Node -> TestBlock a -> TestStep a
    DisconnectNodes :: Network -> TestBlock a -> TestStep a
    DisconnectUpstream :: Network -> TestBlock a -> TestStep a
    PacketLoss :: Scientific -> Node -> TestBlock a -> TestStep a
    Wait :: TestStep ()

instance Typeable a => ExprType (TestBlock a) where
    textExprType _ = "test block"
    textExprValue _ = "<test block>"
