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
    , testSteps :: Expr (TestStep ())
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
    Scope :: TestBlock a -> TestStep a
    Subnet :: TypedVarName Network -> Network -> (Network -> TestStep a) -> TestStep a
    DeclNode :: TypedVarName Node -> Network -> (Node -> TestStep a) -> TestStep a
    Spawn :: TypedVarName Process -> Either Network Node -> [ Text ] -> (Process -> TestStep a) -> TestStep a
    SpawnShell :: Maybe (TypedVarName Process) -> Node -> ShellScript -> (Process -> TestStep a) -> TestStep a
    Send :: Process -> Text -> TestStep ()
    Expect :: SourceLine -> Process -> Traced Regex -> [ TypedVarName Text ] -> ([ Text ] -> TestStep a) -> TestStep a
    Flush :: Process -> Maybe Regex -> TestStep ()
    Guard :: SourceLine -> EvalTrace -> Bool -> TestStep ()
    DisconnectNode :: Node -> TestStep a -> TestStep a
    DisconnectNodes :: Network -> TestStep a -> TestStep a
    DisconnectUpstream :: Network -> TestStep a -> TestStep a
    PacketLoss :: Scientific -> Node -> TestStep a -> TestStep a
    Wait :: TestStep ()

instance Typeable a => ExprType (TestBlock a) where
    textExprType _ = "test block"
    textExprValue _ = "<test block>"
