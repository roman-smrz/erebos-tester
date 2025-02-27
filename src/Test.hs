module Test (
    Test(..),
    TestStep(..),
    TestBlock(..),
) where

import Data.Scientific
import Data.Text (Text)

import Network
import Process
import Script.Expr

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

instance ExprType TestBlock where
    textExprType _ = "test block"
    textExprValue _ = "<test block>"
