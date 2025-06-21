module Test (
    Test(..),
    TestStep(..),
    TestBlock(..),

    MultiplyTimeout(..),
) where

import Control.Concurrent.MVar
import Control.Monad.Except
import Control.Monad.Reader

import Data.Scientific
import Data.Text (Text)
import Data.Typeable

import Network
import Output
import Process
import Run.Monad
import Script.Expr
import Script.Object
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
    CreateObject :: forall o. ObjectType TestRun o => Proxy o -> ConstructorArgs o -> TestStep ()
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


data MultiplyTimeout = MultiplyTimeout Scientific

instance ObjectType TestRun MultiplyTimeout where
    type ConstructorArgs MultiplyTimeout = Scientific

    createObject oid timeout
        | timeout > 0 = do
            var <- asks (teTimeout . fst)
            liftIO $ modifyMVar_ var $ return . (* timeout)
            return $ Object oid $ MultiplyTimeout timeout

        | otherwise = do
            outLine OutputError Nothing "timeout must be positive"
            throwError Failed

    destroyObject Object { objImpl = MultiplyTimeout timeout } = do
        var <- asks (teTimeout . fst)
        liftIO $ modifyMVar_ var $ return . (/ timeout)
