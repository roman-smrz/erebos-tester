module Test.Builtins (
    builtins,
) where

import Data.Map qualified as M
import Data.Text (Text)
import Data.Typeable

import Process (Process)
import Test

builtins :: [ ( VarName, SomeVarValue ) ]
builtins =
    [ ( VarName "send", builtinSend )
    , ( VarName "guard", builtinGuard )
    , ( VarName "wait", builtinWait )
    ]

getArg :: Typeable a => FunctionArguments SomeExpr -> Maybe ArgumentKeyword -> a
getArg (FunctionArguments args) kw =
    case M.lookup kw args of
        Just (SomeExpr expr) | Just expr' <- cast expr -> expr'
        _ -> error "parameter mismatch"

builtinSend :: SomeVarValue
builtinSend = SomeVarValue (FunctionArguments $ M.fromList atypes) $
    \_ args -> TestBlock [ Send (getArg args (Just "to")) (getArg args Nothing) ]
  where
    atypes =
        [ ( Just "to", SomeArgumentType (ContextDefault @Process) )
        , ( Nothing, SomeArgumentType (NoDefault @Text) )
        ]

builtinGuard :: SomeVarValue
builtinGuard = SomeVarValue (FunctionArguments $ M.singleton Nothing (SomeArgumentType (NoDefault @Bool))) $
    \sline args -> TestBlock [ Guard sline (getArg args Nothing) ]

builtinWait :: SomeVarValue
builtinWait = SomeVarValue mempty $ const . const $ TestBlock [ Wait ]
