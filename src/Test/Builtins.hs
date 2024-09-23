module Test.Builtins (
    builtins,
) where

import Data.Map qualified as M
import Data.Typeable

import Test

builtins :: [ ( VarName, SomeVarValue ) ]
builtins =
    [ ( VarName "guard", builtinGuard )
    , ( VarName "wait", builtinWait )
    ]

getArg :: Typeable a => FunctionArguments SomeExpr -> Maybe ArgumentKeyword -> a
getArg (FunctionArguments args) kw =
    case M.lookup kw args of
        Just (SomeExpr expr) | Just expr' <- cast expr -> expr'
        _ -> error "parameter mismatch"

builtinGuard :: SomeVarValue
builtinGuard = SomeVarValue (FunctionArguments $ M.singleton Nothing (ExprTypePrim (Proxy @Bool))) $
    \sline args -> TestBlock [ Guard sline (getArg args Nothing) ]

builtinWait :: SomeVarValue
builtinWait = SomeVarValue mempty $ const . const $ TestBlock [ Wait ]
